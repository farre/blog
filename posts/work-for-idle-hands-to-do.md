---
title: Work for idle hands to do
date: 2017-06-21
author: Andreas Farre
tags: mozilla, requestIdleCallback, idleDispatch, DOM quantum, timers
commentId: 2
branch: posts/work-for-idle-hands-to-do/1
---

With Firefox 55 we saw the release of `Window.requestIdleCallback(callback)`{.js}, which makes it possible for a page to request that a script callback should be called as soon as the user agent is idle. This is something that Potch has written about[^1], but along with `requestIdleCallback`{.js} there is also an underlying framework that enables the same or similar for both the UI as well as Gecko.

<!--more-->

### What is idle?

Before we begin we need to define what it means to be idle, especially in the context of requestIdleCallback. Basically there are three levels of event queues, with decreasing priority. The first is where we handle vsync events, the second where we handle regular events, and the third is the idle queue. Scheduling the execution of events from the first two queues needs to actually needs to do some interleaving to not starve event handling, but as far as handling events from the idle queue the rule is that if the first two queues are empty, then events from the event queue will be executed. So in this sense, idle means as soon as there are no other events pending than those on the idle queue[^2].

There is a flaw with this definition of idleness, but we'll leave that be for a while. This definition of idleness gives us enough information to be able to schedule events that we want to run when there are no pending vsync events or any regular events.

We can now do:

``` c++
nsCOMPtr<nsIRunnable> event = ...;
NS_IdleDispatchToCurrentThread(event.forget());
```

This will schedule the runnable `event` to the current threads idle queue, and this will be executed the next time we process an event and find that there is only events on the idle queue.

### No guarantees

As we touched upon above, there are no guarantees that the queues with higher priority will be empty, which means that events posted to the idle aren't guaranteed to run. Because of this we have we need a way to say that we want to schedule an event to run when the user agent is idle, but if we haven't been idle for some period of time then run the event anyway.

This we can do like so:

``` c++
nsCOMPtr<nsIRunnable> event = ...;
uint32_t timeout = 100;
NS_IdleDispatchToCurrentThread(event.forget(), 100);
```

Here we supply the call to `NS_IdleDispatchToCurrentThread`{.cpp} with a timeout in milliseconds. If the event hasn't run before `timeout` milliseconds has passed, it will run anyway. This is accomplished by wrapping the event and posting the wrapper to the idle queue and setting up a timer that when it fires runs the wrapped event. The wrapping event also makes sure that the wrapped event isn't called multiple times.

### The flaw

Our definition of idleness has, as we've already hinted, a weakness. It only says something about if we're idle right *now*. If we want to be able to schedule events so that they run while we're idle, we need some notion of how *long* we will be idle. Because of this we define the idea of and idle period[^3] to help us and the thread processing events to determine when an idle period will end.

``` c++
interface nsIIdlePeriod : nsISupports
{
    /**
     * Return an estimate of a point in time in the future when we
     * think that the associated thread will become busy. Should
     * return TimeStamp() (i.e. the null time) or a time less than
     * TimeStamp::Now() if the thread is currently busy or will become
     * busy very soon.
     */
    TimeStamp getIdlePeriodHint();
};
```

For the main thread of Gecko we define idle periods to be[^4]:

* The time between refresh driver ticks, i.e. we are in some kind of animation
* The time until a timer will fire
* 50ms, if there are no timers or they're scheduled to fire in more than 50ms, and if we aren't animating, i.e. we haven't ticked the refresh driver for a while.

With this the thread can make more informed decisions of if it is a good idea to run an event from the idle queue or not. If the current idle period will end soon, then we will most surely execute an event that originates from either the vsync queue or the normal queue. This is enough if the events that we wish to schedule from the idle queue are (very) short. The reason for this is that although it is possible that the current idle period will last for up to 50ms, there is no way for the event to know this. And for that situation we need something more.

Putting these things together we're able to define an interface for events that are aware of how long they're allowed to execute.

``` c++
class nsIIdleRunnable : public nsISupports
{
public:
  virtual void SetDeadline(TimeStamp aDeadline) {}
  virtual void SetTimer(uint32_t aTimeout, nsIEventTarget* aTarget) {}
};
```

If we do

``` c++
nsCOMPtr<nsIIdleRunnable> event = ...;
NS_IdleDispatchToCurrentThread(event.forget());
```

where `event`{.cpp} is a class that inherits from `nsIIdleRunnable`{.cpp}, the thread will make sure to call `SetDeadline`{.cpp} before running the event, where the argument `aDeadline`{.cpp} is the point in time when the current idle period will end. Using this the event can know how long it has to execute, and handle that accordingly. It is expected that events respect this deadline, and splits up or avoids doing work if it can't manage to finish in time. This way it is possible to cooperatively schedule work when the user agent is idle.

If you use the `nsIIdleRunnable`{.cpp} interface it is expected that if you wish to call `NS_IdleDispatchToCurrentThread`{.cpp} with a timeout then you need to implement `SetTimer`{.cpp}. This is because we wish to limit the amount of wrappers when dispatching events by having idle runnables manage their own timers. The call to `SetTimer`{.cpp} will happen when the event is dispatched using `NS_IdleDispatchToCurrentThread`{.cpp}.

### Now you're just being fancy

Defining these classes that implement and expose the interface for idle events needs a fair bit of boilerplate. To help alleviate this we define some utility classes and APIs.

To begin with we have the `IdleRunnable`{.cpp}[^5] class. This class is intended to be sub-classed, and you need to at least override the `Run`{.cpp} method, but there is also the `SetDeadline`{.cpp} mentioned above as well as SetTimer, but at least you get nsISupports for free. You should also make sure to override the `GetName` method from the `nsINamed` interface so that the event shows up with its name in telemetry[^6].

This is still a fair bit of boilerplate which is why we expose the final utility API. In the same way that we can create runnables using `NewRunnableMethod`{.cpp}[^7] we have `NewIdleRunnableMethod`{.cpp}[^7]. This means that if you have a refcounted class it is possible to create a runnable from a class method.

``` c++
class IdleClass final
{
public:
  NS_INLINE_DECL_REFCOUNTING(IdleClass)
  void IdleMethod() {}
  void SetDeadline(TimeStamp aTimeStamp) {}
private:
  ~IdleClass() {}
};

RefPtr<IdleClass> idleObject = new IdleClass();
nsCOMPtr<nsIRunnable> runnable = NewIdleRunnableMethod("ExampleIdleClass", idleObject, &IdleClass::IdleMethod);
NS_IdleDispatchToCurrentThread(runnable);
```

The first argument of `NewIdleRunnableMethod`{.cpp} is the name that is returned using the `nsINamed`{.cpp} interface. If you want to use a timeout, again we must do it differently using `NewIdleRunnableMethodWithTimer`{.cpp}. The benefit here is that we get the wrapper-less timer for free.

If the method is expected to finish quickly and doesn't need to handle the end of the current idle period it is fine to skip defining `SetDeadline`{.cpp} for the receiving class, but if you do it will be called when the corresponding method for an `nsIIdleRunnable`{.cpp} would be called.

### Script

For the UI the story is cleaner. There is:

``` javascript
var fn = function() {};
Services.tm.idleDispatchToMainThread(fn);
```

and

``` javascript
var fn = function() {};
var timeout = 100;
Services.tm.idleDispatchToMainThread(fn, 100);
```

Both behaves as expected.

[^1]: <https://hacks.mozilla.org/2016/11/cooperative-scheduling-with-requestidlecallback/>
[^2]: See <https://searchfox.org/mozilla-central/source/xpcom/threads/nsThread.h>
[^3]: See <https://searchfox.org/mozilla-central/source/xpcom/threads/nsIIdlePeriod.idl>
[^4]: See <https://searchfox.org/mozilla-central/source/xpcom/threads/MainThreadIdlePeriod.h> This is currently the only idle period defined. If you wish to use this feature for other threads than the main thread, an nsIIdlePeriod needs to be defined and registered with the thread using `nsIThread.registerIdlePeriod`{.js}
[^5]: https://searchfox.org/mozilla-central/source/xpcom/threads/nsThreadUtils.h
[^6]: See for example <https://telemetry.mozilla.org/new-pipeline/evo.html#!aggregates=median&cumulative=0&end_date=null&keys=AsyncFreeSnowWhite&max_channel_version=nightly%252F56&measure=IDLE_RUNNABLE_BUDGET_OVERUSE_MS&min_channel_version=nightly%252F53&processType=*&product=Firefox&sanitize=1&sort_keys=submissions&start_date=null&trim=1&use_submission_date=0>. The probe's name is IDLE_RUNNABLE_BUDGET_VERUSE_MS, and the name of the event shows up as a key in this histogram.
[^7]: See <https://searchfox.org/mozilla-central/source/xpcom/threads/nsThreadUtils.h>
