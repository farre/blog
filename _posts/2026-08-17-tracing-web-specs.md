---
title: "Tracing algorithms through web specifications"
date: 2026-08-17
tags: [mozilla, web specs, tooling]
---

A bug comes in, or someone shows me a page doing something surprising, and the question is always the same one. Is that what the spec says? Not what we all assume it says, and not what Gecko happens to do, but what the algorithm actually does when you follow it step by step. So I open [html.spec.whatwg.org](https://html.spec.whatwg.org/), find the entry point, and start reading. Twenty tabs later I have usually lost the thread.

<!-- more -->

This isn't an occasional chore. I do it when I'm implementing something new, when I'm working out whether a Gecko bug is a bug in Gecko or a bug in the spec, when I'm reviewing a spec change someone else has proposed, and when I'm trying to design something that has to fit alongside everything that already exists. Four fairly different jobs, and every one of them comes down to the same mechanical work. Start at an algorithm, follow the calls, keep track of where you've been.

The specs are built for exactly this, in principle. Every step that invokes another algorithm is a hyperlink, and the whole thing is one large call graph spread across HTML, DOM, Fetch, URL and Infra. The trouble is that walking that graph by hand is slow, and it's the kind of slow a machine should be doing on your behalf.

Two tools have changed how I do this. One answers questions about the graph, and the other helps me walk it. If you'd rather install them first and read afterwards, [they're both at the end](#getting-them).

## webspec-index trace

[webspec-index](https://github.com/jnjaeschke/webspec-index) is [Jan Jaeschke](https://github.com/jnjaeschke/)'s tool for querying web specifications from the command line. It indexes the specs, on demand as it needs them, and lets you ask about definitions, references and IDL. In August it gained a `trace` subcommand, and that's the one I keep reaching for.

You give it two anchors and it finds the routes between them. It answers in JSON by default, which is the right choice for feeding it into something else but not for reading, so add `--format markdown` when the audience is you. Say I want to know how a navigation can end up firing `navigateerror`:

{% highlight console %}
$ webspec-index trace HTML#navigate HTML#event-navigateerror \
    --format markdown
{% endhighlight %}

{% highlight text %}
# trace: `HTML#navigate` -> `HTML#event-navigateerror`

Max depth 6, kind `step`. Found 11 trace(s).

## Trace 1 (5 hop(s))

1) [`HTML#navigate` step 20](…#beginning-navigation:set-the-ongoing-navigation)
   calls `HTML#set-the-ongoing-navigation`
   > Set the ongoing navigation for navigable to navigationId.

2) [`HTML#set-the-ongoing-navigation` step 2](…#aborting-navigation:inform-the-…)
   calls `HTML#inform-the-navigation-api-about-aborting-navigation`
   > Inform the navigation API about aborting navigation given navigable.

3) [`HTML#inform-the-navigation-api-…` step 3.1](…#ongoing-navigation-tracking:…)
   calls `HTML#abort-the-ongoing-navigation`
   - under: While navigation's ongoing navigate event is not null:
   > Abort the ongoing navigation given navigation.

4) [`HTML#abort-the-ongoing-navigation` step 7](…#ongoing-navigation-tracking:…)
   calls `HTML#abort-a-navigateevent`
   > Abort event given error.

5) `HTML#abort-a-navigateevent` step 6 calls `HTML#event-navigateerror`
   > Fire an event named navigateerror at navigation using ErrorEvent, with
     additional attributes initialized according to errorInfo.
{% endhighlight %}

I've wrapped those lines and shortened the URLs to fit; in the real output each hop is one long line and every link is a full deep link to that exact call site.

Every hop names the exact step number, quotes the step text, and tells you the condition it sits under. That last part matters more than it looks. Step 3.1 only runs while the ongoing navigate event is non-null, and knowing that is the difference between a path that exists on paper and a path your case actually took.

Eleven routes, though[^1]. Reading eleven of these back to back is not much better than the twenty tabs.

## Three zoom levels

This is where `webspec-index trace` gets genuinely nice, and it's the part I want to draw attention to, because it's easy to miss in the help text. The `--detail` flag changes how much of each route you get, and the three levels turn out to be three different ways of working rather than just three verbosity settings.

Ask the same question again with `--detail edges` and the prose falls away, leaving the skeleton:

{% highlight console %}
$ webspec-index trace HTML#navigate HTML#event-navigateerror \
    --detail edges --format markdown
{% endhighlight %}

{% highlight text %}
# trace: `HTML#navigate` -> `HTML#event-navigateerror`

Max depth 6, kind `step`. Found 11 trace(s).

## Trace 1 (5 hop(s))

1) `HTML#navigate` step 20 calls `HTML#set-the-ongoing-navigation`

2) `HTML#set-the-ongoing-navigation` step 2 calls `HTML#inform-the-navigation-api-about-aborting-navigation`

3) `HTML#inform-the-navigation-api-about-aborting-navigation` step 3.1 calls `HTML#abort-the-ongoing-navigation`

4) `HTML#abort-the-ongoing-navigation` step 7 calls `HTML#abort-a-navigateevent`

5) `HTML#abort-a-navigateevent` step 6 calls `HTML#event-navigateerror`

## Trace 2 (5 hop(s))

1) `HTML#navigate` step 22.4 calls `HTML#fire-a-push/replace/reload-navigate-event`

2) `HTML#fire-a-push/replace/reload-navigate-event` step 2 calls `HTML#inform-the-navigation-api-about-aborting-navigation`

...
{% endhighlight %}

Now you can read all eleven at once, and a shape appears that I could not see in the verbose output at all. There are five places inside `navigate` where a route starts:

| step | calls |
| :--- | :--- |
| 15.1 | `#navigate-fragid` |
| 20 | `#set-the-ongoing-navigation` |
| 21.1 | `#navigate-to-a-javascript:-url` |
| 22.4 | `#fire-a-push/replace/reload-navigate-event` |
| 24.1 | `#checking-if-unloading-is-canceled` |

And all eleven finish on the same line:

{% highlight text %}
`HTML#abort-a-navigateevent` step 6 calls `HTML#event-navigateerror`
{% endhighlight %}

There are exactly two ways into that step, from `HTML#abort-the-ongoing-navigation` step 7 and from `HTML#process-navigate-event-handler-failure` step 5. Eleven routes exist because three of the five entry points re-enter the same navigate event firing machinery from different directions, not because there are eleven genuinely different things going on.

That's a useful thing to know before you start debugging. If a navigation is firing `navigateerror` and you want to find out why, you don't instrument eleven places. You instrument the one step they all pass through and work backwards from there.

So the way I use it now is to start at `--detail edges` to find the shape, then drop back to the default once I know which route I care about and want to read the reasoning. The third level, `--detail compact`, drops the prose but keeps every anchor as a deep link to the exact call site, which is the form you want when you're pasting a route into a bug report for someone else to follow.

## WebSpec Tracer

`webspec-index trace` answers a specific kind of question, and it's worth being precise about which kind. You have to know both ends. That's fine when you're asking whether one thing can reach another, but a lot of the time I only know where I'm standing. I have a starting point, a bug in front of me, and no idea yet where the algorithm goes.

There's also something `webspec-index trace` can't do for you in principle. It finds routes through the spec text, statically. It doesn't know which branch your case took, because that depends on what actually happened at runtime, and only you have that information.

So I wrote [WebSpec Tracer](https://github.com/farre/webspec-tracer), a Firefox extension that walks the specs with you rather than ahead of you. You give it a starting anchor and it shows you that algorithm with every linked term highlighted. Click one and it becomes the next hop in the trace, and the panel moves you into it. Click the wrong one and Back undoes it.

![The WebSpec Tracer sidebar showing the navigate algorithm, with the linked terms in each step highlighted, next to a GitHub issue form containing the trace so far as markdown](/assets/images/2026-08-17-tracing-web-specs/branch-point.png)

The screenshot is a walk that started at `location.assign` and has arrived inside `navigate`. Step 15 is the interesting one. It runs only if four things are all true, that there's no document resource, no response, the URL matches the current entry's URL ignoring fragments, and the fragment is non-null. If all four hold, it navigates to a fragment and returns. Otherwise the algorithm keeps going into the full navigation path below.

No tool can decide that for me. Whether my bug went left or right at step 15 depends on the page and on what the user did. But the extension puts the decision in front of me with the alternatives visible, which is precisely what I was failing to do for myself with twenty tabs open.

The trace builds up in the panel underneath as markdown, with every anchor pointing at the exact call site. That box is a display rather than a text field, so there are buttons for getting the trace out of it. Copy does what you'd expect. Insert is the one I actually use, and it's the reason the extension lives in the sidebar: it writes the markdown straight into whichever field you have focused on the page, so you can have a spec issue open next to the panel and drop the trace into it without going near the clipboard. Reset clears everything and starts over.

![The same panel, with the GitHub issue switched to the Preview tab, showing the two hops rendered as linked spec anchors](/assets/images/2026-08-17-tracing-web-specs/branch-preview.png)

Switch the issue over to Preview and there are the same two hops, rendered, each one a link into the spec. That's most of the reason I built the thing. Explaining a route through an algorithm in prose is miserable, and the version you retype by hand is always slightly wrong. This way what I hand over is exactly what I walked.

One more thing worth knowing, about the anchors themselves. The starting one has to say which spec it's in, so `HTML#dom-location-assign` rather than just `dom-location-assign`, but after that the extension works it out as it goes. Anything that turns up in the trace without a prefix is in the spec you started from, and a prefix appears when the trace crosses a boundary:

![The panel showing WebIDL's wait for all algorithm, with a six hop trace whose last line ends in WEBIDL#wait-for-all](/assets/images/2026-08-17-tracing-web-specs/cross-spec.png)

That's the same walk carried a few hops further. Hop 3 is that step 15 decision, taken, and the last line is `WEBIDL#wait-for-all`. Somewhere in the navigate event firing the trace left HTML entirely, and the prefix is the only thing that tells me so. The panel on the left has stopped showing me HTML too, and is now sitting in WebIDL. That's easy to lose track of when you're clicking through by hand, and it's usually the moment a question stops being about HTML and starts being about something else.

## When one call happens twelve times

Sometimes an algorithm invokes another one from many different places, and a trace has to say something sensible about that. This was in the design from the start, and the answer it arrived at is to do what the spec already does. Start at `HTML#update-the-image-data` and follow it to `HTML#abort-the-image-request`:

![The panel showing the abort the image request algorithm, with a single trace line listing twelve numbered call sites](/assets/images/2026-08-17-tracing-web-specs/multi-branch.png)

`HTML#abort-the-image-request` is invoked from twelve separate places inside `HTML#update-the-image-data`. The extension doesn't make me choose one, and it doesn't flatten them into a single link either. It records the hop once and hangs all twelve call sites off it as numbered links, so the line reads `#update-the-image-data calls #abort-the-image-request, [2], [3]` and onwards to `[12]`, each number pointing at its own anchor.

The numbering isn't invented. It's the spec's own scheme for repeated references: the anchors run `#abort-the-image-request`, then `-2`, `-3`, and so on up to `-12`. All the trace does is carry that through, so a hop in the output identifies call sites the same way the spec identifies them, and the numbers mean the same thing in both places.

That also keeps the trace honest. The hop happened, and that's true whichever of the twelve fired, but the fact that there are twelve is worth knowing on its own, and anyone reading can go and check any one of them. Writing this out by hand you would link the first and lose the other eleven without ever noticing. The same thing is visible in the previous screenshot, where hop 3 reads `#navigate calls #navigate-fragid, [2], [3]`.

`webspec-index trace` deals with the same situation the other way round. Ask it the same question and you get twelve separate one hop traces, each naming its own step number, step 2, then 7.4.2, then 11.1, and so on down the algorithm. Neither is wrong. The command line is telling you which steps, and the extension is keeping your trace to one line while preserving every place it could have come from.

## Which one, when

They divide up cleanly enough that I don't think about it much any more. If I know both ends and want to know what's between them, or how many different routes there are, that's `webspec-index trace`. If I know where I'm starting and want to find out where it goes, that's the extension.

The one I underused at first was `--detail edges`. Asking a broad question and then zooming out far enough to see the answer's shape is a different move from following a single path carefully, and it's the one that has surprised me most often, usually by showing me that several things I thought were separate all funnel through the same step.

## Getting them

webspec-index is on crates.io:

{% highlight bash %}
cargo binstall webspec-index
{% endhighlight %}

It builds its index as it goes, so the first trace into a corner you haven't visited before pauses briefly to fetch and index the specs the route touches. After that it's fast.

WebSpec Tracer is on [addons.mozilla.org](https://addons.mozilla.org/en-US/firefox/addon/webspec-tracer/), and the source is on [GitHub](https://github.com/farre/webspec-tracer). It lives in the sidebar, toggled with `Ctrl+Shift+U`, so you can keep it open next to whatever you're reading.

Both are worth a try if you spend any time at all reading specs, whether you're implementing them, arguing with them, or just trying to find out whether the browser is wrong or you are.

#### Notes

{:footnotes}

1. Eleven is what you get at the default maximum depth of six hops. Raise it and there are more.

[^1]: Eleven is what you get at the default maximum depth of six hops. Raise it and there are more.

{% include comments.html %}
