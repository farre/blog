---
title: "Session History Diagrams in Firefox DevTools"
date: 2026-05-22
tags: [mozilla, session history, navigation, jake diagram]
---

Firefox Nightly now ships a new DevTools panel: Session History Diagrams. It sits under the Application tab, next to Service Workers and Manifest. If you've spent time debugging navigation bugs or building SPAs and wondered what the browser's session history actually looks like, this is for you.

<!-- more -->

## Jake diagrams

The HTML specification has a section ominously titled "[Welcome to the dragon's maw](https://html.spec.whatwg.org/#navigation-and-session-history)":

> Navigation, session history, and the traversal through that session history are some of the most complex parts of this standard.

To help reason about this, the spec uses [Jake diagrams](https://html.spec.whatwg.org/#jake-diagrams) — named after Jake Archibald. They're a tabular notation where columns represent steps in session history and rows represent navigables (the top-level browsing context plus any iframes). Background colors mark document boundaries; the current step is highlighted. They're a genuinely useful tool for capturing multi-navigable interactions that are otherwise hard to describe in prose.

The spec is candid about their limitation: "they only work with a single level of nesting." Real pages can nest iframes inside iframes, and session history has to track all of it.

![A Jake diagram showing a top-level navigable and two iframes across five history steps](/assets/images/2026-05-22-session-history-diagrams/jake-diagram.png)

## Firefox Session History Diagrams

The new DevTools panel extends Jake diagrams to handle arbitrary nesting. Every column is a step in the traversable's session history. Every row is a frame, listed in pre-order from the frame tree — top-level document first, then its first iframe, then that iframe's children, and so on. Colors still indicate document boundaries within a navigable. The diagram updates live as you navigate.

<div style="border-radius:12px; overflow:hidden; display:inline-block; max-width:100%"><img src="/assets/images/2026-05-22-session-history-diagrams/demo.gif" style="display:block; max-width:100%"></div>

In the screenshot, step 3 is the current position (highlighted in blue). You can see that step 2 — a Reddit page — had a nested frame that's gone by the time we navigate back. That kind of thing is usually invisible; here it's right in front of you.

## Who is this for?

Two audiences.

For web developers, especially those building single-page applications or working with the [Navigation API](https://developer.mozilla.org/en-US/docs/Web/API/Navigation_API), the diagram makes the browser's session history state legible. A push where you expected a replace, a missing history entry, an iframe that accumulated entries unexpectedly — these are exactly the kinds of bugs that are hard to reason about without seeing the state directly.

For browser engineers, it's even more useful. Navigation and session history are notoriously hard to work on. Being able to watch the diagram change while reproducing a bug turns opaque internal state into something you can point at.

## Enabling it

The feature is in Firefox Nightly and will ship as Firefox 153. Enable it with the pref `devtools.application.sessionHistory.enabled` in `about:config`, then reload DevTools and navigate to Application → Session History.

## Thanks

A big thanks to [Nicolas Chevobbe](https://github.com/nchevobbe) [:nchevobbe], who was heavily involved in getting the DevTools integration right. The work is tracked in [Bug 2015726](https://bugzilla.mozilla.org/show_bug.cgi?id=2015726).

{% include comments.html %}
