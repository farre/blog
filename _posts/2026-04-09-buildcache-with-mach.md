---
title: "BuildCache now works with mach"
date: 2026-04-09
tags: [mozilla, mach, buildcache]
---

I'm happy to announce that [buildcache](https://gitlab.com/bits-n-bites/buildcache) is now a first-class compiler cache in `mach`. This has been a long time coming, and I'm excited to finally see it land.

<!-- more -->

For those unfamiliar, [buildcache](https://gitlab.com/bits-n-bites/buildcache) is a compiler cache that can drastically cut down your rebuild times by caching compilation results. It's similar to ccache, but even more so sccache, in that it supports C/C++ out of the box, as well as Rust. It has some nice unique properties of its own though, which we'll look at more closely in following posts.

## Getting started

Setting it up is straightforward. Just add the following to your mozconfig:

{% highlight bash %}
ac_add_options --with-ccache=buildcache
{% endhighlight %}

Then build as usual:

{% highlight bash %}
./mach build
{% endhighlight %}

That's it.

## Give it a try

If you run into any issues, please file a bug and tag me. I'd love to hear how it works out for people, and any rough edges you might hit.

{% include comments.html %}
