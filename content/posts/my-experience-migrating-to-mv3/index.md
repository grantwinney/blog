---
categories:
- Browser Extension
- Google
- MV3
date: "2022-11-19T22:09:03Z"
description: ""
draft: false
cover:
  image: photo-1534198601173-4de3342a0b62.jpg
slug: my-experience-migrating-to-mv3
summary: I migrated my addons to MV3, and learned that version numbers increase, DRY
  is overrated, and 3 and 15 are probably important but I have no idea why. What I
  didn't learn is how MV3 made my addon better.
tags:
- Browser Extension
- Google
- MV3
title: My experience migrating to MV3
---


Over the summer I wrote about Google's forced manifest v3 update, and was up in the air about whether to bother figuring it out. Well, with only about 6 weeks left (actually, things got pushed out even more since the original article), I decided to give it another go, mostly because a good number of people have found Hide Comments Everywhere to be helpful.

To briefly recap, Google is spearheading a change to the way browser extensions are written, claiming it increases security and privacy. However, it'll be deterimental to the way privacy extensions like Ghostery and uBlock Origin work, and the Electronic Frontier Foundation is calling it harmful. Since nearly all the major browsers are built on top of Chromium however, we're all along for the ride.

What it means is that every developer who writes an addon now needs to spend time figuring out how MV3 works, what changes are required, and make those changes before June 2023 when Google unlists MV2 addons from their store. Don't be shocked if some of your favorite browser addons simply vanish next summer.

Firefox, which isn't built on Chromium, is attempting to support both and will begin accepting MV3 addons in just a couple days, although they aren't requiring anyone to adopt the change. The question is, how manageable will that be in the long run?

The first thing I tried was just bumping up the version number. Maybe I'd get lucky? Nope. I was greeted by a looong line of errors.

One by one, I moved down the list. Browser_action was renamed to action and browser_specific_settings is a Firefox thing which might be required in Firefox for MV3. Requesting host URLs (like I do, to periodically grab updated css selectors to block on sites) got moved into their own section called host permissions. All pretty straight-forward so far, and documented pretty well in a number of places. Here's a few I found along the way:

 * Migrating to Manifest V3 - Chrome Developers
 * Chrome Extension Manifest V3: A Migration Guide - VSH Solutions
 * Begin your MV3 migration by implementing new features today | Mozilla

That last error gave me pause. What does "service work registration failed" mean? Service workers replaced background pages, which isn't cryptic, but the error itself is so vague. Why did it fail? Well, reason 15 of course! Including the error number in there required a bug fix, apparently, and oh boy what a good time that would've been to toss some more descriptive text in there. I never did find a source for what the numbers mean, but while I was flailing around for a fix, they kept switching between 3 and 15.

The problem ended up being that my background script required other scripts, which I instructed the manifest file to load in tandem. That's not allowed with service workers.

"background": {
    "scripts": [
        "js/third-party/jquery-3.6.0.min.js",
        "js/third-party/axios.min.js",
        "js/third-party/toastr.min.js",
        "js/shared.js",
        "js/background.js"
    ],
    "persistent": false
},

You get to specify one file, which you can mark as a module in order to import code from other modules that export their functions. Or something. I'm sure a web developer could explain all this, but I'm not one, nor do I feel like learning about this weekend. I gave it the ol' college try, it didn't go well, and I ended up copying whatever I needed from other files into the "service worker" file itself. For the more intrepid, maybe this'll help. I decided DRY code wasn't worth my sanity.

One of the benefits of having to go through everything with a fine-tooth comb, though, was that I streamlined my background page / service worker. I realized I didn't need toastr, so I dropped that reference. jQuery is rarely a necessity, and wasn't in my service worker, so I dropped that reference too. I dropped Axios from the addon altogether, in favor of fetch. Here's a nice little comparison that I found informative: Axios vs. fetch(): Which is best for making HTTP requests?

This code seems to work just fine with MV3, where I inject the stylesheet into the page, which had me scratching my head. I guess it's okay because it's in the content script, running within the context of the current page?

var header = document.querySelector('head');
if (header) {
    header.appendChild(style);
} else {
    document.documentElement.prepend(style);
}

The reason for my concern is that one of the differences between background pages and service workers is that the latter doesn't have access to the DOM. I interpreted that as not having access to the DOM at all, but it seems I was mistaken. I hope.

In MV2, anyone could get out there and write an addon, and I feel like the complexities here might stymie a lot of people with neat ideas. Time will tell.

I already linked to some of these articles throughout this post, but here's a bunch of sites I found useful while trying to migrate. Oh, and my migration is complete, uploaded, and was accepted the next day.

 * Chrome Extension Manifest V3: A Migration Guide - VSH Solutions
 * Service worker overview - Chrome Developers
 * JavaScript modules - JavaScript | MDN
 * Using the Fetch API - Web APIs | MDN
 * Axios vs. fetch(): Which is best for making HTTP requests? - LogRocket

As for the Firefox store, they don't begin accepting MV3 extensions until next week, so I guess that's a story for another time. Here's some more links, from Mozilla:

 * Mozilla’s Manifest v3 FAQ | Mozilla Add-ons Community
 * Manifest v3 update | Mozilla Add-ons Community
 * Manifest v3 in Firefox: Recap & Next Steps | Mozilla Add-ons Community
 * Manifest V3 Firefox Developer Preview — how to get involved | Mozilla
