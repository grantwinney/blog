---
title:
slug: comparing-comment-blockers
summary:
description:
date:
draft: true
postimage:
postimagealt:
postimagecaption:
categories:
  - 
tags:
  -
---
I was about to write how it's been a few years since I created [Hide Comments Everywhere](https://grantwinney.com/hide-comments-everywhere/), a browser addon for, um, hiding comments. Everywhere. But then I looked back at the repo history and realized it's been nearly a decade! Where's the time go.

Anyway, it's gone through a few upgrades over the years but I've left it largely untouched. It works good enough, so why mess with it? I am curious though. It's not the only addon of its type, and certainly not the most popular either. Which leaves me wondering, how do other ones work?

## Hide Comments Everywhere

Addon: [Hide Comments Everywhere - Chrome Web Store](https://chromewebstore.google.com/detail/hide-comments-everywhere/bmhkdngdngchlneelllmdennfpmepbnc)
Source: [grantwinney/hide-comments-everywhere](https://github.com/grantwinney/hide-comments-everywhere/)

I'll start with my own first. The addon is split into two parts:

1. The code that's uploaded to the Google and Mozilla stores. It doesn't get updated often.
2. A file with key/value pairs, where the key is a domain and the value is the CSS selectors to block. The addon checks daily for updated definitions and downloads/caches them.

How it works, briefly:

- When a page loads, it injects a style sheet into the DOM, with CSS selectors to block.
- Then it checks if you chose to allow comments for the site, and disables the style sheet if you did.
- There's options for what the toolbar button should do when you click it, whether selections are remembered, a place to add your own selectors, update the cached definitions manually, etc.

The general rules, a little less briefly:

1. Determine which CSS selectors to *potentially* block.
	1. When a tab loads, look up the domain in the file and get the CSS selectors for the site.
	2. If the site isn't listed, use a set of "catch-all" selectors for common commenting systems.
	3. If a user added their own selector for the site in the blacklist box on the "options" page, use that instead of anything found in the file.
2. Determine whether those CSS selectors actually *should* be blocked.
	1. If comments were previously allowed for the current site, then don't block them.
	2. If a user created their own selector for the site in the whitelist box on the options page, or the site is one of the few sites listed in a global whitelist, then don't block them.
	3. However, if the site wasn't previously allowed, or it doesn't exist in the whitelist, or the user added their own selector to the blacklist, then _do_ block them.


## Shut Up: Comment Blocker

