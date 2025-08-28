---
date: "2019-07-08T19:16:16Z"
description: ""
draft: false
image: https://images.unsplash.com/photo-1619468129361-605ebea04b44?crop=entropy&cs=tinysrgb&fit=max&fm=jpg&ixid=MnwxMTc3M3wwfDF8c2VhcmNofDMzfHxwaW5zfGVufDB8fHx8MTYzNDA0NjgyOQ&ixlib=rb-1.2.1&q=80&w=2000
slug: generate-links-for-headers
summary: Ever wanted to share a link, not just to a webpage, but to a particular section
  of a webpage? This extension automatically generates links for all headers on the
  page, to make it easier to share links that jump to a specific section.
title: Generate Links for Headers
---
Ever wanted to share a link, not just to a webpage, but to a particular _section_ of a webpage? GLfH automatically generates links for all headers on the page, to make it easier to share links that jump to a specific section.

It's available for [Firefox](https://addons.mozilla.org/en-US/firefox/addon/generate-links-for-headers/) and [Chrome](https://chrome.google.com/webstore/detail/generate-links-for-header/dckfkngmahjdokkkmconmfjdmicjcmgf) (so [Brave](https://support.brave.com/hc/en-us/articles/360017909112-How-can-I-add-extensions-to-Brave-), [Opera](https://addons.opera.com/en/extensions/details/install-chrome-extensions/), and Edge too).

![](https://grantwinney.com/content/images/2022/03/image.png)

## How's it work?

Most of the time, when you see a header on a webpage, it has an ID associated with it. You can use the ID to create a link to that section, but that means viewing source code, finding the header element, and appending the ID to the URL before sharing it. It doesn't have to be that hard.

This extension scans the page and generates links for all headers on the page for you, assuming they have an ID or Name assigned.

- Hover over any header, and an anchor link will appear.
- Click on the 🔗 link icon to navigate to that anchor, and to copy the link to your clipboard at the same time.

If there's no ID or Name in the header tag, it will search any elements nested inside the header tag for the first one with an ID or Name assigned. Failing that, it looks at the immediate parent element (i.e. a header nested in a DIV). If it still finds nothing, then no link will be generated or shown.

You can [read more details here](https://grantwinney.com/automatically-adding-links-next-to-all-headers-on-the-page-a-chrome-extension/).

## Source Code

The [source code](https://github.com/grantwinney/generate-links-for-headers-in-chrome) is available on GitHub.

## Permissions

It needs to read and modify all pages, because that's what it does. It uses JavaScript to scan the page for headers with IDs (or Names), and then injects one link for each header that has it present.

## Contributions / Questions

If you have a fix, open a PR in GitHub. If you have a question or comment, [open an issue](https://github.com/grantwinney/generate-links-for-headers-in-chrome/issues/new). I'll get to things as time permits.