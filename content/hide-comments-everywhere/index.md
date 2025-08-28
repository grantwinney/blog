---
date: "2019-07-08T20:14:34Z"
description: ""
draft: false
image: kelly-sikkema-zwU2x2Yg-xY-unsplash-1.jpg
slug: hide-comments-everywhere
summary: Hides various commenting systems across the web, including (but not limited
  to) Disqus, YouTube, various news sites and forums, etc.
title: Hide Comments Everywhere
---
The comments sections on most major news outlets and social media sites are full of vitriol. I just wanted the content, so I wrote an extension that hides various commenting systems across the web, including (but not limited to) Disqus, YouTube, Instagram, replies on Reddit and Twitter, etc.

_(Getting it to work on Facebook was a pain, and required constant updates, so I don't really bother trying anymore... I recommend trying_ [_UnDistracted_](https://chrome.google.com/webstore/detail/undistracted-hide-faceboo/pjjgklgkfeoeiebjogplpnibpfnffkng)_.)_

Available for [Firefox](https://addons.mozilla.org/en-US/firefox/addon/hide-comments-everywhere/) and [Chrome](https://chrome.google.com/webstore/detail/hide-comments-everywhere/bmhkdngdngchlneelllmdennfpmepbnc), and works on Brave _(_[_natively_](https://support.brave.com/hc/en-us/articles/360017909112-How-can-I-add-extensions-to-Brave-)_)_ and Opera _(requires an_ [_additional extension_](https://addons.opera.com/en/extensions/details/install-chrome-extensions/)_) too._

![](https://grantwinney.com/content/images/2019/07/hide-comments-2.jpg)

![](https://grantwinney.com/content/images/2022/03/popup.png)

![](https://grantwinney.com/content/images/2022/03/options1.png)

![](https://grantwinney.com/content/images/2022/03/options2.png)

## How's it work?

It checks whether or not comments should be blocked, based on a global set of definitions, in combination with your own preferences about which sites should show or hide comments.

Click on the icon in the toolbar to toggle showing/hiding comments for a site. You can choose whether your selections are remembered when you visit the site again with a setting on the options page.

When the page is loaded, the appropriate style sheet for the site is injected into the DOM, and then the style sheet is either enabled or disabled based on whether the comments should be shown or not.

If you're interested, check out the [source code](https://github.com/grantwinney/hide-comments-everywhere/).

## Contributions / Questions

If you notice a commenting system that could be blocked by default, [open an issue](https://github.com/grantwinney/hide-comments-in-chrome-sites/issues/new). Include the website where you noticed it, and I'll try to tackle it as time permits.

If you're comfortable with RegEx and HTML/CSS, you could just create a pull request against [the file that defines which sites and html elements are blocked](https://github.com/grantwinney/hide-comments-in-chrome-sites/blob/master/sites.json).

Have a question, comment or request? [Open a new issue](https://github.com/grantwinney/hide-comments-everywhere/issues/new) with as many details as possible. The more you let me know upfront, the less I'll have to ask later. I'll get to it as time permits.

## FAQs

Okay, these aren't _really_ frequently asked questions. I mean, someone somewhere probably asked them.. at least once. They're definitely questions though.

### What permissions does it need?

You'll be notified that it can "read and change all your data on the websites you visit" because that's how it works - it hides comments when the page loads so you don't have to see them.

### What's the order of preference?

In order to determine whether (and how) to hide or show comments on a particular site, Hide Comments Everywhere has a two-step process.

Step 1 is determining which styles to inject into the page.

1. First, it checks the global definitions stored on GitHub (which are cached locally) for the current site. If a match is found, it applies the styles for that site.
2. Then, if a match isn't found, it applies a catch-all set of styles that are frequently used by commenting tools and content management systems.
3. Finally, it checks your personal blacklist to see if you've set your own style for a particular site. If you have, it trumps anything set in step 1.

Step 2 is determining whether or not to enable or disable those styles.

1. First, it checks to see whether you've previously clicked the "toggle" button to hide or show comments on a site.
2. Then, it checks your personal whitelist. If you've included a site that should always show comments, it allows them even if you previously clicked the "toggle" button to hide them.
3. After that, it checks a global whitelist of sites that should always be allowed. There's only a few sites in that list, that don't play nicely with this addon, like GitHub. Anything there trumps your personal whitelist or toggling.
4. Finally, it checks your personal blacklist. If you've got a site on there, it trumps everything, even if it's in the global whitelist. My thinking is, if I couldn't find a reasonable way to block comments on a few sites but you _did_ somehow, well then... more power to ya.

After the page loads, you can still click the "toggle" button (but the effect is only temporary if the site is in one of the whitelists or your blacklist, because when the page reloads, the above logic will run again).

### What kind of data does it save?

It checks for updated definitions that I store on GitHub. If it finds updated definitions, it'll cache them in local storage, but it only checks once daily. You can open the Options page and force it to get the latest definitions if you want.

Everything else is stored in sync storage, like the settings and whitelist and blacklist on the Options page, and whether or not you toggled comments on or off for particular sites. If you don't use sync, those calls (iirc) just revert to local storage.

I don't use analytics or anything else to track what you're doing. If you want to share anything, there's myriad ways, like on GitHub, or by email, or just by leaving a comment below (yeah I know, kinda ironic).

Hope you find this addon as useful as I do!