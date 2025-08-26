---
categories:
- CSS
- Coding
date: "2020-02-13T23:50:42Z"
description: ""
draft: false
cover:
  image: https://images.unsplash.com/photo-1517433456452-f9633a875f6f?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=2000&fit=max&ixid=eyJhcHBfaWQiOjExNzczfQ
slug: how-to-make-a-dark-mode-with-css
summary: Every time I learn some new piece of CSS I'm amazed at how flexible and powerful
  it is. Like how easy it is to tailor your site for your visitor's "dark mode" preference!
tags:
- CSS
- Coding
title: How to make a dark mode with CSS
---


Every time I learn some new piece of CSS I'm amazed at how flexible and powerful it is, and the prefers-color-scheme media element is no exception. The "dark mode" setting from a visitor's desktop or mobile device can be passed to the browser, which then applies it to your site according to your style sheets. So. Cool.

MDN has a good example and lots of notes, as usual (I love their docs!), and I created my own fun little example below. To try it out, toggle between light and dark mode on your device, and the sun should change to a moon. If it doesn't work for some reason, you can see what it should do in the screen capture at the bottom of this post. 🌞 🌜














How's it work?

Without diving too deep, here's a few pieces to the puzzle...


Responsive Design

It's possible (and has been for years) to design a website that responds to the device a visitor happens to be using, such as the wildly different screen sizes between a mobile device vs a desktop. This could involve writing JavaScript, but as CSS is given more power, it's able to handle most layouts all by itself.


Media Queries

One element of CSS that figures into responsive design is the media query, which can account for things like screen resolution, orientation, and whether the visitor prefers higher contrast colors.

For this to work, a device has to make this data available to the browser, which in turn has to use it to apply the correct CSS layout to the page. Different layouts result in different color schemes, collapsed menus, sidebars dropping below the post, etc.


Prefers-color-scheme Feature

One of the media features, called prefers-color-scheme, is used to determine whether the user prefers light mode or dark mode. It's based on their device settings, and most browsers support it.

You specify your "base" styles first - whatever you want applied no matter the device setting - and then you can override those based on whether the visitor prefers light or dark mode. Here's the code I used for the images above:

<style type="text/css">
    #pic {
        margin: auto;
        height: 400px;
        width: 400px;
        background-image: url("https://grantwinney.com/content/images/2020/02/sun.png");
        background-size: 360px 360px;
        background-repeat: no-repeat;
		background-position: center;
    }

    @media (prefers-color-scheme: light), (prefers-color-scheme: no-preference) {
        #pic {
            background-color: skyblue;
            background-image: url("https://grantwinney.com/content/images/2020/02/sun.png"); }
    }

    @media (prefers-color-scheme: dark) {
        #pic {
            background-color: midnightblue;
            background-image: url("https://grantwinney.com/content/images/2020/02/moon.png"); }
    }
</style>

<div id="pic"></div>

Notice that there's a setting for when they haven't indicated a preference, and I used the , to indicate an "or" clause, meaning (in my example) that the sun image and skyblue background will show up if they've selected "light" mode or nothing at all.

And just in case it doesn't work for you, which probably means you're either using an unsupported browser or your device doesn't pass those settings to the browser, this is how it looks in Windows when I toggle between modes. In clockwise order is DuckDuckGo who has a whole separate theme, the MDN example I loved so much, the Windows settings for dark mode, aaaand.. some new-agey sun/moon example someone put together.
