---
categories:
- Surviving WinForms
- WinForms
- CEF
- CEFSharp
- Coding
date: "2022-08-16T03:56:47Z"
description: ""
draft: false
cover:
  image: photo-1523726491678-bf852e717f6a.jpg
slug: displaying-an-iis-hosted-site-in-cefsharp
summary: Thanks to CEFSharp, we can finally bring WinForms to the web! That didn't
  sound right. Okay, let's just look at hosting a site in IIS and showing it.
tags:
- Surviving WinForms
- WinForms
- CEF
- CEFSharp
- Coding
title: Displaying an IIS hosted site in CEFSharp
---
A few weeks ago I shared how you can use [CEFSharp](http://cefsharp.github.io/) to display an html page. I called it hosting, but uh, it was really just displaying a single html page that was baked into the project itself. Yeah, cutting edge, I know. It's WinForms, the bar is low folks.

[Host a simple webpage in WinForms with CefSharp](https://grantwinney.com/hosting-a-simple-webpage-in-winforms-with-cefsharp/)

This time we'll host the same page in IIS and see how CEFSharp can still interact with it. I won't give a whole overview of IIS, but if you want to try it yourself, here's a few steps.

> The code in this article is available on [GitHub](https://github.com/grantwinney/Surviving-WinForms/tree/master/Web/CEFSharp/BasicCefSharpIIS?ref=grantwinney.com), if you'd like to use it in your own projects or just follow along while you read.

Enable IIS in Windows Features, if it's not already.

![](https://grantwinney.com/content/images/2022/08/image-6.png)

Copy the `BasicCefSharp_Site` folder from [my sample code](https://github.com/grantwinney/Surviving-WinForms/tree/master/Web/CEFSharp/BasicCefSharpIIS) into the `c:\inetpub\wwwroot` directory. Reusing that location will make your life easier, since there's some security on the `wwwroot` folder that you won't have to recreate.

![](https://grantwinney.com/content/images/2022/08/image-14.png)

In IIS, right-click sites and add a new website. Nothing matters in here, other than pointing to the correct location on disk, and changing the port if you don't want to interrupt something else already running locally on the default port 80.

![](https://grantwinney.com/content/images/2022/08/image-12.png)

Open the new website in your browser and you should see the sample page in all its amazing hypertext markup glory.

![](https://grantwinney.com/content/images/2022/08/image-15.png)

Fire up the WinForms project, and give it a go. You'll need to change line 27 if you picked a different port. Otherwise, it behaves like it did [last time](https://grantwinney.com/hosting-a-simple-webpage-in-winforms-with-cefsharp/), except now you're interacting with an actual website running locally, and not just a one-off html page.

I couldn't help monkeying with it a little bit though, adding an address bar that subscribes to the CEFSharp control's `AddressChanged` event handler to display the current URL. Oh, and it now has _twice_ as many hypertextual markedup pages!

![](https://grantwinney.com/content/images/2022/08/cefsharpiis-1.gif)