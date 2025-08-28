---
categories:
- Bots
- DDOS
- Security
date: "2021-12-24T02:40:30Z"
description: ""
draft: false
cover:
  image: momentista-Cdr28uRzzFI-unsplash.jpg
  relative: true
slug: why-am-i-getting-a-please-complete-the-security-check-error
summary: I visited a page the other day and saw something odd.. a page that said it
  was checking my browser. It's unpolished and unfriendly, and my first instinct was
  to just close the tab and forget it. Then I dropped to the bottom of the page and
  noticed the link to Cloudflare. Interesting.
tags:
- Bots
- DDOS
- Security
title: Why am I getting a "Please complete the security check" error?
---
I use [Fathom Analytics](https://usefathom.com/) to see where traffic's coming from. It's very privacy-friendly though - the only thing I can see is the top-level domains, not the exact page or visitors' IP addresses. Anyway, it's fun sometimes to visit the sites where people are coming from, and see what they're about, and it shows me which content is most relevant. Is it a Raspberry Pi site linking to one of my demos from a few years back? Is it another developer expanding on some sample code I posted, or some course at a university that linked to one of my introductory types of posts?

I went to visit one the other day though, and saw something I hadn't seen before.. a weird page that said it was checking my browser. This is such a bizarre, unfriendly looking landing page, that my first instinct was to just close the tab and forget it. Then I dropped to the bottom of the page and noticed the link to Cloudflare. Interesting...

![](https://grantwinney.com/content/images/2021/12/image-7.png)

## Is it legit or some kind of scam?

It's legit. This might seem like a silly question to some, but that page weirded me out.

If you've never heard of Cloudflare, it's a popular service that offers, among other things, protection from [bots](https://www.cloudflare.com/products/bot-management/) and [DDoS attacks](https://www.cloudflare.com/ddos/) for websites all over the world. It's similar to the protections offered by Akamai, which [I stumbled on (and wrote about) a couple years ago](https://grantwinney.com/websites-requesting-access-to-motion-sensors/). As a webmaster, you reroute your traffic through them, and they can analyze it and block bad actors (i.e. people trying to send so many requests to your website that they crash your server and bring the site down).

The [hCaptcha](https://www.hcaptcha.com/) service, on the other hand, was a new one to me. The appearance and name is so similar to Google's reCaptcha service, which we've all seen everywhere, that it made me hesitate. It's like finding a Chrumbook laptop or a browser called FurFox. Just slighly off lol.

It's legit too though, and Cloudflare even posted an entire article last year about what hCaptcha is and [why they decided to use it](https://blog.cloudflare.com/moving-from-recaptcha-to-hcaptcha/). I guess the underlying technique is called Captcha, and [Google's flavor is reCaptcha](https://support.google.com/recaptcha/answer/6080904?hl=en). Maybe there's other services too, like iCaptcha, jCaptcha, and CaptchaInTheRye. Who knows.

I signed up for Cloudflare just so I could check out their settings, and got a link in an email that I had to click on to verify who I was. It opened a page on cloudflare.com and.. gee, that looks familiar.

![](https://grantwinney.com/content/images/2021/12/image-10.png)

While I was in there, I checked some of the different settings a webmaster can configure, and there's a lot. In a firewall section, the "security level" is set to medium by default. I didn't enable the service for my site to confirm, but my guess is if someone wanted to crank things up to 11 across the board (set the level to High or even I'm Under Attack), Cloudflare would probably show that page to _every_ visitor, which may be what's happening to me.

![](https://grantwinney.com/content/images/2021/12/image-11.png)

## How can I fix it?

I'm no expert on this, but seeing it peaked my interest enough to do some research. It may not be very helpful though. There's not a really an appeal process I saw, where you can ask Cloudflare to unflag you or whatever.

If the website owner cranked up those settings above, that's their choice and there's not too much you can do. If it's a site you normally use and don't see that on, maybe they were under some kind of DDOS attack and turned things up temporarily to protect themselves. It may just go back to normal in a few days.

It's possible that your IP address was flagged for some reason. If your using the network in a large corporate building, or on a university campus, or in any public location, there could be just one or a few public-facing IP addresses that are divided internally among hundreds of other people in the same building. If anyone on the network does something shady and Cloudflare decides to flag the public IP address, the party's over for everyone. I think the giveaway here is that you'd start seeing the above page on a _lot_ of sites, since I'd wager quite a few use Cloudflare and normally it's completely transparent to you. Not a ton you can do, unless there's a sys admin somewhere who can track down whatever shadiness might be taking place, then get a new IP address.

That seems a lot less likely on a home network, but not impossible. I did come across a thread where someone had this problem at home, and they somehow figured out that their ISP was placing multiple customers behind a single IP or something. They brought it to the attention of the ISP who was able to help them out, and apparently go after the individual who was ruining it for everyone. ymmv

If you're just seeing it on one site though, it's probably them not you. Check the box if you really want in, find those traffic lights and crosswalks, and go on with life. :)

## What set off my alarm bells?

The page, while legit, is unpolished and has just _got_ to be confusing to the average Internet user. I really don't know why a large outfit like Cloudflare is presenting the page like this, unless they're hoping to just scare visitors off. 😏

- First and foremost was the suggestion that the visitor go check for viruses and malware. That absolutely seems like something a scam site would suggest, because then the very next thing would be a suggestion to download some software to fix it... like the (poorly rated) "Privacy Pass" browser extension they suggest installing. Which actually appears to be legit too, and uses some similarly named protocol that the IETF is [considering making an official standard](https://datatracker.ietf.org/wg/privacypass/about/). Is the typical user _really_ going to download an extension and figure out how to generate tokens that bypass confusing security warning pages? (Spoiler: No)
  
- Calling the website a "web property" below the captcha seems weird too. Like someone messed up the term, which is something I'd expect to see in a poorly worded phishing email. But maybe this thing can pop up for anything accessible on the web, like APIs and FTP sites or something, so they chose "web property" to cover them all? How about "and will grant you temporary access to this resource"?
  
- The lack of links to any other resources explaining what's happening and why, other than a link to Cloudflare's [500 error](https://www.cloudflare.com/5xx-error-landing/) page that states "Something went wrong (but it’s not your fault)". That's _meaningless_. True, a 500 error is a server error, and when it happens on a site it's not the visitor's fault. But I just came from a page that told me to check for malware on my computer, and then I saw settings that can be adjusted to most likely show this. Is it my fault? My computer? The website owner? Maybe. It's definitely not a 500 error on the server though.

I'm gonna stop here. This it turning to more of a rant than anything helpful, but the more I write about it, the more I feel like Cloudflare is missing the mark. There's an opportunity to educate mere mortals who get caught up in their nets for one reason or another, but instead the page really does kinda look like something a server might toss up if it were throwing a 500 error.

If you want to dig in further, [learn more about Cloudflare here](https://support.cloudflare.com/hc/en-us/categories/200275218).
