---
title: Pull requests aren't a stamp of approval
slug: pull-requests-are-not-a-stamp-of-approval
summary: Pull requests are a chance to ask, learn, and make sure that the code being merged is something EVERYONE is comfortable owning.
description:
date: 2025-11-02T01:04:00
draft: false
postimage: pexels-towfiqu-barbhuiya-3440682-11412596.webp
postimagealt:
postimagecaption: Photo by <a href="https://www.pexels.com/photo/a-marker-near-checked-circles-on-white-paper-11412596/">Towfiqu barbhuiya</a>
categories:
  - Reflect
tags:
  - code-review
  - teamwork
---
Despite a few side projects and a lot of playing around (like on this blog), the vast majority of the code I'll ever write will be for large projects shared by dozens or even hundreds of developers, all of them contributing to millions of lines written over *decades*. I'll come along, merge a few changes and add a few more layers to the code cake, and a hundred more developers (who knows, maybe even you?) will add even more in the years to come.

That's the way it is with writing code for large companies, where we don't get to see the beginning or the end. Our changes build on top of other changes, layer on layer, and the bigger the change, the bigger the impact down the line. When one layer has mistakes though, everything around it is tainted too. Wouldn't it be nice if we could check out each other's code _before_ it got added to the mix? _Oh wait..._

It took me a long time to see the value of [pull requests](https://grantwinney.com/what-is-a-code-review/) (I used to be arrogant about my code - hey, it works on _my_ machine), but now that I do, it's frustrating when I reach out hoping for advice and feedback, only to be given a quick stamp of approval. If I touch a piece of code that Bob the dev worked on last month or a stored proc that Susan the DBA created, and I tag them on a PR, it's *not* a formality. This process is for _everyone's_ benefit!

**What I write today (good, bad, or ugly) is what _they_ inherit and have to work with tomorrow, and vice-versa.**

And so, I _want_ their feedback, their constructive criticism, their optimizations. I want to know if I'm going wrong and where, and that they're comfortable with what I'm about to merge in, because _they_ may be the next person who has to deal with it... and who knows if I'll be around to explain (or help fix) it when they do!

If you get tagged on one and you're wondering what's the point, remember their code will be yours to deal with tomorrow. So spend a little time, ask some questions, and provide that constructive feedback. You'll be doing everyone a favor! 😉