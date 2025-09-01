---
categories:
- Reflections
date: "2021-07-05T19:20:06Z"
description: ""
draft: false
cover:
  image: photo-1592819695396-064b9572a660.jpg
slug: avoiding-tribal-knowledge-in-programming
summary: When I was less skilled as a developer, it was enough to just stay afloat,
  learning what I needed for the current day or project. As my skill and confidence
  grows, I've come to appreciate the extra things in life - like a decent set of docs.
tags:
- reflection
title: Avoiding tribal knowledge in the programming world
---
Whenever I've started a new job, the most overwhelming thing is getting up to speed with everything. How do I run this project; has anyone run into that problem before; how do I change the configuration? Do we have a license for such-and-such; what server hosts this website; am I running into a new issue or something we've already solved before?

No one person has all the answers, who _does_ have the answers can be unclear, and sometimes (shudder) _no one_ currently has the answers! Most knowledge is tribal, living in the heads of people who take bits and pieces of the puzzle with them when they leave... which can really suck for those of us left behind, cursed to sift through the ashes of whatever cobbled-together corner of the system they've left behind.

Eventually though, sometimes slowly and painfully, the answers come, the gaps are filled in, and the pieces fall into place. And then the next new hire comes aboard and it happens again.

![](https://images.unsplash.com/photo-1611329857570-f02f340e7378?crop=entropy&cs=tinysrgb&fit=max&fm=jpg&ixid=MnwxMTc3M3wwfDF8c2VhcmNofDR8fHB1enpsZXxlbnwwfHx8fDE2MjU1MDY2Nzg&ixlib=rb-1.2.1&q=80&w=2000)

Photo by [Sigmund](https://unsplash.com/@sigmund?utm_source=ghost&utm_medium=referral&utm_campaign=api-credit) / [Unsplash](https://unsplash.com/?utm_source=ghost&utm_medium=referral&utm_campaign=api-credit)

But is it really even our job to write documentation? Isn't a company just hiring us to write code? The electrician I hired to wire up our basement and garage didn't request a full electrical blueprint, nor did he leave me with a wiring diagram afterward. He investigated what was there, built on top of it, and handed me a bill.

Maybe if you're just doing contract work, you can get away with that, but if you're working longterm somewhere, the docs are for _you_ as much as for everyone else. Even shortterm, it seems like the responsible thing to do is leave behind a short user manual and troubleshooting guide, for everyone currently using it and for anyone who has to build on it later. After all, I know what change the electrician made, but the person I sell my house to won't.

When someone asks you about the work you did 6 months ago, how quickly can you answer them? Wouldn't it be nice to just send them a link to a document, instead of having to rack your brain and sift through code and notes? And hopefully, when they're done, they add their own notes in a version of the "leave the place cleaner than you found it" motto.

![Colorful Stack](https://images.unsplash.com/photo-1541692641319-981cc79ee10a?crop=entropy&cs=tinysrgb&fit=max&fm=jpg&ixid=MnwxMTc3M3wwfDF8c2VhcmNofDJ8fGJsb2Nrc3xlbnwwfHx8fDE2MjU1MDk2MTg&ixlib=rb-1.2.1&q=80&w=2000)

Photo by [La-Rel Easter](https://unsplash.com/@lastnameeaster?utm_source=ghost&utm_medium=referral&utm_campaign=api-credit) / [Unsplash](https://unsplash.com/?utm_source=ghost&utm_medium=referral&utm_campaign=api-credit)

If you're thinking, "who has the time for that?!", then you're thinking about it wrong. Documentation, like writing tests, reviewing requirements, and everything else about coding that's not _strictly_ coding, is best done as you go along. If you wait until the very end of the project, it'll be much more difficult. Eh, let's be honest - it won't happen at all.

When I started a new job last year, going through the same thing again, I took the opportunity to pick up a new habit. When a team member shows me how to access something, I document it. When we're setting up a new website or upgrading a third-party integration, I document it. When there's a meeting and I learn something new that'll come in useful in the future, I document it.

I'm documenting pretty much everything I learn about our codebase and infrastructure, and as I learn more, I build the documentation out. It's not like you need some elaborate system either, although [I do happen to like wiki's](https://grantwinney.com/creating-your-own-secure-wiki-using-dokuwiki/). We have a sharepoint site that's basically a glorified google docs, but it's organized, searchable, and accessible to everyone on the team.

![](questions-2245264_1280.jpg)

Image by [Gerd Altmann](https://pixabay.com/users/geralt-9301/?utm_source=link-attribution&utm_medium=referral&utm_campaign=image&utm_content=2245264) from [Pixabay](https://pixabay.com/?utm_source=link-attribution&utm_medium=referral&utm_campaign=image&utm_content=2245264)

So what's in the documentation? I try to answer the basic who, when, where, etc questions.

- Where's the app or part of the system hosted?
- How do we access it, change it, run it?
- What other parts of the system does it rely on?
- Who helped setup the infrastructure for it, developed it, or can help troubleshoot it?

Most of the time, it's information that's fresh in my mind anyway and easy to write down, but experience has taught me that one thing pushes out another, and in no time at all it'll be tough to remember!

Do your team (and yourself) a favor... avoid tribal knowledge!
