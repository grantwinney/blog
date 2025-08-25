+++
categories = ["Reflections"]
date = 2023-08-24T21:30:16Z
description = ""
draft = false
image = "https://images.unsplash.com/photo-1582455462467-b2bf6e3e211f?crop=entropy&cs=tinysrgb&fit=max&fm=jpg&ixid=M3wxMTc3M3wwfDF8c2VhcmNofDR8fGRpZ3xlbnwwfHx8fDE2OTI4OTk0MTd8MA&ixlib=rb-4.0.3&q=80&w=2000"
slug = "even-in-failure-success"
summary = "When what we're trying to accomplish fails, the extra knowledge and clarity we get just by making the attempt is a win all by itself."
tags = ["Reflections"]
title = "Even in failure, an increase in understanding is a win"

+++


Adding something new to an application is kind of exciting, especially when it's an obvious change, and even moreso when it's something the users actually want. Then there's the behind-the-scenes kinda changes, where you solve some problem in the system, and even though (best case scenario) no one will know you were there, there's still a sense of accomplishment. And of course there's everything related to development too - implementing and figuring out issues with DevOps and git and installers and whatever programming tools you might want to use, etc.

Sometimes though, it's exciting to remove things too.

The more a system grows and ages, and the more hands that touch it and change it and leave their prints all over it, the more complex and unwieldy it becomes. Something that took a few steps to begin with, grows into something with 20 steps that no single person completely understands anymore, which makes it that much harder to ever simplify again. And for those who attempt it, success is never guaranteed. The road to hell and all that...

I had an opportunity recently to attack a problem like that. We have a deployment process for some reports that involves myriad steps, making updates in this file and that one, copying this over here and that over there, building multiple repos and bringing it all together into one master install. There's a long list of steps that's been built up over time, some parts automated and others definitely not, and I had the audacity - the sheer arrogance - to attempt to remove a couple of those steps. lol

Long story short, it didn't work. It seemed to at first, but after a complete runthrough of everything, it definitely wasn't. No big deal, not the end of the world, it just meant I had to undo my changes. It isn't the failure that I'm thinking about though, but what I did with it. My first instinct was to say "aw crap", revert all my changes, and just move on. Full stop, reverse course, nothing to see here.

Then I wondered, but why?

I spent an evening picking apart the steps I'd changed, and the steps around the steps I'd changed. After digging for a few hours, I found an installation script that ran some other command line things, and discovered that something being copied from Point A wasn't in the correct format anymore when it arrived at Point B. After reviewing the steps with someone on the team, I learned of yet another step (unknown to me) where certain files are run through a utility and then exported, altering the format slightly... in just the way needed for the now-failing installation script.

Unfortunately, eliminating the need for that step would be complicated, involve others' time rather than just mine, and was not the hill to die on mid-project. I was bummed at first, but then realized that the end result of my failure was understanding a process much better than before. In fact, I can see a time in the near future, due to some work being done by other teams, where I'll be able to revisit this and make it work like I'd hoped it would, undoing some tech debt that's accumulated over the years.

Sometimes we don't get the chance to dig deeper.. there's just no time. I've had quite a few of those. But when we can, even if there's no "big win" to ultimately be had, more clarity around a thing is a "win" by itself. Like standing too close to a pixelated image, and then moving a little further back, and a little further back, we keep getting a better and better view of how things work.