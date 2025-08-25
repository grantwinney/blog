+++
categories = ["Reflections"]
date = 2023-11-14T04:31:52Z
description = ""
draft = false
image = "https://images.unsplash.com/photo-1597425098166-90b88803284b?crop=entropy&cs=tinysrgb&fit=max&fm=jpg&ixid=M3wxMTc3M3wwfDF8c2VhcmNofDJ8fHN3aXJsfGVufDB8fHx8MTY5OTkyNzA4OHww&ixlib=rb-4.0.3&q=80&w=2000"
slug = "swirly-mass-of-code"
tags = ["Reflections"]
title = "A swirly mass of shared code"

+++


I heard a story recently, where a team was asked, after spending months adding a set of features to a codebase, to remove a specific feature from very early on in the project, right before the release date. Other features had been built around it and on top of it. Without necessarily even intending too, the devs that came after that code was written would've had to understand it in order to add to it. I don't know what the outcome was, but that's not an easy ask.

If you're a fan of Harry Potter (the older stuff, not The Cursed Playscript or The 50 Incarnations of Grindelwald trilogy), you've heard of pensieves. Those little dishes of weird, smoky, flowy "thoughts". A person could offload new thoughts into them, slosh the contents around like a fine wine, let them sit there mixing and simmering for awhile, and then extract them later to gain new insights.

Like everything in the wizarding world, the rules around pensieves and extracting thoughts were pretty loose. Harry jumps into them and experiences other people's thoughts, and Snape ends up handing some of his own over to Harry, after which Harry would remember them so... are those new thoughts or did he just soak in someone else's? And don't get me started on why Mrs Weasley could make the dishes wash themselves, but not accio some bricks to make a bigger house.

A codebase is a bit like a pensieve.. one that an entire team is sharing. Each dev adds some code and it's mixed in with everything else - all the thoughts, ideas, and goals that came before. Each new thing builds on, and touches, and affects the rest, and what you end up with is a new thing that's not exactly what it was before. Ever changing, ever mutating.

Devs that come even later dip their faces right into that swirly mass of code (quite the image) and, combined with their own experience, come away with a new insight into how the code works. Later on they'll add their own thoughts and code to the swirling mass, and the codebase will look different, again. For a short time, you could extract those new thoughts from the codebase, but that gets pretty tough pretty quickly.

A lot of things are out of our control when we're writing code though. That request to remove a feature.. stuff like that happens. Some feature depends on a third-party integration that's not done yet, and there's nothing anyone can do about it. Before trying to pull out every trace of code related to a feature though, it's certainly worth thinking about how to do the least impactful thing, like commenting out a few lines that affect some important calculations, or the single line that calls the rest of the code in question.

After all, safely removing code can take as long as adding it in the first place. 😬