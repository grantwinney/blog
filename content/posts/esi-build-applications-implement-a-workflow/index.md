---
categories:
- Azure DevOps
- ESI
- DevOps
date: "2022-07-16T18:48:55Z"
description: ""
draft: false
cover:
  image: photo-1601388152430-4ad0f14c0788.jpg
slug: esi-build-applications-implement-a-workflow
summary: Continuing with Microsoft's ESI, I spent some time this week learning about
  workflows, reusable elements, and protecting and monitoring the code.
tags:
- Azure DevOps
- ESI
- DevOps
title: 'ESI: Building Apps with Azure DevOps - Implement a Workflow'
---


A few weeks ago, I wrote about the Enterprise Skills Initiative, a program from Microsoft that my current employer has access to, so.. yay, free learning material.

ESI: An Intro to Azure DevOpsFor the next few weeks, I’d like to write a few blog posts as I go through the Enterprise Skills Initiative, a Microsoft website that seems to consist mostly of courses on learning Azure, with a little bit of GitHub and VSCode thrown in too. I got an email fromG.Winney 🖱Grant Winney

DevOps in general, and Azure DevOps specifically, is something I've had a growing interest in, ever since someone at our company set it up a couple years ago. The whole "DevOps" concept is worth learning more about (sprints and workflows, collaboration and communication, and all that), but I'm really interested in scripting and automation at the moment.

I'm on the lookout for opportunities to use it, like the job I recently setup at work that watches for pushes to master for a number of repos, then triggers an automatic draft PR to pull those changes into each team branch and fires off a notification to the team lead. If there's no merge conflicts, they just publish the PR, add a couple team members to review it, and merge it in at their discretion. It replaced an important process that's easy to forget about.

This week I moved on to the next learning path, building apps with Azure DevOps.

Build applications with Azure DevOps learning path - LearnIn this learning path, find out how to collaborate with others to continuously build, test, and verify your applications using Azure Pipelines and GitHub.Microsoft Docssteved0x

Each of the learning paths have hours of material to go through, so I just focused on the one for creating pipelines and workflows.

Implement a code workflow in your build pipeline by using Git and GitHub - LearnLearn along with the Space Game web team the benefits of collaboration through Visual Studio Code and GitHub.Microsoft Docsjuliakm

Microsoft has things setup so that, once you create your free account, a single button click generates the entire environment you need for the current lesson. At the end, one more click deletes everything. Poof. You miss some of the finer details in setting things up, but then, should you really need to craft your own measuring cups and harvest grain in order to learn how to bake a cake? You can always delve deeper later when there's a reason too.

This week's lesson included features for abstraction and reusability. If you want to run a job under several similar environments, you can inject variables that change values when the job runs. And then there's templates, which I haven't gotten to use before but can help DRY up jobs that are largely duplicates with a few minor differences between them.

Templates let you define your logic one time and then reuse it several times. Templates combine the content of multiple YAML files into a single pipeline.

There was also a bit about adding badges to your readme file, something that I've used before and find somewhat useful when I'm checking out other projects on GitHub. The lesson demo'd adding a badge to show whether the last build passed or not, but there's all kinds of other badges you can add too, some of which are more useful than others.

They also covered a bit on protecting the branch from people just pushing things in (very good idea) and a bit on setting up a dashboard for the project (this one-man jury's still out). We use a dashboard at work, and it's only marginally useful. I usually forget it's there. Some of the modules aren't customizable, and the one that shows a webpage inside an iframe just "refused to connect" on everything. I think the main benefit would be to have it displayed on a large monitor where the whole team could see the health of the project at a glance. Or maybe it's for the project manager working remotely. Meh.

So far I'm finding the material good, even though I know most of what I've seen so far, just from day-to-day use at work. The storyline aspect of the lessons are pretty thin and contrived. You're following a team who had a rough release, and definitely some areas they could improve in, and the newest developer they hired just happens to be familiar with devops'ing all the things, and she's begun to overhaul everything. Completely realistic. Every lesson sees her converting the team to GitHub, VS Code, Azure DevOps, and whatever else Microsoft owns. It's not really entertaining to me, but uh, I guess it's less dry than just presenting the material?

Oh, and one little comment about their use of "main" instead of "master" for the production branch. Without getting into the politics or ideology of anything, there's been a significant push by GitHub to rename "master" to "main". Fair enough. But they state in the lesson that:

In Git, the default branch, or trunk, is typically called main

That's simply not true. On GitHub, sure, they've reworked things so the master branch is named "main" by default unless you take specific steps to disable/revert it. There's nothing to force that in Git (GitHub is merely one tool that adds features on top of Git), and in projects going back many, many years, the production branch is still named "master". That's just something to be aware of, if you're new to all this and wondering why reality doesn't match their claim.

Looking forward to continuing with their lessons... After all, why take a chance on missing vital steps, when you can write a script and guarantee nothing's forgotten? Why do something manually if you can have it done for you? Why check on some change in state repeatedly, when you can have an environment like Azure DevOps watch it for you, do most of the work for you, and then notify you that there's some action that needs your attention?

Automation is the pepto bismol to our manual process heartburn. :)
