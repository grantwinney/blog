---
categories:
- Reflections
date: "2021-12-20T12:00:00Z"
description: ""
draft: false
cover:
  image: photo-1512411538328-27d71d1921cd.jpg
slug: sunk-costs-timeboxing-asking-for-help
summary: One of the biggest struggles I have with programming is knowing when to ask
  for help. A little struggle is necessary for growth, but when am I just wasting
  time when I could be learning from others?
tags:
- Reflections
title: Sunk costs, timeboxing, and asking for help
---


One of the biggest struggles I have with programming (or anything I'm trying to
learn) is knowing when to ask for help. Ask too soon, and I might miss an
opportunity to grow in self-reliance and self-confidence. A little struggle is
necessary for growth. But ask too late, and I might just be wasting time and
missing the opportunity to learn from others.

Last week, I was updating some of our DevOps builds for a new environment (if
that's a new term for you, read more here
[https://azure.microsoft.com/en-us/overview/what-is-devops]), when one of our
team's jobs to deploy some code to a server began failing. I thought maybe the
wrong version of the code was being deployed, but that wasn't it. I redeployed a
couple times, then started digging into the logs on the server.

After several hours and no answers, I reached out to someone else on the team.
She asked where the job was deploying the code to. Was it pointing to the new
environment?

No. No it was not. 😐

The code I tried redeploying several times would've fixed the original issue,
but unfortunately it was being deployed to the old environment. It had taken me
longer to explain what I'd tried than it did for my coworker to find the thing
I'd missed!

And that's the struggle. How do you know when to reach out? It's so easy to feel
like you've already invested time in finding a solution so you can't stop now
[https://thedecisionlab.com/biases/the-sunk-cost-fallacy/]. Just one more thing
on google to try, just one more hour (then one more, and one more) and you'll
have a solution. Almost there...

The best thing I've tried so far (when I remember to do it!) is to time-box my
efforts [https://www.agilealliance.org/glossary/timebox]. If it seems likely
that you'll find a solution in an hour, and still reasonable that it might even
take a few hours, then make a deal with yourself to re-evaluate where things are
at after 3 or 4 hours. If you've exhausted the obvious things to try, or you're
just going in circles, it's time to tag someone else in. Sometimes, putting
your
thoughts in order
[https://www.parkersoftware.com/blog/rubber-ducking-not-just-a-funny-phrase/] is
enough to realize what you missed. And when that's not enough, a second opinion
never hurts.
