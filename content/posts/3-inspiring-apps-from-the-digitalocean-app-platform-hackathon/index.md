---
categories:
- DigitalOcean
date: "2021-01-12T16:42:00Z"
description: ""
draft: false
cover:
  image: photo-1504384764586-bb4cdc1707b0.jpg
slug: 3-inspiring-apps-from-the-digitalocean-app-platform-hackathon
summary: A few of my favorite submissions from DigitalOcean's App Platform hackathon,
  that hold real promise for bringing people together!
tags:
- DigitalOcean
title: 3 inspiring apps from the DigitalOcean App Platform Hackathon
---
[DigitalOcean](https://m.do.co/c/448f25462030) ran a month-long [hackathon](https://dev.to/devteam/announcing-the-digitalocean-app-platform-hackathon-on-dev-2i1k) to drum up excitement for their new [App Platform PaaS](https://www.digitalocean.com/docs/app-platform/). I haven't tried it yet, but I'd like to (and probably should have since they were offering $50 credits). Most devs just want to bang out code, not worry about provisioning hardware and infrastructure, and it sounds like that's where App Platform comes in.

There were a lot of submissions to check out. Some were fun, others looked like they were a good technical challenge, but a few stood out (to me, anyway) as holding real promise for helping bring people together. 🤝

## Goodeed

One of the great things about living in a community is the opportunity for sharing resources. Whether it's food, a power tool, or just an extra hand, I've asked for it and lent it out, and a lot of my neighbors are the same. I've wished more than once that it were easier for people to get in touch when something was needed.

[Jiayi](https://dev.to/chewypao) created an app that lets you set your name and location, then make a request - for anything. People who are geographically close to you see your request, and can start a conversation. If they're willing to share, you mark the item as borrowed. It stays in your dashboard until you return the item and delete your original post. 👍

[Goodeed - DO Hackathon Submission - DEV Community](https://dev.to/chewypao/do-hackathon-submission-goodeed-app-53c7?ref=grantwinney.com)

[try it out](https://goodeed-app-ugrpa.ondigitalocean.app/) | [deploy it](https://cloud.digitalocean.com/apps/new?repo=https://github.com/annacjy/goodeed-app/tree/main) | [source code](https://github.com/annacjy/goodeed-app)

## HandSign

Years ago, I went through a phase where I tried learning ASL. I memorized the alphabet and a few words, but all I had were reference books (yeah.. it was awhile ago). There was no one to practice it with either.

[Syauqy](https://twitter.com/syauqy) used [TensorFlow](https://www.tensorflow.org/) (machine learning) and some hand gesture models to create an app that translates the position of your fingers into valid sign language. It doesn't work for every letter (like J or Z that require movement), but it does a pretty good job. Taken to the extreme, I imagine something like this could help people learn ASL, or even serve as an ASL-to-text translation service.

[HandSign - Learn a Sign Language with Your Camera - DEV Community](https://dev.to/syauqy/handsign-learn-a-sign-language-with-your-camera-2n5?ref=grantwinney.com)

[try it out](https://handsign-m4qq6.ondigitalocean.app/) | [deploy it](https://cloud.digitalocean.com/apps/new?repo=https://github.com/syauqy/handsign-tensorflow-gatsby/tree/master) | [source code](https://github.com/syauqy/handsign-tensorflow-gatsby)

## Rally Call

Most of us assume our cell phones will always just work, and why not? In the city, there's cell towers everywhere. But you don't have to get very far outside any city to find more rural areas where cell reception drops out, sometimes completely. That's usually no biggie though - everyone's got wi-fi and that's easy enough to jump on, right? In an emergency situation, though, cell and wi-fi might both be down... then what?

User [chrs strm](https://dev.to/chrsstrm) wrote an app that integrates with [Twilio](https://www.twilio.com/docs/voice/twiml) to allow groups of people to leave messages for each other in one central location. Granted, you'd have to remember the phone number and access code, but with that, any member of the team could update the others, from anywhere a number could be dialed. Messages are stored, so any team member can play them back at any time.

It's basic, but in a bad enough situation, it might be all that's needed. Better than sending smoke signals or carrier pigeons anyway. 😂

[Rally Call - An Emergency Comm Hub App in Just 14 Days - DEV Community](https://dev.to/chrsstrm/rally-call-an-emergency-comm-hub-app-in-just-14-days-4kli?ref=grantwinney.com)

[try it out](https://rallycall-lf9t4.ondigitalocean.app/) | [deploy it](https://cloud.digitalocean.com/apps/new?repo=https://github.com/chrsstrm/rallycall/tree/main) | [source code](https://github.com/chrsstrm/rallycall)

Although I was able to try these out with the demos each author posted, I wasn't able to deploy any of them to DigitalOcean, getting various errors during the deploy process. That was a little bit of a bummer, but writing apps like these in such a tight timeframe is really impressive... hopefully they'll all get a little polish in the future!

Aside from these three, there were a few other interesting submissions, for generating recipes from your leftovers, tracking car repairs, finding public-domain audiobooks, little games, etc. They're [all listed here](https://dev.to/t/dohackathon), in no semblance of any order whatsoever.. 😬