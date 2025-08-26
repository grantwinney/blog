---
categories:
- API
- Climate
date: "2018-01-23T04:58:58Z"
description: ""
draft: false
cover:
  image: https://images.unsplash.com/photo-1446776811953-b23d57bd21aa?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=2000&fit=max&ixid=eyJhcHBfaWQiOjExNzczfQ
slug: what-is-the-planet-os-api
summary: Planet OS seems to have found a great number of services that provide scientific/weather/earth
  related data, regularly download a small sample set from each, and provide a way
  to access the data through their own API. Let's see what they've got and what we
  can do with it!
tags:
- API
- Climate
title: Access weather and climate data with the Planet OS API
---


Someone once hit me up on Twitter to tell me about their API called Planet OS - or more accurately, an API for accessing other APIs.

Planet OS maintains a curated catalog of public and commercial datasets. For each of the datasets in our catalog, Planet OS provides a detailed summary page that contains key metadata attributes and a downsampled dataset preview.

[We provide] consistent programmatic access to high-quality weather, climate and environmental datasets from the world’s leading providers, making it easy to build data-driven applications and analyses.

They seem to have found a great number of services - such as NASA and NOAA - that provide scientific/weather/earth related data, regularly download a small sample set from each service, and provide a way to access that information through their own API. Neato. I think. There's a lot of data there, I only barely scratched the surface, and I'm not a meteorologist so I didn't understand a lot of what I was looking at.


Get an authentication key

You can use their service for free, for up to 500 requests a day and 5GB of data a month. Certainly enough to play around with since most of the data is returned as text that usually amounts to a few KB.

First, sign up for free, then check here for your auth key. If it worked, then you should see the second image instead of the first.


Trying it out

First, two things to consider:

 * If you're unfamiliar with APIs, you might want to read this first to familiarize yourself.
 * You may want to install Postman, which allows you to access API endpoints without having to write an app, as well as save the calls you make and sync them online between your computers.

As you browse the Planet OS REST API, they provide examples in several languages, which is really nice. If you're just testing things out, you can copy the cURL examples onto the command line or into Postman to test them out.

(Note that in all of the examples I post in here, I'm using https instead of http - and it works just fine. They support using ssl, yet many of their examples use http. If you use http, your API key will be sent in plain text and can be viewed by anyone monitoring your network.)

To make sure it works, either call the following from a command line or just paste it in your browser's address bar:

curl https://api.planetos.com/v1/datasets?apikey=<your_api_key>

Or (my preference) use Postman: (because I like to save my queries)


What else is there?

Although you can explore their datasets with other API calls, it looks far easier to use the GUI they've created named Datahub. You can specify exactly what kind of API you'd like to find. A fairly open-ended search for non-commercial data turned up a long list of interesting results.

Clicking "Open Data" for any of the above results brings up a more detailed page about the particular data set, including links to the organization, and filters for finding other interesting data. At this point though, I'm at a bit of a loss, and I've spent more time than I intended poking around trying to make heads or tails of the data.

There's a lot of acronyms and abbreviations thrown around that I'm unfamiliar with, but which seem to follow some scientific format. If you know more, or find a good use for this data, be sure to let me know below!

And if you have questions, they've setup a Planet OS slack channel.
