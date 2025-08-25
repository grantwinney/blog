+++
categories = ["API"]
date = 2017-12-16T04:58:47Z
description = ""
draft = false
image = "https://images.unsplash.com/photo-1517959184834-fed20bdba773?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=2000&fit=max&ixid=eyJhcHBfaWQiOjExNzczfQ"
slug = "what-is-twitter-api"
summary = "The Twitter API lets you access tweets, users who tweet, metadata, manipulate lists, and more. Let's check it out!"
tags = ["API"]
title = "Accessing Tweets and More With the Twitter API"

+++


Let's check out the Twitter API, how to use it and what it has to offer. You can start with their getting started guide if you like, but basically Twitter offers API calls that return various JSON payloads, or what they call tweet data dictionaries.

Their API allows you to access tweets (a basic message), users (metadata about the senders of tweets), entities (components of a message, like hashtags and urls), and "extended" entities (media, such as videos and pictures). You can do a lot more too, like manipulate lists, report users as spam, etc.

First though, two things to consider:

 * If you're unfamiliar with APIs, read this first to familiarize yourself with the concept.
 * Install Postman, which allows you to access API endpoints without having to write an app.


Authenticating

Before you can do anything, you'll need to authenticate. Twitter wants to know you're a valid (authorized) user before you start using their API. For example, I just used Postman to get some of my followers' IDs without authenticating. They don't allow it and they let me know in the response.

Go ahead and try it yourself. We'll fix it up pretty soon, so it'll actually work.


Create an Application

Normally, if you're accessing an API it's because you're trying to create your own application to do something with the results of those API calls. The first step in "authorizing" yourself is to tell Twitter a little about the application you'd like to make... of course, you don't have an actual app to make yet, so just fill in whatever bogus info you'd like!

After filling in the first three fields (in a real-world app, you'd need to specify a redirect URL too), and selecting that all-important developer agreement box, it should create your application and show a message at the top. Step one complete.


Generate an Access Token

You still need an access token, but if you flip to the "Keys and Access Tokens" tab you'll notice that there's a message that you don't have one yet.

Time to fix that. Click on "Create my access token" to generate a random token and token secret, similar to below. You'll need those values marked by red arrows in the next step.


Use the Access Token

Back in Postman where the previous API call failed, click the Authorization tab, choose OAuth 1.0, and enter the values indicated by the red arrows up above into Postman.

Try an API call again and it should work this time.


What else can it do?

Lots, as it turns out. There's an API reference that gives us a lot to try.

You could get the terms of service you skipped when you signed up for Twitter. If you do, would you give me the cliff's notes version?

You could use your coordinates to get a WOEID (find your lat/long here), and then use the WOEID to find trends for that location. I tried it out for Cleveland. (Oddly, nothing in the list of results, which is clearly for Cleveland, matches the "Cleveland trends" I see when I actually visit twitter.com. I dunno...)

GET https://api.twitter.com/1.1/trends/closest.json?lat=41.499320&long=-81.694361

[
    {
        "name": "Cleveland",
        "placeType": {
            "code": 7,
            "name": "Town"
        },
        "url": "http://where.yahooapis.com/v1/place/2381475",
        "parentid": 23424977,
        "country": "United States",
        "woeid": 2381475,
        "countryCode": "US"
    }
]

GET https://api.twitter.com/1.1/trends/place.json?id=2381475

[
    {
        "trends": [
            {
                "name": "Jeezy",
                "url": "http://twitter.com/search?q=Jeezy",
                "promoted_content": null,
                "query": "Jeezy",
                "tweet_volume": 42661
            },
            {
                "name": "Embiid",
                "url": "http://twitter.com/search?q=Embiid",
                "promoted_content": null,
                "query": "Embiid",
                "tweet_volume": 56065
            },
            {
                "name": "Roberson",
                "url": "http://twitter.com/search?q=Roberson",
                "promoted_content": null,
                "query": "Roberson",
                "tweet_volume": 15400
            },
            {
                "name": "#LivePD",
                "url": "http://twitter.com/search?q=%23LivePD",
                "promoted_content": null,
                "query": "%23LivePD",
                "tweet_volume": 16908
            },
            {
                "name": "Russ",
                "url": "http://twitter.com/search?q=Russ",
                "promoted_content": null,
                "query": "Russ",
                "tweet_volume": 38797
            },
            ...
        ],
        "as_of": "2017-12-16T04:17:41Z",
        "created_at": "2017-12-16T04:15:44Z",
        "locations": [
            {
                "name": "Cleveland",
                "woeid": 2381475
            }
        ]
    }
]

You're not limited to just requesting data either. There are plenty of API endpoints that allow you to create something too. For example, you could create a new list.

POST https://api.twitter.com/1.1/lists/create.json?name=Public Figures&description=Politics, celebrities, whatever...

{
    "id": 941910108359086080,
    "id_str": "941910108359086080",
    "name": "Public Figures",
    "uri": "/GrantWinney/lists/public-figures",
    "subscriber_count": 0,
    "member_count": 0,
    "mode": "public",
    "description": "Politics, celebrities, whatever...",
    "slug": "public-figures",
    "full_name": "@GrantWinney/public-figures",
    "created_at": "Sat Dec 16 05:57:24 +0000 2017",
    "following": false,
    "user": {
        ...
    }
}


If you'd like to develop an application in a particular language to take advantage of the API, Twitter has a list of libraries that'll get you started.


Notes

If you get an error like this one, even though some requests are making it through, double-check the request and parameters. While experimenting I'd sometimes get a more specific error, but I saw this one quite a few times when I'd added invalid query parameter names or invalid values for valid names.

{
    "errors": [
        {
            "code": 32,
            "message": "Could not authenticate you."
        }
    ]
}
