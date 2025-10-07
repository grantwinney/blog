---
categories:
  - Explore
date: 2019-02-12T11:47:00Z
description: ""
draft: false
postimage: /banners/generic-explore-banner.webp
slug: using-the-ip-geolocation-api-to-find-info-about-an-ip-address
summary: Last year I caught an article about a simple, free service called ipify that returns your IP address. It became so popular the author soon found himself dealing with billions of requests per month! Here's a look at that API and the IP Geolocation API that it spawned.
tags:
  - geolocation
title: Find info about an IP address with the IP Geolocation API
---
I read an article awhile back, called [To 30 Billion and Beyond](https://dev.to/rdegges/to-30-billion-and-beyond-3f94), about a simple to use and completely free service called [ipify](https://www.ipify.org/) _(_[_source code_](https://github.com/rdegges/ipify-api)_),_ which returns your current IP address in a few different formats. Randall Degges wrote it because he needed it personally, but it became so popular he soon found himself having to deal with _tens of billions_ of requests per month!

The spinoff from that was a separate [IP Geolocation API](https://geoipify.whoisxmlapi.com/) that can tell you all kinds of information about an IP address once you have it. That's what I want to look at today, although I'll show an example of ipify too.

Before we dig deeper though, a few things to consider:

- If you're new to APIs, here's a brief intro: [What is an API?](https://grantwinney.com/what-is-an-api/)
- You also may want to install [Postman](https://www.getpostman.com/), which lets you access endpoints without having to write an app, plus it saves/syncs everything to the cloud.
- Afterwards, check out some [other APIs I've written about](https://grantwinney.com/tags/api/).

---

## ipify

There's not much else to say about it. Randall Degges, personal project, 30 billion requests, the road to hell and all that. ;) It's amazing he was (and presumably still is) paying for it out of pocket every month. Quite generous.

```none
GET https://api.ipify.org
  > 123.456.123.456

GET https://api.ipify.org?format=json
  > { "ip": "123.456.123.456" }

GET https://api.ipify.org?format=jsonp&callback=process_reply  
  > process_reply({"ip":"71.74.96.6"});
```

He provides tons of examples in different languages, and links to libraries written for different languages and frameworks too. If you wanted to try it out in C#, for example, it's as simple as a half-dozen lines of code.

```csharp
using (var httpClient = new HttpClient())
{
    var ip_task = httpClient.GetStringAsync("https://api.ipify.org");
    ip_task.Wait();
    Console.WriteLine($"My public IP address is: {ip_task.Result}");
}

Console.ReadLine();
```

I could see one obvious application, where you only have a dynamic IP at your house, but you want to run a web server that's available externally. Maybe something for personal use, like security cameras setup around your property. Normally you'd need a static IP, but if you ran a small service that checked for your IP once a minute and then notified you... hmm...

---

## Getting Started

Whois API provides a free tier that provides 1000 requests per month _(to this particular API - they have_ [_other APIs_](https://user.whoisxmlapi.com/products) _with their own limits),_ so just [sign up](https://geoipify.whoisxmlapi.com/signup). Right away, they provide you with a sample query using Google's well-known name server and your personal API key. It's a great way to get started quickly.

### What's Google's geolocation?

```
https://geoipify.whoisxmlapi.com/api/v1?ipAddress=8.8.8.8&apiKey=<your_key>
```

Unless something's gone drastically wrong, running this in Postman should produce a result like this. If that's the case, your network is up, Google is up, and your API key works. :)

```json
{
    "ip": "8.8.8.8",
    "location": {
        "country": "US",
        "region": "California",
        "city": "Mountain View",
        "lat": 37.40599,
        "lng": -122.078514,
        "postalCode": "94043",
        "timezone": "-08:00"
    },
    "as": {
        "asn": 15169,
        "name": "GOOGLE",
        "route": "8.8.8.0/24",
        "domain": ""
    }
}
```

### What's _your_ geolocation?

Just leave the IP address off the request, and it uses yours! That could be useful, say if you want to run a server from your house that's available externally, but you have a

```none
https://geoipify.whoisxmlapi.com/api/v1?apiKey=<your_key>
```

### What's your blog's geolocation?

You can search for a website's geolocation info too, just by specifying the domain name.

```none
https://geoipify.whoisxmlapi.com/api/v1?domain=grantwinney.com&apiKey=<your_key>
```

The response is most likely wherever my server is running.

```json
{
    "ip": "45.55.81.77",
    "location": {
        "country": "US",
        "region": "New Jersey",
        "city": "Clifton",
        "lat": 40.8344,
        "lng": -74.1377,
        "postalCode": "07014",
        "timezone": "America/New_York"
    },
    "domains": [
        "grantwinney.com",
        "www.grantwinney.com"
    ]
}
```

---

## What next?

So, this was a short one. Their API is ridiculously easy to use in comparison to some other APIs I've tried out. Signup was easy, the "free" tier of usage is obvious and well documented, and the site is clean and easy to read. Awesome!

They have a bunch of other APIs to check out too if you're interested. Maybe I'll cover one or more of them in the future...

- WHOIS API: [https://whoisapi.whoisxmlapi.com](https://whoisapi.whoisxmlapi.com)
- Email Verification API: [https://emailverification.whoisxmlapi.com](https://emailverification.whoisxmlapi.com)
- IP Geolocation API: [https://geoipify.whoisxmlapi.com](https://geoipify.whoisxmlapi.com)
- Reverse IP API: [https://reverse-ip-api.whoisxmlapi.com](https://reverse-ip-api.whoisxmlapi.com)
- Reverse MX API: [https://reverse-mx-api.whoisxmlapi.com](https://reverse-mx-api.whoisxmlapi.com)
- Reverse NS API: [https://reverse-ns-api.whoisxmlapi.com](https://reverse-ns-api.whoisxmlapi.com)
- Other APIs: [https://whoisxmlapi.com](https://whoisxmlapi.com)
