---
categories:
- API
date: "2018-06-24T20:02:24Z"
description: ""
draft: false
cover:
  image: photo-1550745165-9bc0b252726f.jpg
  relative: true
slug: access-yesterdays-internet-with-the-wayback-machine-apis
summary: The Wayback Machine, a product of the Internet Archive, is an ambitious tool
  that's been documenting websites for many years. It's useful when a page you need
  is removed by the original author. Let's take a look at their API and how we might
  make use of it.
tags:
- API
title: Access yesterday's Internet with the Wayback Machine API
---
Today I'm checking out the Wayback Machine APIs from the Internet Archive. If you haven't heard of the IA before, it's a site that's aiming to... well.. archive everything. They're probably best known for helping people find [archived versions of deleted pages](https://web.archive.org/), but they have a lot of [old software and games](https://archive.org/details/software), [books](https://openlibrary.org/), and [much more](https://archive.org/projects/). Unfortunately, they only have a [demo version](https://archive.org/details/F19StealthFighter_1020) of F-19 stealth fighter, the first game I ever played... well, other than [Oregon Trail](https://archive.org/details/msdos_Oregon_Trail_The_1990). And [Number Munchers](https://archive.org/details/msdos_Number_Munchers_1990)!!

It sure is convenient that all the games of my... um... youth are on a uh... archival website. 😢

Okay, let's see what we can do! But first, two things before you get started:

- If you're new to this, consider reading "[What do we mean by API?](https://grantwinney.com/what-is-an-api/)" to familiarize yourself.
- You also may want to install [Postman](https://www.getpostman.com/), which lets you access API endpoints without having to write an app, plus you can save/sync everything to the cloud.

---

## Request a webpage

When I started writing this I really thought there would be more to their API, but there's just [one call with a couple parameters](https://archive.org/help/wayback_api.php), so... we'll look at it really closely. 😁 In fact, the Internet Archive has [a number of projects](https://archive.org/projects/) which may each have their own APIs and warrant a separate look at some future point (like the [Open Library](https://openlibrary.org/dev/docs/api/books)).

There's no authentication necessary, and no posted rate limits, so to get the latest cached version of a page you just pass in the URL. Here's a post I wrote years ago, but have since deleted:

[http://archive.org/wayback/available?url=grantwinney.com/how-to-make-security-essentials-ignore-directories/](http://archive.org/wayback/available?url=grantwinney.com/how-to-make-security-essentials-ignore-directories/)

If there's anything available, you'll get the latest snapshot:

```json
{
    "url": "grantwinney.com/how-to-make-security-essentials-ignore-directories/",
    "archived_snapshots": {
        "closest": {
            "status": "200",
            "available": true,
            "url": "http://web.archive.org/web/20140808005655/http://www.grantwinney.com:80/how-to-make-security-essentials-ignore-directories/",
            "timestamp": "20140808005655"
        }
    }
}
```

### Getting an older version

If you want an older version, you can pass in a timestamp. It'd be nice if all you had to do was subtract one second from the previous timestamp to get the next oldest one, but it seems to do a _"closest to the timestamp"_ match, so the timestamp you specify has to be _closer_ to the older version than the newer one. It's in `YYYYMMDDhhmmss` format, so here I'll subtract 1 month and try again:

[https://archive.org/wayback/available?url=grantwinney.com/how-to-make-security-essentials-ignore-directories/&timestamp=20140708005654](https://archive.org/wayback/available?url=grantwinney.com/how-to-make-security-essentials-ignore-directories/&timestamp=20140708005654)

And the response that comes back is a slightly older version from 2 months previous:

```json
{
    "url": "grantwinney.com/how-to-make-security-essentials-ignore-directories/",
    "timestamp": "20140708005654",
    "archived_snapshots": {
        "closest": {
            "status": "200",
            "available": true,
            "url": "http://web.archive.org/web/20140607150143/http://www.grantwinney.com:80/how-to-make-security-essentials-ignore-directories/",
            "timestamp": "20140607150143"
        }
    }
}
```

### What if nothing's available?

If there's nothing available for a page, you'll just get an empty object:

```json
{
    "url": "grantwinney.com/how-to-make-security-essentials-ignore-directories/asdf",
    "archived_snapshots": {}
}
```

---

## JSONP Callbacks

There's one other parameter besides `timestamp`, and that's `callback` which produces a JSONP response.

To see the difference, try requesting the resource in any of the major browsers...

```javascript
var Httpreq = new XMLHttpRequest();
let apiUrl = 'https://archive.org/wayback/available';
let reqUrl = 'grantwinney.com/how-to-make-security-essentials-ignore-directories/';
Httpreq.onreadystatechange = function() {
    if (Httpreq.readyState == XMLHttpRequest.DONE) {
        alert(JSON.parse(Httpreq.responseText));
    }
}
Httpreq.open("GET",`${apiUrl}?url=${reqUrl}`, false);
Httpreq.send(null);
```

_(or alternatively using jQuery)_

```javascript
let apiUrl = 'https://archive.org/wayback/available';
let reqUrl = 'grantwinney.com/how-to-make-security-essentials-ignore-directories';
$.getJSON(`${apiUrl}?url=${reqUrl}`);
```

... and you're likely to get an error similar to this one:

> Failed to load [https://archive.org/wayback/available?url=grantwinney.com/how-to-make-security-essentials-ignore-directories/:](https://archive.org/wayback/available?url=grantwinney.com/how-to-make-security-essentials-ignore-directories/:) No 'Access-Control-Allow-Origin' header is present on the requested resource. Origin '[https://fiddle.jshell.net](https://fiddle.jshell.net)' is therefore not allowed access.  
>   
> Uncaught DOMException: Failed to execute 'send' on 'XMLHttpRequest': Failed to load '[https://archive.org/wayback/available?url=grantwinney.com/how-to-make-security-essentials-ignore-directories/](https://archive.org/wayback/available?url=grantwinney.com/how-to-make-security-essentials-ignore-directories/)'. at XMLHttpRequest.send (\<anonymous>:1:781)

This is new territory for me, but from what I understand it's a security measure in most browsers, that prevents scripts from requesting resources from domains that are different than the one they're running in. It helps prevent malicious scripts from doing Bad Things with your data. Here's a couple sites on [using JSONP for cross domain requests](https://www.getfilecloud.com/blog/using-jsonp-for-cross-domain-requests/) and [using JSONP with jQuery](https://www.sitepoint.com/jsonp-examples/).

While [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) appears to be the newer/better way to make cross-domain requests, there's also JSONP. In either case the server has to support it, and the Wayback Machine supports JSONP, so let's see what that request looks like.

_Using an inline function:_

```javascript
let apiUrl = 'https://archive.org/wayback/available';
let reqUrl = 'grantwinney.com/how-to-make-security-essentials-ignore-directories';
$.getJSON(`${apiUrl}?url=${reqUrl}&callback=?`, function(json){
  console.dir(json);
});  // inline function
```

_Or a separate function, if that's more your style:_

```javascript
function handleData(json) {
  console.dir(json);
}

let apiUrl = 'https://archive.org/wayback/available';
let reqUrl = 'grantwinney.com/how-to-make-security-essentials-ignore-directories';
$.getJSON(`${apiUrl}?url=${reqUrl}&callback=?`, handleData);  // separate function
```

In either case, you'll get a response like this in the console:

```
Object
  archived_snapshots:
    closest:
      available:true
      status:"200"
      timestamp:"20140808005655"
      url:"http://web.archive.org/web/20140808005655/http://www.grantwinney.com:80/how-to-make-security-essentials-ignore-directories/"
  url: "grantwinney.com/how-to-make-security-essentials-ignore-directories"
```

With a little adjustment, we can get just the archived URL:

```javascript
let apiUrl = 'https://archive.org/wayback/available';
let reqUrl = 'grantwinney.com/how-to-make-security-essentials-ignore-directories';
$.getJSON(`${apiUrl}?url=${reqUrl}&callback=?`, function(json){
  let snapshot = json['archived_snapshots'];
  if (Object.keys(snapshot).length !== 0) {
  	console.dir(snapshot.closest.url);
    // prints http://web.archive.org/web/20140808005655/http://www.grantwinney.com:80/how-to-make-security-essentials-ignore-directories/
  }
});
```

## Wayback Machine as a browser extension

You can see how this could be implemented as a browser extension fairly easily. If you request a resource and get a 404, check the Wayback Machine and automatically redirect to the cached version if there is one.

In fact, there's a Chrome extension called [Wayback Machine](https://chrome.google.com/webstore/detail/wayback-machine/fpnmgdkabkmnadcjpehmlllkndpkmiak) that does just that, and it's actually from the Internet Archive! How nice.

![wayback-machine-extension](https://grantwinney.com/content/images/2018/06/wayback-machine-extension.png)

If you're curious what it does, check it out using the awesome [Chrome extension source viewer](https://chrome.google.com/webstore/detail/chrome-extension-source-v/jifpbeccnghkjeaalbbjmodiffmgedin) extension, or check out the main crux of it below.

It adds a listener, so that if any page you visit returns a 404 or any one of a handful of other codes indicating "failure", and assuming you're not in "incognito" mode _(nice),_ it'll go out and check the Wayback Machine for the most recent copy available. If it finds one, it'll popup a banner like the one above, offering to load it. So if you see the banner, an archived copy definitely exists. Clicking the link in the banner loads the archived page.

```javascript
/*
 * LICENSE: AGPL-3
 * Copyright 2016, Internet Archive
 */
chrome.webRequest.onCompleted.addListener(function(details) {
    function tabIsReady(isIncognito) {
        var httpFailCodes = [404, 408, 410, 451, 500, 502, 503, 504,
            509, 520, 521, 523, 524, 525, 526
        ];
        if (isIncognito === false &&
            details.frameId === 0 &&
            httpFailCodes.indexOf(details.statusCode) >= 0 &&
            isValidUrl(details.url)) {
            Globalstatuscode = details.statusCode;
            wmAvailabilityCheck(details.url, function(wayback_url, url) {
                if (details.statusCode == 504) {
                    //notify(wayback_url,'View an archived version courtesy of the Internet Archive WayBack Machine');
                    chrome.notifications.create(
                        'wayback-notification', {
                            type: 'basic',
                            requireInteraction: true,
                            iconUrl: '/images/logo.gif',
                            title: "Page not available ?",
                            message: "View an archived version courtesy of the WayBack Machine",
                            buttons: [{
                                title: "Click here to see archived version"
                            }]
                        },
                        function(id) {
                            myNotID = id;
                        }
                    );
                    chrome.notifications.onButtonClicked.addListener(function(notifId, btnIdx) {
                        if (notifId === myNotID) {
                            if (btnIdx === 0) {
                                chrome.tabs.create({
                                    url: wayback_url
                                });
                                chrome.notifications.clear(myNotID);
                                myNotID = null;
                            }
                        }
                    });
                } else {
                    chrome.tabs.executeScript(details.tabId, {
                        file: "scripts/client.js"
                    }, function() {
                        chrome.tabs.sendMessage(details.tabId, {
                            type: "SHOW_BANNER",
                            wayback_url: wayback_url
                        });
                    });
                }
            }, function() {
            });
        }
    }
    if (details.tabId > 0) {
        chrome.tabs.get(details.tabId, function(tab) {
            tabIsReady(tab.incognito);
        });
    }
}, {
    urls: ["<all_urls>"],
    types: ["main_frame"]
});

/*
 * Checks Wayback Machine API for url snapshot
 */
function wmAvailabilityCheck(url, onsuccess, onfail) {
    var xhr = new XMLHttpRequest();
    var requestUrl = "https://archive.org/wayback/available";
    var requestParams = "url=" + encodeURI(url);
    xhr.open("POST", requestUrl, true);
    xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xhr.setRequestHeader("Wayback-Api-Version", 2);
    xhr.onload = function() {
        var response = JSON.parse(xhr.responseText);
        var wayback_url = getWaybackUrlFromResponse(response);
        if (wayback_url !== null) {
            onsuccess(wayback_url, url);
        } else if (onfail) {
            onfail();
        }
    };
    xhr.send(requestParams);
}
```

---

## A Final Note

It's an ambitious project, but as a minimalist I just don't get the value of archiving _everything_. The Library of Congress has some [amazing collections](https://www.loc.gov/collections/) too, like notes from [Abraham Lincoln](https://www.loc.gov/collections/abraham-lincoln-papers/about-this-collection/) and [Alexander Hamilton](https://www.loc.gov/collections/alexander-hamilton-papers/about-this-collection/), but it's sensibly curated. LoC even had access to the Twitter floodgates but decided [most of it isn't worth collecting](https://blogs.loc.gov/loc/files/2017/12/2017dec_twitter_white-paper.pdf) - but I'd bet IA would love access to it all. In fact, I seem to remember a tweet from Brewster Kahle (IA founder) that appeared to call out Twitter for not opening their floodgates.

I also question the legality of what's been collected, which makes me hesitant to actually download software and books from their site. Is all that software really available to download, or is it basically piracy? What about the digitized books? What about [websites who didn't want to be archived](https://lauren.vortex.com/2017/04/23/more-regarding-a-terrible-decision-by-the-internet-archive)? In their drive to archive "all the things", I hope they're diligent about the rights and preferences of others. Maybe these issues are already addressed somewhere on their site - if you know where, please share links. Thanks!

If you're interested in learning more about APIs, check out my comparison of [the many ways APIs can authorize access](https://grantwinney.com/a-look-at-the-many-ways-apis-can-authorize-access/). And if you've ever thought about writing an API wrapper in the language of your choice, [you might find this helpful](https://grantwinney.com/what-is-an-api-wrapper-and-how-do-i-write-one/).
