---
categories:
- API
date: "2019-03-03T14:14:00Z"
description: ""
draft: false
cover:
  image: photo-1504711434969-e33886168f5c.jpg
slug: searching-historical-newspapers-with-the-chronicling-america-api
summary: I've got a few old newspapers from the 1930s and 1940s, and find them fascinating.
  And so I was excited to stumble on the Library of Congress's Chronicling America
  API, which 'provides access to information about historic newspapers and select
  digitized newspaper pages'. Let's check it out.
tags:
- API
title: Searching historical newspapers with the Chronicling America API
---
Last summer I found an old newspaper at a garage sale, an edition of __The Johnstown Democrat__ from 1931. It was a lot of fun to show the kids, and to let them actually hold and touch a piece of history that's nearly 90 years old. I have a couple newspapers from the end of WWII as well... they're fascinating.

I was excited to stumble on the Library of Congress's [Chronicling America API](https://chroniclingamerica.loc.gov/about/api), which __"provides access to information about historic newspapers and select digitized newspaper pages"__. The result of an effort called the [National Digital Newspaper Program](http://www.loc.gov/ndnp/), it's completely free and unrestricted. You don't even need to register for a key, which many APIs use to throttle usage. It's not the same as holding it in your hand, but it's still really darn fascinating.

Before we dig deeper though, two things to consider:

- If you're new to APIs, check out "[What do we mean by API?](https://grantwinney.com/what-is-an-api/)" to learn more.
- You also may want to install [Postman](https://www.getpostman.com/), which lets you access API endpoints without having to write an app, plus you can save/sync everything to the cloud.

---

## How to Access

Since there's no key, it's extraordinarily easy to access... just make the call you're interested in. Using it however, proves to be a little more complicated.

## Find Newspapers by Title

This is a pretty big API - really, a set of APIs - and I'm only going to look at one of them. The sheer amount of data available is astounding. They provide bulk downloads of the OCR text they've generated from newspapers, and it must be terabytes of data.

So let's say you know the name of the paper you want. It was Bourbon News in Kentucky, from around the turn of the century. Search for the title and you'll get back a set of results like below. It's in JSON format using a standard called [OpenSearch](http://www.opensearch.org/Specifications/OpenSearch/Extensions/Suggestions/1.1), where the first line is your [search term](http://www.opensearch.org/Specifications/OpenSearch/Extensions/Suggestions/1.1#Suggestion_prefix), and the next 3 blocks represent lists of [suggested completions](http://www.opensearch.org/Specifications/OpenSearch/Extensions/Suggestions/1.1#Search_terms), [additional info](http://www.opensearch.org/Specifications/OpenSearch/Extensions/Suggestions/1.1#Descriptions), and [URLs to request each suggestion](http://www.opensearch.org/Specifications/OpenSearch/Extensions/Suggestions/1.1#Query_URLs), in that order. If you want to learn more about OpenSearch follow the links - I don't intend to cover it here.

```
GET http://chroniclingamerica.loc.gov/suggest/titles/?q=bourbon+news
```

```json
[
    "bourbon news",
    [
        "Bourbon news mirror. (Bourbon, Marshall County, Ind.) 1946-1947",
        "Bourbon news-mirror. (Bourbon, Ind.) 1971-current",
        "The Bourbon news. (Fort Scott, Kan.) 1921-1925",
        "The Bourbon news. (Bourbon, Ind.) 1893-1900",
        "The Bourbon news. (Millersburg, Ky.) 1881-1883",
        "The Bourbon news. (Paris, Ky.) 1895-19??"
    ],
    [
        "sn87056228",
        "sn87056230",
        "sn83040370",
        "sn84045464",
        "sn86069871",
        "sn86069873"
    ],
    [
        "http://chroniclingamerica.loc.gov/lccn/sn87056228/",
        "http://chroniclingamerica.loc.gov/lccn/sn87056230/",
        "http://chroniclingamerica.loc.gov/lccn/sn83040370/",
        "http://chroniclingamerica.loc.gov/lccn/sn84045464/",
        "http://chroniclingamerica.loc.gov/lccn/sn86069871/",
        "http://chroniclingamerica.loc.gov/lccn/sn86069873/"
    ]
]
```

One thing I noticed is that there's something funky about the search. Searching `bourbon+news+paris` returned no results, while searching `the+bourbon+news` still returned results without __"The"__ in them. Maybe articles are ignored and words in parenthesis aren't indexed? Not sure.

### Narrow results to a single publication

Assuming we're interested in the Paris KY newspaper, we want the last one above __(btw,__ [__lccn__](https://www.loc.gov/marc/lccn.html) __stands for Library of Congress Control Number)__. We can easily parse the JSON and display the result, or even build a link out of it:

https://jsfiddle.net/grantwinney/s1x082co/

### List all editions in a single publication

That's nice for getting the HTML representation, but what if we want to get a JSON representation of that page in order to dig deeper? Some resources take a query parameter like `?format=json` so I figured I could just append that to the url, but as it turns out you have to add `.json` to the URL in this case. Easy enough. Then you get JSON results for each edition. It's the same information reported by the nice little [calendar](https://chroniclingamerica.loc.gov/lccn/sn86069873/issues/) they generate on their site... in fact, with the data you get back you could do the same on your site.

```
GET http://chroniclingamerica.loc.gov/lccn/sn86069873.json
```

```json
{
  "place_of_publication": "Paris, Ky.", 
  "lccn": "sn86069873", 
  "start_year": "1895", 
  "place": [
    "Kentucky--Bourbon--Paris"
  ], 
  "name": "The Bourbon news.", 
  "publisher": "Champ & Miller", 
  "url": "http://chroniclingamerica.loc.gov/lccn/sn86069873.json", 
  "end_year": "19??", 
  "issues": [
    {
      "url": "http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-01/ed-1.json", 
      "date_issued": "1897-01-01"
    }, 
    {
      "url": "http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-05/ed-1.json", 
      "date_issued": "1897-01-05"
    }, 
    {
      "url": "http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1.json", 
      "date_issued": "1897-01-08"
    }
  ], 
  "subject": [
    "Bourbon County (Ky.)--Newspapers.", 
    "Kentucky--Bourbon County.--fast--(OCoLC)fst01216486", 
    "Kentucky--Paris.--fast--(OCoLC)fst01213382", 
    "Paris (Ky.)--Newspapers."
  ]
}
```

You can manipulate the results however you like. Maybe print out a summary of the returned results, along with a list of each edition that visitors can click on?

https://jsfiddle.net/grantwinney/hLg1cfvz/

### List all pages in a single edition

Requesting the third item in the list, part of the data we get back is a "pages" list that includes a URL for each page in the edition we requested. It looks like there's nine pages in this one.

```
GET http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1.json
```

```json
{
  "title": {
    "url": "http://chroniclingamerica.loc.gov/lccn/sn86069873.json", 
    "name": "The Bourbon news."
  }, 
  "url": "http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1.json", 
  "date_issued": "1897-01-08", 
  "number": "3", 
  "batch": {
    "url": "http://chroniclingamerica.loc.gov/batches/kyu_airplane_ver01.json", 
    "name": "kyu_airplane_ver01"
  }, 
  "volume": "17", 
  "edition": 1, 
  "pages": [
    {
      "url": "http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1/seq-1.json", 
      "sequence": 1
    }, 
    {
      "url": "http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1/seq-2.json", 
      "sequence": 2
    }, 
    ...
    ...
    {
      "url": "http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1/seq-8.json", 
      "sequence": 8
    }, 
    {
      "url": "http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1/seq-9.json", 
      "sequence": 9
    }
  ]
}
```

### Find an individual page

Let's check out the first page. We get the name and issue date, as well as links to a PDF image of the page and the OCR they generated from it.

```
https://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1/seq-1.json
```

```json
{
  "jp2": "http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1/seq-1.jp2", 
  "sequence": 1, 
  "text": "http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1/seq-1/ocr.txt", 
  "title": {
    "url": "http://chroniclingamerica.loc.gov/lccn/sn86069873.json", 
    "name": "The Bourbon news."
  }, 
  "pdf": "http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1/seq-1.pdf", 
  "ocr": "http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1/seq-1/ocr.xml", 
  "issue": {
    "url": "http://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1.json", 
    "date_issued": "1897-01-08"
  }
}
```

You could use that information to display the page or do something else with it, like embed the pdf in your own site. That might be a little trickier than it sounds, but the point is that the data is there if you've got an idea of what you'd like to do with it.

```javascript
let elem = document.getElementById('link');

fetch('https://chroniclingamerica.loc.gov/lccn/sn86069873/1897-01-08/ed-1/seq-1.json')
.then(data => {return data.json()})
.then(res => {
    let pdf = new URL(res.pdf);
    pdf.protocol = 'https:'
    elem.innerHTML = `<p><embed src="${pdf}" type="application/pdf" height="300px" /></p>`;
})
.catch(error => elem.innerHTML = error)
```

## Other Stuff of Interest

If that wasn't enough, here's a few other random links for your leisure. :)

- [Search Newspaper Images](https://chroniclingamerica.loc.gov/search/pages/results/) __(Library of Congress)__
- [Virginia Gazette](http://research.history.org/DigitalLibrary/va-gazettes/VGAllIssues.cfm), the official newspaper of Virginia, printed in Williamsburg from 1736 until 1780 __(Colonial Williamsburg)__
- [Batches of data](https://chroniclingamerica.loc.gov/batches/) and [raw OCR content](https://chroniclingamerica.loc.gov/ocr/) __(Library of Congress)__
- [Other related APIs](https://labs.loc.gov/lc-for-robots) __(Library of Congress)__
- [API for accessing historic programs of publicly funded radio and television](https://github.com/WGBH/AAPB2#api) __(American Archive of Public Broadcasting)__