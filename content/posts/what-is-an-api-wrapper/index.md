---
categories:
- Questions
date: "2018-01-25T04:59:32Z"
description: ""
draft: false
cover:
  image: RpgvvtYAQeqAIs1knERU_vegetables.jpg.jpg
  relative: true
slug: what-is-an-api-wrapper
summary: When you find an API to use in your app, you'll need to access it in a specific
  language - not always an easy or straightforward task. As long as you're doing all
  that work, why keep it to yourself? Let's look at creating an API wrapper that you
  can share with others!
tags:
- Questions
title: What is an API wrapper?
---
Forget about peeling back the layers - today we're gonna talk about _adding_ layers.

When you find an API that looks interesting, you'll naturally want to try it out. I've [tested and written quite a bit about APIs](https://grantwinney.com/tag/api/), and most of the time I start off with a tool like [Postman](https://www.getpostman.com/). That's fine for playing around, but eventually you'll want to use it in your app.

APIs come in all shapes and sizes. Some are dead simple; others are amazingly complex - even overly complicated at times. It takes time to implement it in a language - to figure out the right way to access _any_ REST endpoint, then to figure out the right way to access a _specific_ endpoint and get the data you're interested in. As long as you're doing all that work, why keep it to yourself?

You can share the work you've done - maybe elaborating to cover all of an API's endpoints, or maybe letting others make pull requests to fill in the gaps. Hopefully the language you're using has some concept of a library, package, or some other way to bundle code up for easy sharing, but that's not necessarily necessary.

## API Wrapper in Python

Several weeks ago, I wrote a few one-off Python scripts to demo accessing the [ISS Notify API](https://grantwinney.com/what-is-iss-notify-api/). I won't repost them here, but go check them out before looking at how we can refactor them. Notice how much code is duplicated between them.

What if we wanted to make another call to the same endpoint? Or a call to a closely related endpoint? Or 10 more calls with different parameters? And what if someone else wanted to make the same calls as you but wasn't sure where to start? Maybe we can help them out.

What I call an API wrapper is really quite simple - just some nice, clean functions to access the API, published somewhere accessible like GitHub. Tada.

Here's two of the Python scripts from the other post, refactored into a single file that I named `iss_api_wrapper.py`.

```python
import urllib2
import json
import datetime

def _call_api(endpoint):
    request = urllib2.Request('http://api.open-notify.org%s' %(endpoint))
    response = urllib2.urlopen(request)
    return json.loads(response.read())

def show_roster():
    result = _call_api('/astros.json')
    print "There are %d people in space:" % (result['number']),
    for i in range(result['number']):
        print result['people'][i]['name'] + ",",

def show_next_pass(latitude, longitude):
    result = _call_api('/iss-pass.json?lat=%s&lon=%s' %(latitude, longitude))
    print('The next ISS pass for %s %s is %s for %s seconds'
          %(result['request']['latitude'],
            result['request']['longitude'],
            datetime.datetime.fromtimestamp(result['response'][0]['risetime']),
            result['response'][0]['duration']))

```

Place the file in a directory called `iss` along with an empty file named `_init_.py`. Create another file _outside_ the directory called `use_iss.py` (or whatever you want) and call the functions in your new module: _(or just_ [_download the scripts from GitHub_](https://github.com/grantwinney/BlogCodeSamples/tree/master/APIs/IssNotifyApiWrapper/Python)_)_

```python
import iss.iss_api_wrapper as iss

iss.show_roster()
print "\n"
iss.show_next_pass('41.4984174', '-81.6937287')
```

Output:

```none
There are 6 people in space: Alexander Misurkin, Mark Vande Hei, Joe Acaba, Anton Shkaplerov, Scott Tingle, Norishige Kanai,

The next ISS pass for 41.4984174 -81.6937287 is 2018-01-24 17:37:00 for 254 seconds
```

Now you can share your nice module / API wrapper with the world. If the API endpoints change in the future, you can change the calls being made in your Python code, but anyone using your module will be none the wiser!

## API Wrapper in C#

Let's try the same thing one more time, in C# this time.

I wrapped all three examples from the [ISS Notify API](https://grantwinney.com/day-11-iss-notify-api/) post. This code depends on the [RestSharp NuGet package](https://www.nuget.org/packages/RestSharp/) _(just discovered it; made accessing the endpoint simple)_ and some classes I had to defined but didn't want to paste below - you can [find everything on GitHub](https://github.com/grantwinney/BlogCodeSamples/tree/master/APIs/IssNotifyApiWrapper/CSharp). You can [see the original JSON output from the API](https://grantwinney.com/day-11-iss-notify-api/) here.

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using RestSharp;

namespace ISS_Notify_Wrapper
{
    public static class ISS
    {
        private static T GetResource<T>(string description, string resource, Tuple<string,string>[] parameters = null) where T : new()
        {
            var client = new RestClient { BaseUrl = new Uri("http://api.open-notify.org") };

            var request = new RestRequest(resource, Method.GET);

            if (parameters != null)
                foreach (var param in parameters)
                    request.AddParameter(param.Item1, param.Item2);

            var response = client.Execute<T>(request);
            var content = response.Content;

            if (response.ErrorException != null)
                throw new ApplicationException($"Unable to retrieve {description}.", response.ErrorException);

            return response.Data;
        }

        public static void ShowRoster()
        {
            var roster = GetResource<Roster>("roster", "astros.json");
            var astronautNames = String.Join(", ", roster.People.Select(x => x.Name));
          
            Console.WriteLine($"There are {roster.Number} people in space: {astronautNames}");
        }

        public static void ShowUpcomingPasses(string latitude, string longitude)
        {
            var nextPass = GetResource<Passes>("next pass", "iss-pass.json",
                new[] { Tuple.Create("lat", latitude), Tuple.Create("lon", longitude) }).Response[0];

            Console.WriteLine($"The next ISS pass for {latitude} {longitude} is " +
                              $"{DateTimeOffset.FromUnixTimeSeconds(nextPass.Risetime)} " +
                              $"for {nextPass.Duration} seconds.");
        }

        public static void ShowCurrentLocation()
        {
            var pos = GetResource<Position>("next pass", "iss-now.json").IssPosition;

            Console.WriteLine($"The current position is: {pos.Latitude} {pos.Longitude}");
        }
    }
}
```

And now you've got a nice wrapper that anyone can download and use, without them having to know exactly how the ISS Notify API looks, or rethink the same logic you already spent time coding.

```csharp
ISS.ShowRoster();
ISS.ShowUpcomingPasses("41.4984174", "-81.6937287");
ISS.ShowCurrentLocation();
```

Output:

```none
There are 6 people in space: Alexander Misurkin, Mark Vande Hei, Joe Acaba, Anton Shkaplerov, Scott Tingle, Norishige Kanai
The next ISS pass for 41.4984174 -81.6937287 is 1/25/2018 3:23:45 AM +00:00 for 563 seconds.
The current position is: -47.0603 147.0037

Press any key to continue...
```

## Learning More...

If you want to check out another example, I wrote [GhostSharp](https://grantwinney.com/ghostsharp/) as a C# wrapper around the API that's built into the Ghost engine that this blog runs on.

What do you think? Are you working on an API wrapper, or thinking about it? I'd love to check it out - let me know or just share your thoughts below!
