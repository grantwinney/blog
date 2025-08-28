---
categories:
- API
- Questions
date: "2017-07-23T19:41:19Z"
description: ""
draft: false
cover:
  image: finger-769300_1920.jpg
slug: what-is-an-api
summary: An API is an Application Programming Interface, but what's that really mean?
  In a more practical sense, it's one programmer hiding the (possibly messy) details
  of their own code behind a nice veneer, in order to make it easier for another programmer
  to consume it in their own program.
tags:
- API
- Questions
title: What is an API?
---
To define it, an API is an Application Programming Interface, but what's that really mean? In a more practical sense, it's one programmer hiding the (possibly messy) details of their own code behind a nice veneer, in order to make it easier for another programmer to consume it in their own program. 😉

## First, what's an interface?

Before we talk about programming, let's consider what an interface is in the most general sense. We're _surrounded_ by interfaces and may not even realize it most of the time. In fact, the _better_ the interface, the less aware we are that it exists! Whenever something complex is abstracted away for us, hiding the complex details behind a little display screen or a simple button push, someone took the time to create an interface for us.

The dashboard in a car is one example. We don't have to count the number of times the crankshaft in the engine turns - just glance at the RPM gauge. We don't have to manually inspect the level of gas in the tank, or read the output of the low-fuel sensor - we get a nice readout on the dash and a dedicated indicator light. The dashboard is an interface, abstracting away the inner-workings of dozens of sensors and components all over the car.

A kitchen is another example. From toasters to microwaves to coffee machines, all kinds of devices take only a button push or two to set off a complex chain of internal motion that we don't need to worry about.

## What's that have to do with programming?

Programming languages have their own interfaces that provide layers of abstraction too. Code is organized into classes and modules, and then select pieces of the code are marked as "public" or "exported" to let you know those are the ones that are safe to use.

Imagine we have a simple `Person` class, and a `PersonReport` class that in effect answers questions about a collection of people, like _"who's an adult?"_ or _"who's a parent?"_.

```csharp
public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public List<Person> Children { get; set; }
}

public class PersonReport
{
    public List<Person> GetAdults(List<Person> persons)
    {
        return persons.Where(x => x.Age >= 18).ToList();
    }

    public List<Person> GetParents(List<Person> persons)
    {
        return persons.Where(x => x.Children.Count > 0).ToList();
    }
}
```

Similar to the dashboard in our car, we don't have to worry about the details underneath. Their abstracted away behind a clean interface. In this case the "messy details" is a simple one-liner of LINQ, but it could be many lines calling other methods and other classes. The point is, we just ask for parents and get the results.

## So.. what _is_ an API then?

Well, the above... and more. Let's consider some code written by someone else (maybe another team in our company, or even a different company) and provided to us as a framework or library. Nearly every language has an existing framework. These are the building-blocks of the language, as well as thousands or even millions of lines of code that prevent you and I from having to reinvent the same wheel over and over.

- In C#, it's the [.NET Framework](https://msdn.microsoft.com/en-us/library/ff361664\(v=vs.110\).aspx) developed by Microsoft
- In Erlang, it's the [OTP](https://github.com/erlang/otp/tree/master/lib/stdlib/src) libraries from Ericsson
- Java has its [base libraries](https://docs.oracle.com/javase/8/docs/technotes/guides/#base) and a bunch of other frameworks, and on and on...

In the sample of code above, even the call to `persons.Where(x => x.Children.Count > 0).ToList();` involved an API call, in the sense that I don't have to know or care _how_ the LINQ library manages to filter my list of persons by a particular attribute. I just specify the condition on which to filter, and call the `Where()` function to do its magic.

## API calls in the cloud, and the REST interface

So far I've been talking about frameworks and libraries which are normally copied to locally and compiled or interpreted with the rest of the app. But what if the code is running on someone else's computer instead of ours, and we can't copy it? It could be running on one server in a basement somewhere or in a state-of-the-art facility – if we don't have access to it, what do we do?

That's what most people mean by APIs. There are thousands of services all over the world, written in any one (or a combination) of a hundred different languages, and the problem is how to integration it using the language of your choice? There are a number of ways, but the most popular right now is the REST interface.

But let's back up again for a minute.

Any time we access a page in a web browser, the browser sends a "GET" message to the page's url like (i.e. "[http://www.example.com](http://www.example.com)"). In return, some web server somewhere in the world returns a block of html markup representing that page to you. Your browser uses that and says "okay, there's a table tag so let's add a table, and a blockquote tag so let's format that however it's supposed to look, and oh! an image tag". It renders the page and does more "GET" operations to get all the images and stylesheets and so on and so forth.

If we're visit a page where we're updating our profile information, the browser might send a "POST" operation to the website, so some web server somewhere in the world can update our data. And if we choose to delete our profile, well then it might send a "DELETE" operation to the website, so it can delete our data from the database.

If you want to see something kind of neat, download [Postman](https://www.getpostman.com/) and try performing a "GET" on some websites. You'll see the markup your browser gets when it requests a site, and you can even use this tool to send "POST" and "DELETE" requests to sites too, if you know of a URL that expects them.

![](https://grantwinney.com/content/images/2024/08/image.png)

Performing a GET with Postman

Just like the browser issues REST commands to get, update, and delete data from around the world, our own apps can too. We can do a "GET" request to a certain endpoint URL such as "[http://www.example.com/users/1234](http://www.example.com/users/1234)", and some server somewhere in the world can do whatever it does and return the data for user 1234 to us. The difference is that instead of returning HTML markup like with the browser, it's likely to return your data in JSON format. Going anymore deeply into REST and JSON will have to be saved for another post though.. this one's already long enough as-is.

So all that to say, if someone asks you what an API is...

An API is some code running on someone else's machine _(where? how? who cares!),_ which they've made accessible to you. And the _way_ you typically access it is using REST, very similar to how your web browser does it.