---
categories:
- C# 12
- C#
- Coding
- .NET 8
date: "2024-12-13T02:05:42Z"
description: ""
draft: false
cover:
  image: muhammad-ramdan-hwoIWT2mtvY-unsplash-1.jpg
slug: using-primary-constructors-with-classes-and-structs-in-csharp
summary: As part of C# 12, we got a new feature called primary constructors. Let's
  see how they work and what we can do with them.
tags:
- C# 12
- C#
- Coding
- .NET 8
title: Using Primary Constructors with Classes and Structs in C# 12 / .NET 8
---


The C# and .NET teams are always adding interesting, useful features, more frequently than ever since the norm has become annual releases. It's easy to lose track of all the changes though, and whenever I go back and dig around I usually find something interesting to try out.

One of the features we got as part of C# 12 is called primary constructors, which gives us a different way to define classes and structs. Let's take a closer look at what we can do with them and how they differ from traditional constructors.



The code in this post is available on GitHub, for you to use, extend, or just follow along while you read... and hopefully discover something new along the way!




Using Primary Constructors in Classes

Here's a standard class, representing a satellite. There's a constructor that sets some properties, only one of which (IsActive) can be changed after instantiation: (why? who knows, lol)

internal class Satellite
{
    public string Name { get; init; }
    public string Owner { get; init; }
    public DateOnly LaunchDate { get; init; }
    public bool IsActive { get; set; }

    public Satellite(string name, string owner, DateOnly launchDate, bool isActive)
    {
        Name = name;
        Owner = owner;
        LaunchDate = launchDate;
        IsActive = isActive;
    }
}

Writing the above code using a "primary constructor" instead (Visual Studio will helpfully do the conversion for us), we end up with something more compact, with the parameters relocated to the class signature line:

internal class Satellite(
    string name, string owner, DateOnly launchDate, bool isActive)
{
    public string Name { get; init; } = name;
    public string Owner { get; init; } = owner;
    public DateOnly LaunchDate { get; init; } = launchDate;
    public bool IsActive { get; set; } = isActive;
}

If we add the record modifier (something from a few versions before), then we can reduce the definition even more, since it causes read-only properties to be generated for us:

internal record class Satellite(
    string Name, string Owner, DateOnly LaunchDate, bool IsActive);

But since the original class allowed us to change the IsActive property, we'd actually have to override the default behavior by defining the property ourselves:

internal record class Satellite(
    string Name, string Owner, DateOnly LaunchDate, bool IsActive)
{
    public bool IsActive { get; set; } = IsActive;
}

Any of the above definitions allows us to intantiate the class like this, and then we can change the one property that allows it as needed:

var satellite =
    new Satellite("Navstar 82", "US", new DateOnly(2023, 1, 17), false);
satellite.IsActive = true;

Records are supposed to be immutable though, so allowing properties to be changed could confuse someone later on. I just felt like showing it off.


Using Primary Constructors in Structs

Structs are supposed to represent singular values, like primitive types or DateTime. Here's a struct that defines a "satellite position" value, which consists of a few things like longitude, latitude, etc.

internal struct SatellitePosition
{
    public string Latitude { get; set; }
    public string Longitude { get; set; }
    public decimal Altitude { get; init; }
    public DateTimeOffset Time { get; set; }

    public SatellitePosition(string latitude, string longitude, decimal altitude, DateTimeOffset time)
    {
        Latitude = latitude;
        Longitude = longitude;
        Altitude = altitude;
        Time = time;
    }
}

All of the properties can be changed except for the altitude (again, who knows why.. we'll just pretend it's some crazy requirement).

As with classes, we can migrate it to a primary constructor easily enough:

internal struct SatellitePosition(string latitude, string longitude, decimal altitude, DateTimeOffset time)
{
    public string Latitude { get; set; } = latitude;
    public string Longitude { get; set; } = longitude;
    public decimal Altitude { get; init; } = altitude;
    public DateTimeOffset Time { get; set; } = time;
}

And, again similar to classes, we can reduce things even further by using a record, if that fits our use case:

internal record struct SatellitePosition(
    string Latitude, string Longitude, decimal Altitude, DateTimeOffset Time);

It's worth mentioning there's some different behavior here though, than what we saw in the class. Whereas a record class generates readonly properties for us, a record struct generates properties that can be changed.

If we want to prevent an individual property from being changed, we can define it ourselves like this (or add readonly to the signature to make everything readonly):

internal record struct SatellitePosition(string Latitude, string Longitude, decimal Altitude, DateTimeOffset Time)
{
    public decimal Altitude { get; init; } = Altitude;
}

Then we can instantiate it as usual:

var satpos =
    new SatellitePosition1("44.3°N", "25.3°W", 20186, DateTimeOffset.UtcNow);
satpos.Latitude = "44.3°S";


Other Concerns

If we need to do something else when a class or struct is instantiated, it is possible to add a normal constructor in addition to the primary constructor, as long as the constructor calls the primary constructor and passes some default values:

internal struct SatellitePosition(
    string latitude, string longitude, decimal altitude, DateTimeOffset time)
{
    public string Latitude { get; set; } = latitude;
    public string Longitude { get; set; } = longitude;
    public decimal Altitude { get; init; } = altitude;
    public DateTimeOffset Time { get; set; } = time;

    public SatellitePosition() : this("", "", 0, DateTimeOffset.MinValue)
    {
        // other important stuff to do on instantiation
    }
}

Unless the default values make sense though, then just sticking with a normal constructor is probably the better way to go.


Learning More

To learn more about primary constructors, explore the official docs. Or to learn more about records, see my other article here:

Records, Classes and Equality in C# 9 / .NET 5The record modifier can define properties and equality in our classes for us, saving time and keeping our code cleaner. Let’s see how it works!Grant WinneyGrant

If you found this content useful, and would like to learn more about a variety of C# features, check out my CSharpDotNetFeatures repo, where you'll find links to plenty more blog posts and practical examples!

GitHub - grantwinney/CSharpDotNetExamples: Discovering and learning about the various features of the C# programming language and .NET Framework.Discovering and learning about the various features of the C# programming language and .NET Framework. - GitHub - grantwinney/CSharpDotNetExamples: Discovering and learning about the various featur…GitHubgrantwinney
