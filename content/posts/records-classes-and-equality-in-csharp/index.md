---
categories:
- C# 9
- C#
- Coding
date: "2024-12-10T12:35:55Z"
description: ""
draft: false
cover:
  image: maksym-kaharlytskyi-Q9y3LRuuxmg-unsplash.jpg
  relative: true
slug: records-classes-and-equality-in-csharp
summary: The record modifier can define properties and equality in our classes for
  us, saving time and keeping our code cleaner. Let's see how it works!
tags:
- C# 9
- C#
- Coding
title: Records, Classes and Equality in C# 9 / .NET 5
---
When C# 9 was released in 2020, one of the main focuses was on "removing ceremony", as they put it. I'm sure that's a regular focus with each release, but this time they called it out specifically. As a result, some of the tedious, boilerplate types of syntax we find ourselves writing were removed.

Among other useful things (like top-level statements and init setters), they added the `record` modifier for classes. In the release a year after, they made it possible to mark structs as a `record` too. Adding the new modifier does a couple cool things for us that have me looking for opportunities to use them, so let's take a closer look.

> The code in this post is available on [GitHub](https://github.com/grantwinney/CSharpDotNetFeatures/tree/master/C%23%2009/RecordModifier), for you to use, extend, or just follow along while you read... and hopefully discover something new along the way!

## Classes

We've all written classes, many of us hundreds or even thousands of times. They're the primary way we group data together into a logical entity, along with methods to manipulate and transform the data, etc. Here's a simple one:

```csharp
public class Plane(string Make, string Model, int Year)
{
    public string Make { get; set; } = Make;
    public string Model { get; set; } = Model;
    public int Year { get; set; } = Year;
}
```

Standard class that doesn't implement interface or override equality operators

It's no surprise that we need public properties to access the values passed in via the primary constructor. And it should be no surprise that two instances with all the same values will _not_ be detected as "equal" to one another:

```csharp
var plane1 = new Plane("Cessna", "680A", 2015);
var plane2 = new Plane("Cessna", "680A", 2015);

Console.WriteLine(plane1.Equals(plane2));  // false
Console.WriteLine(plane1 == plane2);       // false
```

We haven't described what makes two `Plane` instances equal, so the references to each instance (which are always different) are used for the comparison by default.

## Classes (with equality defined by us)

If we want to [compare two objects for equality](https://grantwinney.com/csharp-compare-two-objects-for-equality/), there's a multitude of ways to do it. Here's two common ones – overriding equality operators and implementing the `IEquatable<T>` interface:

```csharp
public class Train(string Make, string Model, int Year) : IEquatable<Train>
{
    public string Make { get; set; } = Make;
    public string Model { get; set; } = Model;
    public int Year { get; set; } = Year;

    public static bool operator ==(Train? x, Train? y)
    {
        if (x is null || y is null)
            return false;

        return x.Make == y.Make && x.Model == y.Model && x.Year == y.Year;
    }

    public static bool operator !=(Train? x, Train? y)
    {
        return !(x == y);
    }

    // IEquatable<T>
    public bool Equals(Train? other)
    {
        return this == other;
    }
}
```

Standard class that implements `IEquatable<T>` and overrides equality operators

I don't know how everyone else likes to do it, but I tend to define equality in just one of the methods, and then have the other ones call it. It keeps the code more DRY, and more easily updated later on if needed.

Now if we create a couple of trains and test to see if they're equal, they are:

```csharp
var train1 = new Train("Odakyu", "3000", 1958);
var train2 = new Train("Odakyu", "3000", 1958);

Console.WriteLine(train1.Equals(train2));  // true
Console.WriteLine(train1 == train2);       // true
```

## Records (with equality built-in)

What's great about records is that a lot of these extra steps (aka ceremony) goes away.

Here's roughly the same class as above, with a different name and marked as a `record` this time. We _could_ include the keyword `class` in there too, since it is one, but it's unnecessary and can be left out.

```csharp
public record Automobile(string Make, string Model, int Year);
```

Record class that does the heavy lifting for us with regards to equality

Nearly everything that was defined in the `Train` class is gone, automatically generated for us behind the scenes! The **parameters we define in the primary constructor get matching properties automatically**, without us having to manually define them.

These properties are init-only properties, since **records are intended to represent "immutable data models"**, so by default we can't change values after a record is initialized. Instead, we're meant to create a new instance using a `with` expression:

```csharp
var kia = new Automobile("Kia", "Forte", 2016);
var newerKia = kia with { Year = 2021 };

Console.WriteLine(kia);
Console.WriteLine(newerKia);
Console.WriteLine(kia == newerKia);  // false
```

If we really want to though, we can define one or more properties with public setters ourselves, like any other class. That behavior might surprise anyone delving into our code who expects properties to be a one-and-done thing, though.

Also, **records implement the** *`*IEquatable<T>*`* **interface for us too**, comparing the values of each property. In fact, if we define a record like this, calling out the `IEquatable<T>` interface explicitly, it doesn't complain... but it also doesn't warn us about the missing method since it's already defined behind-the-scenes:

```csharp
public record Automobile(string Make, string Model, int Year)
     : IEquatable<Automobile>
```

And if we try to overload the equality operators, we can't. It warns us that they're already defined.

![](https://grantwinney.com/content/images/2024/12/image-1.png)

So now this works with just the original one-liner defining the record:

```csharp
var auto1 = new Automobile("Toyota", "Corolla", 2023);
var auto2 = new Automobile("Toyota", "Corolla", 2023);

Console.WriteLine(auto1.Equals(auto2));  // true
Console.WriteLine(auto1 == auto2);       // true
```

## Learning More

I feel like, personally, I oscillate between creating complex but useful things, and then simplifying them. Then I add to them which makes them more complex again, and then I go back and simplify again. It's a bit of a tug o' war, and I'm glad when the teams at Microsoft take the time to simplify things for us.

One last thought. I see a lot of "records vs classes" types of posts out there for C#, but a record isn't an _alternative_ to classes – it's a modifier that changes their behavior. I think the docs make that clear by stating that the `record class` syntax is _"a synonym to clarify a reference type"_. We can replace the `class` keyword with `record` in a class definition, or keep them both if it helps make things clearer, but it means the same thing. On the other hand, other references like the [primary constructors](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-12#primary-constructors) doc states that, _"You can now create primary constructors in any_ _`_class_`_ _and_ _`_struct_`__. Primary constructors are no longer restricted to_ _`_record_`_ _types."_, which makes it sound very separate. Maybe the `record` modifier changes behavior so drastically that it _should_ be considered a different type altogether?

If you want to learn more about records, I found [this article](https://falberthen.github.io/posts/cs10-records/) by Felipe Henrique interesting. Then there's the [official docs](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/record) from Microsoft, as well as [other changes in C# 9](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-version-history#c-version-9). And if you'd like to learn more about a variety of [C#](https://grantwinney.com/tag/csharp/) features, check out my [CSharpDotNetFeatures repo](https://github.com/grantwinney/CSharpDotNetFeatures), where you'll find links to plenty more blog posts and practical examples!
