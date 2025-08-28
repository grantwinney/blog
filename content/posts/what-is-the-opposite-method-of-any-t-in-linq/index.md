---
categories:
- C#
- LINQ
- Coding
date: "2019-11-11T23:36:00Z"
description: ""
draft: false
cover:
  image: photo-1515879218367-8466d910aaa4.jpg
  relative: true
slug: what-is-the-opposite-method-of-any-t-in-linq
summary: One of the many nice functions in LINQ is a single word that iterates through
  a collection, returning true if at least one item in the collection matches the
  condition you specify. But what's the opposite of the Any keyword in LINQ?
tags:
- C#
- LINQ
- Coding
title: What is the opposite of Any in LINQ?
---
If you are (or hope to be) a .NET developer, knowing [LINQ](https://learn.microsoft.com/en-us/dotnet/csharp/linq/) is a _huge_ time-saver. It's a syntax that allows you to manipulate data in a fashion that'll be familiar to anyone who's worked in a database.

## Any

One of the many nice functions in LINQ is a single word that iterates through a collection, returning `true` if at least one item in the collection matches the condition you specify.

```csharp
var currencies = new[] { "USD", "EUR", "JPY" };

Console.WriteLine(currencies.Any(x => x == "MXN"));  // False
```

But what's the _opposite_ of `Any<T>()`?

What if, instead of finding out whether the list of currencies includes "peso", you wanted to make sure the list of currencies did _not_ include "peso"? You could negate the above, but you might think that reads a bit funny... and I'd agree.

```csharp
var currencies = new[] { "USD", "EUR", "JPY" };

Console.WriteLine(!currencies.Any(x => x == "MXN"));  // True
```

## All

The only way to make sure that a list _doesn't_ include a particular value, or that _no_ item in the collection matches a particular condition, is to check every single item in the collection... and that's what `All<T>()` is for.

```csharp
var currencies = new[] { "USD", "EUR", "JPY" };

Console.WriteLine(currencies.All(x => x != "MXN"));          // True

Console.WriteLine(currencies.All(x => !x.StartsWith("M")));  // True
```

## Try it out

[Try it out yourself here](https://dotnetfiddle.net/Widget/Sxr0Yz)!
