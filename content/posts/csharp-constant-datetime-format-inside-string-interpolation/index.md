---
categories:
- csharp
- Coding
- csharp-6
- datetime-handling
date: "2019-04-04T20:06:50Z"
description: ""
draft: false
cover:
  image: photo-1501139083538-0139583c060f.jpg
slug: csharp-constant-datetime-format-inside-string-interpolation
summary: I was upgrading some code to use string interpolation, a feature introduced
  in C# 6, when I ran into a small snag with DateTimes and a format string stored
  as a constant.
tags:
- csharp
- coding
- csharp-6
- datetime-handling
title: Using a constant as a DateTime format inside string interpolation
---
I was upgrading some code to use string interpolation, a feature [introduced in C# 6](https://docs.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-6#string-interpolation), when I ran into a small snag with DateTimes and format strings.

> The code in this post is available on <a href="https://github.com/grantwinney/CSharpDotNetExamples/tree/master/C%23%2006/ConstantDateTimeFormatInStringInterpolation">GitHub</a>, for you to use, expand upon, or just follow along while you read... and hopefully discover something new!

The original code looked something like this:

```csharp
var name = "Grant";
var message = string.Format("Hello {0}, the date is {1:MM/dd/yyyy}.", name, DateTime.Now);
Console.WriteLine(message);

// output: Hello Grant, the date is 04/04/2019.
```

Updating the `string.Format` to use string interpolation was straight-forward:

```csharp
var name = "Grant";
var message = $"Hello {name}, the date is {DateTime.Now:MM/dd/yyyy}.";
Console.WriteLine(message);

// output: Hello Grant, the date is 04/04/2019.
```

But then I tried to move the format string into a constant so I could use it in several places, which didn't work:

```csharp
const string DATE_FORMAT = "MM/dd/yyyy";  // ignored

var name = "Grant";
var message = $"Hello {name}, the date is {DateTime.Now:DATE_FORMAT}.";
Console.WriteLine(message);

// output: Hello Grant, the date is DATE_9OR4AT.
```

The output looks funky because the `F` and `M` in "DATE_FORMAT" are valid formats (for tenths of a second and month, respectively). It took a few minutes to realize it, but I just had to use the overloaded `ToString()` method that accepts a format string:

```csharp
const string DATE_FORMAT = "MM/dd/yyyy";

var name = "Grant";
var message = $"Hello {name}, the date is {DateTime.Now.ToString(DATE_FORMAT)}.";
Console.WriteLine(message);

// output: Hello Grant, the date is 04/04/2019.
```

And what is string interpolation? According to [MSDN](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/string-interpolation), it's just syntactic sugar around the traditional `String.Format` method:

> At compile time, an interpolated string is typically transformed into a [String.Format](https://docs.microsoft.com/en-us/dotnet/api/system.string.format) method call. That makes all the capabilities of the [string composite formatting](https://docs.microsoft.com/en-us/dotnet/standard/base-types/composite-formatting) feature available to you to use with interpolated strings as well.

In general, it looks better, is clearer to read, and is more convenient too.

## Other Resources

Want to learn more? Start here:

- [$ - string interpolation (C# Reference)](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [String interpolation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/string-interpolation)
- [Back to Basics: String Interpolation in C#](https://weblog.west-wind.com/posts/2016/Dec/27/Back-to-Basics-String-Interpolation-in-C)