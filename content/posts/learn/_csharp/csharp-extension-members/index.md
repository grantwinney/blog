---
title: Using Extension Members in C# 14
slug: csharp-extension-members
summary: Extension members take extension methods to the next level. Let's see how to use this new C# 14 feature.
description:
date: 2025-12-02T12:58:00
draft: false
postimage: /banners/default-learn-banner.webp
postimagealt:
postimagecaption:
categories:
  - Learn
tags:
  - csharp-14
---
Visual Studio 2026 was [released a week ago](https://learn.microsoft.com/en-us/visualstudio/releases/2026/release-notes), so it's time to checkout [.NET 14](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-14) and see what new and interesting goodies we got, starting with [extension members](https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/extension-methods)! If you'd like, you can check out [my thoughts on other .NET 14 features](https://grantwinney.com/tags/csharp-14), as well as download the source code for this post from my [CSharpDotNetFeatures](https://github.com/grantwinney/CSharpDotNetFeatures) repo on GitHub.

## Extension Methods (a review)

For a long time, we've had the ability to extend a class with extension methods, making it seem like the original class has methods on it that it doesn't. For example, here's a class with three methods that act on strings:

```cs
public static class BlogStringHelpers
{
    // Truncate post for summary, as for use in social media.
    public static string ToExcerpt(this string str, int maxLength = 128) =>
        str.Length <= maxLength ? str : $"{str[..maxLength]}...";

    // Convert string to a slug to use as identifier for post.
    public static string ToSlug(this string str) =>
        Regex.Replace(str.Trim().ToLower(), @"\s+", "-");

    // Convert string to Title Case to use as title for post.
    public static string ToTitleCase(this string str)
    {
        var words = str.Split(' ', StringSplitOptions.RemoveEmptyEntries);
        for (int i = 0; i < words.Length; i++)
        {
            var w = words[i];
            words[i] = char.ToUpper(w[0]) + w.Substring(1, w.Length - 1);
        }
        return string.Join(' ', words);
    }
}
```

And here's a few examples using the above methods:

```cs
var post = "Here we go again, with another .NET release.";
var excerpt = post.ToExcerpt(10);  // "Here we go..."

var str = "using new extension members";
var slug = str.ToSlug();           // "using-new-extension-members"
var title = str.ToTitleCase();     // "Using New Extension Members"
```

The first parameter to any extension method is an instance of whatever class is being "extended" (i.e. `this string str` above), so there's no such thing as methods that extend a class _without_ needing an instance. Until now...

## Extension Members

### Instance Methods

To convert the above to use the new [extension members](https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/extension-methods) feature:

1. Remove `this string str` from each method,
2. Nest everything inside an extension block that defines `str` as the extension (aka receiver) parameter, and
3. Remove `static` from everything except the class definition.

The `string str` variable is then available to all of the (instance) methods defined inside the curly braces of the extension block:

```cs
public static class BlogStringHelpers
{
    extension(string str)
    {
        // Truncate post for summary, as for use in social media.
        public string ToExcerpt(int maxLength = 128) =>
            str.Length <= maxLength ? str : $"{str[..maxLength]}...";

        // Convert string to a slug to use as identifier for post.
        public string ToSlug() =>
            Regex.Replace(str.Trim().ToLower(), @"\s+", "-");

        // Convert string to Title Case to use as title for post.
        public string ToTitleCase()
        {
            var words = str.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            for (int i = 0; i < words.Length; i++)
            {
                var w = words[i];
                words[i] = char.ToUpper(w[0]) + w.Substring(1, w.Length - 1);
            }
            return string.Join(' ', words);
        }
    }
}
```

The output of the above members is the same as the extension methods, but let's take a look at what else we can do that we _couldn't_ do before.

### Instance Properties

Aside from instance _methods_ (like above), we can also define instance *properties*:

```cs
public static class BlogStringHelpers
{
    extension(string str)
    {
        public bool IsTitleTooLong => str.Length > 100;

        public bool IsTitleAlphanumeric =>
            str.All(c => char.IsLetterOrDigit(c) || char.IsWhiteSpace(c));
    }
```

Calling them is as straightforward as calling the methods:

```cs
var str = "Introducing C# 14"
var isTooLong = str.IsTitleTooLong;             // false
var isTitleAlphaNum = str.IsTitleAlphanumeric;  // false
```

This works because the instance of whatever type we're extending is defined in the extension container and available to the properties. Let's see what else we can do.

### Static Methods and Properties

We can also define static methods and properties, to extend a type without requiring a particular instance of it. This is something we couldn't do with extension methods, as the first parameter was *always* the instance itself.

All that's needed in the extension block container is the type we're extending, in this case the `DateTime` struct. It already has `Today`, of course, but what if I wrote an app that needed to use yesterday's date in a bunch of places? Might be convenient to effectively add that to `DateTime`:

```cs
public static class DateTimeHelpers
{
    extension(DateTime)
    {
        public static DateTime Yesterday => DateTime.UtcNow.AddDays(-1);
        public static DateTime Tomorrow => DateTime.UtcNow.AddDays(1);

        public static string Weekday => DateTime.UtcNow.DayOfWeek.ToString();

        public static bool IsTodayTomorrow => false;

        public static string GetDailyHoroscope()
            => "Carp diem.. seize the fish! Or is it fish the day? 🐟";
    }
}
```

All of these static properties and methods can be called as if they're part of `DateTime` and, unlike the older extension methods, we don't need to have an instance of a date first.

```cs
Console.WriteLine(DateTime.Yesterday);            // 12/1/2025 4:22:06 PM
Console.WriteLine(DateTime.Tomorrow);             // 12/3/2025 4:22:06 PM
Console.WriteLine(DateTime.Weekday);              // Tuesday
Console.WriteLine(DateTime.IsTodayTomorrow);      // False
Console.WriteLine(DateTime.GetDailyHoroscope());  // Carp diem.. seize the fish! Or is it fish the day? 🐟
```

### Operators

We can also define operators. For example, the `DateTime` struct already defines a few operators for manipulating dates, but what if we wanted to add a new one to add days to a given date?

```cs
public static DateTime operator +(DateTime d, TimeSpan t)
public static DateTime operator -(DateTime d, TimeSpan t)
public static TimeSpan operator -(DateTime d1, DateTime d2)
```

We could define a new operator that takes days instead of a `TimeSpan`, like this:

```cs
public static class DateTimeHelpers
{
    extension(DateTime)
    {
        public static DateTime operator +(DateTime dt, int days)
            => dt.AddDays(days);
    }
}
```

```cs
var added = DateTime.Now + 5;  // 12/7/2025 12:55:29 PM
```

Of course, I wouldn't recommend using this *exact* code, since it's not obvious whether `5` represents days, years, or something else, but the point is that you can do things like this if you have a use for them!

## Everything All At Once

Finally, we can also define extension methods in the same class with extension members, as well as other things. Here's a class that's doing bits of everything we talked about above and more:

```cs
public static class DateTimeHelpers
{
    public static readonly DateTime Yesterday;

    // Static Constructor, because why not..
    static DateTimeHelpers()
    {
        Yesterday = DateTime.Yesterday;
    }

    // Simple Method
    public static string GetStats()
    {
        var i = typeof(DateTimeHelpers).GetMethods(
            BindingFlags.Static | BindingFlags.Public).Length;
        return $"{nameof(DateTimeHelpers)} has {i} properties.";
    }

    // Extension Method
    public static int GetPreviousLeapYear(this DateTime dt) =>
        Enumerable.Range(dt.Year - 4, 4).Single(DateTime.IsLeapYear);

    // Extension Members
    extension(DateTime dt)
    {
        // Static Method
        public static string GetDailyHoroscope()
            => "Carpe die.. seize the dice! 🎲🎲";

        // Static Properties
        public static DateTime Yesterday => DateTime.UtcNow.AddDays(-1);
        public static DateTime Tomorrow => DateTime.UtcNow.AddDays(1);

        // Instance Method
        public int GetNextLeapYear() =>
            Enumerable.Range(dt.Year + 1, 4).Single(DateTime.IsLeapYear);

        // Instance Properties
        public DateTime NextWeek => dt.AddDays(7);
        public bool IsFutureDate => dt > DateTime.UtcNow;
    }
}
```

## Thoughts

Having not actually used them in a prod environment yet, outside of the post here, I like them so far. I like that it makes things a little more DRY by defining the variable once in the extension container, and I like having the ability to define extension *properties* now too. I also like that it makes a class a bit more organized by keeping the extension methods and properties grouped inside the extension block.

Will I use this a lot in the future? I'm not sure.. what do you think? Do you have any good uses for this in mind?

## Learn More

- [Extension member declarations - C# reference | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/extension)
- [Extension members - C# | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/extension-methods)
