---
title: A comparison of DateTime to DateTimeOffset
slug: csharp-datetime-vs-datetimeoffset
summary: Should you use DateTime or DateTimeOffset? Well, it depends...
description:
date: 2025-09-06T17:16:00
draft: false
cover:
  image:
  alt: banner image for post
  caption:
categories:
  - Coding
tags:
  - csharp
  - datetime-handling
postimage: https://images.unsplash.com/photo-1421789665209-c9b2a435e3dc?q=80&w=1171
---
For the last 20 years, the .NET Framework (starting with 2.0 in 2005) has had two structures for storing date/time values - `DateTime` and `DateTimeOffset`. In the last 15+ years of programming, nearly every instance of any C# code I've seen dealing with dates and times uses `DateTime`, and I have no idea why. My guess is that a lot of intro books and official docs used `DateTime`, since it's a little simpler, and everyone just went with it and didn't look back.

It's important to take a few minutes and understand the difference, so let's take a look at a couple short examples, which you can also [find here](https://dotnetfiddle.net/tyxO1X).

## A Short Example

### DateTime

Here's a short example using three `DateTime` values. The first one is in the "local" time zone (but which one is that?), the second one is UTC, and the last one isn't specified... could be either, who knows.

```csharp
Console.WriteLine("=== DATETIME ===");

var dtLocal = new DateTime(2025, 9, 4, 12, 0, 0, DateTimeKind.Local);
var dtUTC = new DateTime(2025, 9, 4, 12, 0, 0, DateTimeKind.Utc);
var dtUnk = new DateTime(2025, 9, 4, 12, 0, 0);  // DateTimeKind.Unspecified

// Local vs UTC vs Unknown DateTime
Console.WriteLine($"Local: {dtLocal}");      // 9/4/2025 12:00:00 PM
Console.WriteLine($"UTC: {dtUTC}");          // 9/4/2025 12:00:00 PM
Console.WriteLine($"Unspecified: {dtUnk}");  // 9/4/2025 12:00:00 PM

// Convert between Local and UTC
Console.WriteLine($"Local to UTC: {dtLocal.ToUniversalTime()}");  // 9/4/2025 4:00:00 PM
Console.WriteLine($"UTC to Local: {dtUTC.ToLocalTime()}");        // 9/4/2025 8:00:00 AM

Console.WriteLine($"Unspecified to UTC: {dtUnk.ToUniversalTime()}");  // 9/4/2025 4:00:00 PM
Console.WriteLine($"Unspecified to Local: {dtUnk.ToLocalTime()}");    // 9/4/2025 8:00:00 AM
```

Printing them all out to the console (lines 8-10) says nothing about which time zone the `DateTime` was recorded in. Still, it's tracking local vs UTC internally via the `DateKind` property, and converting the local one to UTC or vice-versa shows that it knows that at least (lines 13-14).

What is "local" though? Once the value is stored in the database in a `DateTime` column type, we can't know. Not only do we not know which local time zone it originally represented, we don't even know if it _was_ local, or if it was UTC! That's the catch. We've lost a critical bit of context.

As for the unspecified one (lines 16-17), it's even less helpful. If you convert it to UTC, it assumes you started with local. If you convert it to local, it assumes you started with UTC. After all, we never told it.

### DateTimeOffset

And here's a short example using three `DateTimeOffset` values. The first one is in a specific "local" time zone (EDT in this case), the second one is UTC, and the last one isn't specified... which defaults to the current "local" time zone, so it actually _is_ still known. No ambiguity here.

```csharp
Console.WriteLine("=== DATETIMEOFFSET ===");

var dtoLocal = new DateTimeOffset(2025, 9, 4, 12, 0, 0, TimeSpan.FromHours(-4));
var dtoUTC = new DateTimeOffset(2025, 9, 4, 12, 0, 0, TimeSpan.FromHours(0));
var dtoUnk = new DateTimeOffset(dtUnk);  // treated as current local time zone

// Local vs UTC vs Unknown DateTimeOffset
Console.WriteLine($"Local: {dtoLocal}");      // 9/4/2025 12:00:00 PM -04:00
Console.WriteLine($"UTC: {dtoUTC}");          // 9/4/2025 12:00:00 PM +00:00
Console.WriteLine($"Unspecified: {dtoUnk}");  // 9/4/2025 12:00:00 PM -04:00

// Convert between Local and UTC
Console.WriteLine($"Local to UTC: {dtoLocal.ToUniversalTime()}");  // 9/4/2025 4:00:00 PM +00:00
Console.WriteLine($"UTC to Local: {dtoUTC.ToLocalTime()}");        // 9/4/2025 8:00:00 AM -04:00

Console.WriteLine($"Unspecified to UTC: {dtoUnk.ToUniversalTime()}");  // 9/4/2025 4:00:00 PM +00:00
Console.WriteLine($"Unspecified to Local: {dtoUnk.ToLocalTime()}");    // 9/4/2025 12:00:00 PM -04:00
```

Now, printing local (or unspecified) uses the current local time zone at the time it was defined, and printing UTC uses UTC of course (lines 8-10). Converting between the two (lines 13-14) is consistent, and there's no unspecified or unknown value (lines 16-17) anymore since it's just treated as local too.

Storing these values in a `DateTimeOffset` column type retains the extra context. So we should use `DateTimeOffset` _everywhere_, right? Well, no...

## Which one do we use?

Sometimes we need the extra context...

- Imagine User A and User B live in time zones **3 hours apart**.
- User A uses an app to schedule a meeting for 8am (for User B it's 11am), and it's stored as a local `DateTime` value.
- User B uses the same app to reopen the schedule and the system, having no idea *which* local time zone it was created in, shows it at 8am instead of 11. Confused, he moves it to 11am, but now User A has it scheduled for 11am as well.

One solution is to convert all `DateTime` values to UTC before saving to the database, and then read them out assuming they're UTC as well. Anyone who's worked in a system that a hundred other people have touched over the course of a couple _decades_ knows that's a fool's dream.

The better solution is to use `DateTimeOffset` which has time zone built in. You can't _not_ specify a time zone, so User A schedules for 8am PST, and when User B opens it later, the system can see they're in EST and convert it to 11am for him.

But sometimes we _don't_ need that, like when we're talking about a specific point in time that's independent of time zone. If Acme Inc is a nationwide retail chain that always closes at 8pm, and their next inventory is at closing time on Fri Oct 3rd, then maybe there's a `NextInventoryDate` column in the database somewhere that's set to "8/3/2025 20:00:00" and that's the same *everywhere*. No matter what time zone you're in, your store closes at 8pm on Aug 3rd.

And so the answer, as usual, is "it depends". *(But it seems like `DateTimeOffset` is better, more often than not.)*

## References

- [DateTime Struct (System) | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.datetime?view=net-9.0) (and [DateTime.Kind](https://learn.microsoft.com/en-us/dotnet/api/system.datetime.kind?view=net-9.0))
- [DateTimeOffset Struct (System) | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=net-9.0)
- [Compare types related to date and time - .NET | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime)