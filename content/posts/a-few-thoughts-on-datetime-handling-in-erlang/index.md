---
categories:
- Erlang
- Date-Time Handling
date: "2018-09-03T14:48:51Z"
description: ""
draft: false
cover:
  image: pocket-watch-1637396_1280.jpg
slug: a-few-thoughts-on-datetime-handling-in-erlang
summary: Handling date and times is a thorn in every experienced developer's side.
  If you haven't had the pleasure yet, you will. ;) Coming off a week of standardizing
  some datetimes across an Erlang app, here's a few personal thoughts.
tags:
- Erlang
- Date-Time Handling
title: A few thoughts on date/time handling in Erlang
---
Ask any programmer who's been at it awhile what their biggest aggravations are, and I'd bet handling dates and times is nearly always in the top 5. I'm just getting off of a week or so of standardizing some date/time handling across an Erlang application, so here's a few thoughts while it's still fresh in my mind _(and then I don't want to think about time ever again)_.

## Decide how to represent time internally

Decide on what's best for your app and _stick to it_. Erlang has several ways to represent time, each with varying levels of precision. You can [read more here](https://learnyousomeerlang.com/time), but they include:

- [calendar:universal_time](http://erlang.org/doc/man/calendar.html#universal_time-0) - returns a tuple, max resolution of seconds
- [erlang:timestamp](https://erldocs.com/18.0/erts/erlang.html#timestamp/0) - returns a _different_ tuple, max resolution of microseconds since [epoch](https://stackoverflow.com/a/1090945/301857)
- [erlang:system_time](https://erldocs.com/18.0/erts/erlang.html#system_time/0) - returns an integer, max res of nanoseconds since the [epoch](https://stackoverflow.com/a/1090945/301857)
- [erlang:monotonic_time](https://erldocs.com/18.0/erts/erlang.html#monotonic_time/0) returns an ever-increasing integer, but not a "time" in the usual sense

An HR app that stores hiring and termination dates, or a time clock app that tracks punch in / punch out times, might only need seconds precision. An app dealing with transactions that occur thousands of times a second may need a better resolution. But whatever you choose, you either end up with an integer, a tuple of integers, or a tuple of tuples of integers.

While [dialyzer](https://learnyousomeerlang.com/dialyzer) and [writing tests](https://learnyousomeerlang.com/eunit) can help, representing the same thing several different ways is at best aggravating... and at worst, leads to difficult to trace bugs. And if you use `erlang:system_time` at different resolutions, like nanoseconds and seconds for example, you'll end up with differently sized integers that mean completely different things - and dialyzer won't help at all.

![](spec-gandalf.png)

## Decide how to represent time externally

If your app has some sort of GUI for users, or an API providing access to data, you'll need a way to present dates and times that's easy to read. Displaying the number of nanoseconds since the epoch, or expecting values to be supplied to an API in that format, is hardly user-friendly. :)

One of the most consistent ways to deal with dates and times is the [ISO8601](https://www.w3.org/TR/NOTE-datetime) standard, which defines a standardized way to display things that everyone can agree on. I'm sure there are other standards besides ISO8601 too (there always are), but this one seems to have stuck.

![](https://imgs.xkcd.com/comics/standards.png)

[_xkcd_](https://xkcd.com/927)

The [iso8601 library](https://github.com/erlsci/iso8601) works nicely, but only formats times that are tuples, so even though ISO8601 technically supports nanoseconds this particular library does not. It can also parse ISO8601 values back to something Erlang can natively work with.

I'd also suggest converting back and forth as soon as it makes sense in your app. In other words, don't pass ISO8601 values around at all levels of your app if what you really want to work with is an integer representing nanoseconds. Just convert the integer to ISO8601 right before you pass it to the user, and convert it to an integer again as they send it back to you.

## Store everything in GMT (UTC +0)

Another great source of hard-to-trace bugs (if that's your thing) is storing dates and times in a local timezone. Store everything in GMT _(_[_not the same as UTC_](https://www.timeanddate.com/time/gmt-utc-time.html)_!)_ so you know exactly where your starting point is, and then convert values to a local timezone at the moment you need them - for a calculation, display purposes, or something else. While a datetime is stored and passed around your system, you really _really_ want high confidence what format it's in so you're not making "best guesses" later on. And if a time is converted to a particular timezone and then stored as an integer, it'll be impossible to figure out what the original timezone was.

Like so many other things in Erlang, there's little to no native support for timezones - one of the many things I miss about the .NET ecosystem. There are several Erlang libraries out there to help working with timezones, but most of them appear abandoned. The best right now seems to be the [qdate](https://github.com/choptastic/qdate) library, which in turn was (I think) based off the [Erlang Localtime](https://github.com/dmitryme/erlang_localtime) library.

## Familiarize yourself with the native tools

If there's some native tools for manipulating dates and times in your language, get familiar with them before turning to third-party libraries. Sometimes they're not much-used or particularly well-known, and you'll only stumble on them by reading the docs. It's worth it.

Erlang provides a handful of functions in the [calendar module](http://erlang.org/doc/man/calendar.html), as well as in the [Erlang BIFs](http://erlang.org/doc/man/erlang.html) like getting times in various formats and converting from local to universal time and back again. But I just stumbled on a little BIF called [convert_time_unit](http://erlang.org/doc/man/erlang.html#convert_time_unit-3) that converts integer values between time units, like nanoseconds to seconds. Internally, it's probably just performing a `div` operation, but I find it to be more self-documenting. Just keep in mind that it always rounds down.

```erlang
% without bif
-define(MILLISECONDS, 1000).
Timeout = 2777 div ?MILLISECONDS.       % 2

% using bif
Timeout = erlang:convert_time_unit(2777, millisecond, second).  % 2

% rounding
Timeout = round(2777 / ?MILLISECONDS).  % 3
```

## Read more about time... and weep

Does time have to be this tricky? I don't know, but right now it's a huge thorn in developers' sides everywhere. Toss in timezones, daylight savings, fractions of seconds, etc and it only gets worse. Here are some fun articles that'll make even the most time-saavy among you realize... it can always get worse. ;)

- [Falsehoods programmers believe about time: @noahsussman: Infinite Undo](https://infiniteundo.com/post/25326999628/falsehoods-programmers-believe-about-time)
- [More falsehoods programmers believe about time: @noahsussman: Infinite Undo](https://infiniteundo.com/post/25509354022/more-falsehoods-programmers-believe-about-time)
- [The Problem with Time & Timezones - Computerphile - YouTube](https://www.youtube.com/watch?v=-5wpm-gesOY)  _(if you happen to work with a framework that makes handling time and timezones easy, this last one'll have you thanking its authors!)_
