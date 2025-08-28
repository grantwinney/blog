---
categories:
- Erlang
- Coding
- Date-Time Handling
date: "2018-07-01T00:32:52Z"
description: ""
draft: false
cover:
  image: sundial.jpg
  relative: true
slug: how-to-calculate-easter-and-other-holidays-in-erlang
summary: I wrote a small library for calculating Easter and other holidays in Erlang.
  Here's how I did it and what I learned.
tags:
- Erlang
- Coding
- Date-Time Handling
title: Calculate Easter and other holidays in Erlang
---
On a whim, I created an Erlang module for calculating holidays, and things were going okay until it came to Easter. Have you ever tried to calculate Easter? It's surprisingly difficult.

Easter doesn't occur on the same day of the month, or on the Xth Sunday, or anything that simple like most holidays. It's based on the occurrence of a particular full moon, among other things. I started by trying to recreate [this explanation](https://www.assa.org.au/edm#Calculator), but trying to recreate it in code got pretty ugly pretty quick.

Here's what I came up with, but it's only the Catholic (aka western) date. The Orthodox (aka eastern) date is a completely different calculation, which I implemented with the help of [Meeus's Julian algorithm](https://en.wikipedia.org/wiki/Computus#Meeus.27s_Julian_algorithm).

```erlang
-spec get_easter(atom(), pos_integer()) -> {pos_integer(), pos_integer(), pos_integer()}.
get_easter(catholic, Year) ->
    G = trunc(math:fmod(Year,19)),
    C = Year div 100,
    H = trunc(math:fmod(C - (C div 4) - ((8 * C + 13) div 25) + 19 * G + 15, 30)),
    I = H - (H div 28) * (1 - (H div 28) * trunc(29 / (H + 1)) * ((21 - G) div 11)),
    Day = trunc(I - math:fmod((Year + (Year div 4)) + I + 2 - C + (C div 4), 7) + 28),
    case Day of
        _ when Day > 31 ->
            {Year, 4, Day - 31};
        _ ->
            {Year, 3, Day}
    end

get_easter_test_() ->
    [
        ?_assertEqual({2019,4,21}, holidays:get_easter(catholic, 2019)),
        ?_assertEqual({2020,4,12}, holidays:get_easter(catholic, 2020)),
        ?_assertEqual({2021,4,4}, holidays:get_easter(catholic, 2021)),
        ?_assertEqual({2022,4,17}, holidays:get_easter(catholic, 2022)),
        ?_assertEqual({2023,4,9}, holidays:get_easter(catholic, 2023)),
        ?_assertEqual({2024,3,31}, holidays:get_easter(catholic, 2024)),
        ?_assertEqual({2025,4,20}, holidays:get_easter(catholic, 2025)),
        ?_assertEqual({2026,4,5}, holidays:get_easter(catholic, 2026)),
        ?_assertEqual({2027,3,28}, holidays:get_easter(catholic, 2027)),
        ?_assertEqual({2028,4,16}, holidays:get_easter(catholic, 2028)),
        ?_assertEqual({2029,4,1}, holidays:get_easter(catholic, 2029)),
        ?_assertEqual({2030,4,21}, holidays:get_easter(catholic, 2030))
    ].
```

There's a heavy use of `div` instead of `/` because the former behaves similar to integer arithmetic in C#, whereas the latter behaves like floating point arithmetic.

For example, `7 div 4 == 1` but `7 / 4 == 1.75`.

## The Importance of Tests

I ended up converting an [algorithm in C#](https://www.codeproject.com/Articles/10860/Calculating-Christian-Holidays), which was a [conversion from c++](https://www.codeproject.com/Articles/1595/Calculating-Easter-Sunday), which was in turn converted from Pascal code, so unit tests seemed like a good idea. I'm reasonably sure it's behaving!

Since we can pass functions around in Erlang, I added a function that allows for passing a date and a list of holidays to test it against.

```erlang
-spec is_holiday(atom(), date_timestamp(), [fun()]) -> boolean().
is_holiday(CountryCode, Date, Holidays) ->
    lists:any(fun(Holiday) -> Holiday(CountryCode, Date) =:= true end, Holidays).

MyDate = {{2019, 12, 25}, {0, 0, 0}},
holidays:is_holiday(us, MyDate, [fun holidays:is_easter/2,
                                 fun holidays:is_thanksgiving/2,
                                 fun holidays:is_new_years/2]).   % returns false
```

If you use Erlang and you need to know if a date is a holiday, try this out. If you have your own holidays to add, open an issue or PR, or just leave a comment below. Contributions welcome!
