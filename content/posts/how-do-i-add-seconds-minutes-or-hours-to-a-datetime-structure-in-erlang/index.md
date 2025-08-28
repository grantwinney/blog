---
categories:
- Erlang
- Coding
- Date-Time Handling
date: "2019-12-01T01:51:59Z"
description: ""
draft: false
cover:
  image: photo-1512856246663-647a81ef198e.jpg
slug: how-do-i-add-seconds-minutes-or-hours-to-a-datetime-structure-in-erlang
summary: I was trying to add times in Erlang, but couldn't find an existing function,
  so I wrote my own.
tags:
- Erlang
- Coding
- Date-Time Handling
title: Add and subtract time from a DateTime structure in Erlang
---
Some languages, like Ruby, give you 12 ways to do the same thing. Other languages, like Erlang, make it tough to find 1 way to do something.

Awhile back, I was trying to add a period of time to an existing `DateTime` value (in `{{Y,M,D},{H,m,s}}` format), but I couldn't find a function (such as in the `Calendar` module) that allowed me to manipulate a `DateTime` value directly.

If you're looking too, the `Calendar` module can be used to convert a `DateTime` to seconds, which makes it easier to add seconds, minutes, hours, etc.

```erlang
Date = {{2018,8,14},{13,10,25}}.
DateInSec = calendar:datetime_to_gregorian_seconds(Date).  % 63701471425
NewDateInSec = DateInSec + 10.                             % 63701471435
calendar:gregorian_seconds_to_datetime(NewDateInSec).      % {{2018,8,14},{13,10,35}}
```

Adding 10 seconds

```erlang
Date = {{2018,8,14},{13,10,25}}.
DateInSec = calendar:datetime_to_gregorian_seconds(Date).  % 63701471425
NewDateInSec = DateInSec + (10 * 60 * 60).                 % 63701507425 (10 hours)
calendar:gregorian_seconds_to_datetime(NewDateInSec).      % {{2018,8,14},{23,10,25}}
```

Adding 10 minutes and 10 hours, with a little math

To make life easier, I created a function to add additional time to (or subtract time from) an existing `DateTime`:

```erlang
-type datetime() :: {{non_neg_integer(), pos_integer(), pos_integer()},
                     {non_neg_integer(), non_neg_integer(), non_neg_integer()}}.
-type timespan() :: {integer(), integer(), integer()}.

-spec add_time_to_datetime(datetime(), timespan()) -> datetime().
add_time_to_datetime(Date, {Hour, Min, Sec}) ->
    DateInSeconds = calendar:datetime_to_gregorian_seconds(Date),
    NewDateInSeconds = DateInSeconds + (Hour * 60 * 60) + (Min * 60) + Sec,
    calendar:gregorian_seconds_to_datetime(NewDateInSeconds).
```

This is trivial to write, but then it's trivial for them to just include too. Add it to the things I miss in a modern framework like .NET.