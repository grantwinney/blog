---
categories:
  - Coding
date: 2023-11-03T16:35:50Z
description: ""
draft: false
cover:
  image:
slug: if-else-vs-switch-case-pattern-matching
summary: "A look at if/else, switch/case, pattern matching, other options ... and which is best. (spoiler: none ;) )"
tags:
  - csharp
title: If/else vs switch/case pattern matching
---
I stumbled on a pull request recently, in which the suggestion was made to replace an `if/else` block with a `switch/case`. The reviewer seemed to feel it was "better". In reality, these are just two approaches to organizing conditional logic, and using one or the other mostly comes down to a matter of taste.. especially with changes to C# in the last few years, but more on that below.

> The code in this post is available on <a href="https://github.com/grantwinney/CSharpDotNetExamples/tree/master/C%23%2009/SwitchPatternMatchingVsIfElse">GitHub</a>, for you to use, expand upon, or just follow along while you read... and hopefully discover something new!

Thinking back to when _I've_ used one over the other, I'd say I reserve `switch/case` for sets with a distinct, finite number of values. Say you want to set a couple flags, based on the current set piece being manipulated in a game of chess. You might use code like this:

```csharp
enum ChessPiece { Rook, Knight, Bishop, King, Queen, Pawn }

var canMoveMultiple = false;
var canMoveStraight = false;

switch (currentChessPiece)
{
  case ChessPiece.Rook:
    canMoveMultiple = true;
    canMoveStraight = true;
    break;
  case ChessPiece.Bishop:
    canMoveMultiple = true;
    canMoveStraight = false;
    break;
  // etc, etc.
}
```

The above code could easily be replaced with an `if/else`; however, I (subjectively) think the first one is "better".

```charp
enum ChessPiece { Rook, Knight, Bishop, King, Queen, Pawn }

var canMoveMultiple = false;
var canMoveStraight = false;

if (currentChessPiece == ChessPiece.Rook)
{
  canMoveMultiple = true;
  canMoveStraight = true;
}
else if (currentChessPiece == ChessPiece.Bishop)
{
  canMoveMultiple = true;
  canMoveStraight = false;
}
else if ( // etc, etc...
```

We usually have quite a bit of freedom in how we write our code though, so maybe the "best" option is to do something else entirely. One might be more readable, or more testable, or just more consistent with the rest of the code in the application.

```csharp
enum ChessPiece { Rook, Knight, Bishop, King, Queen, Pawn }

var canMoveMultiple = currentChessPiece == ChessPiece.Rook
  || currentChessPiece == ChessPiece.Knight
  || currentChessPiece == ChessPiece.Bishop
  || currentChessPiece == ChessPiece.Queen;

var canMoveStraight = currentChessPiece == ChessPiece.Rook
  || currentChessPiece == ChessPiece.King
  || currentChessPiece == ChessPiece.Queen
  || currentChessPiece == ChessPiece.Pawn;
```

## Traditional switch/case can't match if/else

One of the drawbacks to `switch/case` has traditionally been that it could only test _distinct_ values - not a range of values like greater or less than. A particular "case" could test a single value, but it couldn't do anything too fancy.

```csharp
switch (bankBalance)
{
  case > 1000000:  // can't do that...
    Console.WriteLine("You're getting a yacht!");
    break;
  case 5 to 10:  // can't do that either...
    Console.WriteLine("You're getting a coffee!");
    break;
  case 0:
    Console.WriteLine("You're getting a job!");
    break;
}
```

You could use the "default" case to catch a single range of values, I suppose, but that's incredibly limited.

```csharp
var remainingBal = currentBal - purchPrice;

switch (remainingBal)
{
  case 0:
    Console.WriteLine("Insufficient funds.");
    break;
  default:
    Console.WriteLine("You're all good!");  // oh wait, negative values are a thing
    break;
}
```

## Pattern matching to the rescue

Things changed in C# 9 though, with all kinds of [pattern matching enhancements](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-9#pattern-matching-enhancements). It probably started before that, but whenever the additional pattern matching / guard clause / case guard / whatever you want to call it was tacked on, the `switch/case` construct supports all kinds of fancy conditions now. It's thumbing its nose at `if/else`, singing "anything you can do, I can do ... just as well".

There's a lot to check out in the [switch expression](https://learn.microsoft.com/en-US/dotnet/csharp/language-reference/operators/switch-expression) docs too, especially in the section on "case guards", but you're here so let's play around with a few examples just to see what it looks like.

## Examples, using the Open Notify API

A few years back, [I wrote about the Open Notify API](https://grantwinney.com/what-is-iss-notify-api/). It's as straight-forward an API as you could possibly have. One of the endpoints returns some data about the current location of the ISS, and the JSON response looks like this:

```json
{"timestamp": 1698171977,
 "message": "success",
 "iss_position": {"latitude": "-50.8709", "longitude": "-6.0132"}}
```

First, we need a couple classes to hold the response. The "message" property contains the status of the API call, and the "iss_position" is, well.. I'll leave it to your imagination. _(A_ [_review of latitude and longitude_](https://www.thoughtco.com/degree-of-latitude-and-longitude-distance-4070616)_, for those who want extra credit, lol.)_

```csharp
public class IssResponse
{
    [JsonPropertyName("timestamp")]
    public int Timestamp { get; set; }

    [JsonPropertyName("message")]
    public string Message { get; set; }

    [JsonPropertyName("iss_position")]
    public IssPosition Position { get; set; }
}

public class IssPosition
{
    [JsonPropertyName("longitude")]
    public string Longitude { get; set; }

    [JsonPropertyName("latitude")]
    public string Latitude { get; set; }
}
```

Then we'll hit the API endpoint using [RestSharp](https://restsharp.dev/) and parse out the response into a couple variables for latitude and longitude. _(Normally I'd test the bool value that_ _`_decimal.TryParse_`_ _returns, but I'm assuming, perhaps incorrectly, that if the API were going to return invalid long/lat values, then the "message" would not be "success".)_

```csharp
var client = new RestClient();
var request = new RestRequest("http://api.open-notify.org/iss-now.json");
var response = await client.GetAsync<IssResponse>(request);

decimal.TryParse(response?.Position.Latitude, out var latitude);
decimal.TryParse(response?.Position.Longitude, out var longitude);
```

### If/else

Alrighty then. If we wanted to print out a message to the user about the location of the ISS, we _could_ use a bunch of nested `if/else` statements like this:

```csharp
switch (response?.Message)
{
  case "success":
    if (latitude > 0)
    {
      if (longitude > 0)
        Console.WriteLine("The ISS is over the northern hemisphere, east of the prime meredian.");
      else if (longitude == 0)
        Console.WriteLine("The ISS is over the northern hemisphere, on the prime meredian.");
      else
        Console.WriteLine("The ISS is over the northern hemisphere, west of the prime meredian.");
    }
    else if (latitude == 0)
    {
      if (longitude > 0)
        Console.WriteLine("The ISS is over the equator, east of the prime meredian.");
      else if (longitude == 0)
        Console.WriteLine("The ISS is over the equator, on the prime meredian.");
      else
        Console.WriteLine("The ISS is over the equator, west of the prime meredian.");
    }
    else
    {
      if (longitude > 0)
        Console.WriteLine("The ISS is over the southern hemisphere, east of the prime meredian.");
      else if (longitude == 0)
        Console.WriteLine("The ISS is over the southern hemisphere, on the prime meredian.");
      else
        Console.WriteLine("The ISS is over the southern hemisphere, west of the prime meredian.");
    }
    break;

  default:
    Console.WriteLine(@" ¯\_(ツ)_/¯ ");
    break;
}
```

### Switch/case with no pattern matching

The traditional `switch/case` couldn't handle any complex logic beyond a simple pattern match, so the above would've probably been our only option.

```csharp
switch (response?.Message)
{
  case "success":
    {
      switch (latitude)
      {
        case 0:   // What about greater or less than 0?
          break;
        default:  // This would catch greater AND less than 0...
          break;
      }
      // etc, etc...
    }
    break;

  default:
    Console.WriteLine(@" ¯\_(ツ)_/¯ ");
    break;
}
```

### Switch case WITH pattern matching

But with pattern matching (guard clauses / whatever), the above is possible to fully convert. We can check that when the message is "success" and lat/long are both greater than 0, we hit one case, but if the message is "success" and lat/long are both _less_ than 0, we hit another case... and on and on.

There are _tons_ of different patterns too, and even shorthand ways of writing it that save some keystrokes. If you're interested in more, go check out the [pattern matching enhancements](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-9#pattern-matching-enhancements) in C# 9 and the [switch expression](https://learn.microsoft.com/en-US/dotnet/csharp/language-reference/operators/switch-expression) docs, and then whatever those link to. Or just keep in mind that this is available as another tool, and the next time you're thinking about using a switch/case, give them the once over.

```csharp
switch (response?.Message)
{
    case "success" when latitude > 0 && longitude > 0:
        Console.WriteLine("The ISS is over the northern hemisphere, east of the prime meredian.");
        break;
    case "success" when latitude > 0 && longitude == 0:
        Console.WriteLine("The ISS is over the northern hemisphere, on the prime meredian.");
        break;
    case "success" when latitude > 0 && longitude < 0:
        Console.WriteLine("The ISS is over the northern hemisphere, west of the prime meredian.");
        break;

    // etc, etc...

    default:
        Console.WriteLine(@" ¯\_(ツ)_/¯ ");
        break;
}
```

Don't forget about switch expressions too, which can be quicker to read but only apply when you're returning a single value.

```csharp
var message = response?.Message switch
{
  "success" when latitude > 0 && longitude > 0 => "The ISS is over the northern hemisphere, east of the prime meredian.",
  "success" when latitude > 0 && longitude == 0 => "The ISS is over the northern hemisphere, on the prime meredian.",
  "success" when latitude > 0 && longitude < 0 => "The ISS is over the northern hemisphere, west of the prime meredian.",
  // and on and on...
  _ => @" ¯\_(ツ)_/¯ "
};

Console.WriteLine(message);
```

Here's another example using switch expressions, just because I think they're neat and that stepping away from the API example for a minute might be good. Imagine this is for some business app, where the user action and other factors (like the current date) determine which report is run.

```csharp
void RunReport(string userAction, User user)
{
  var d = DateTime.Now;

  var reportToRun = userAction.ToLower() switch
  {
    "sale" => "CustomerPurchase",

    "audit" when user.Position == "manager" || user.Department == "accounting" => "AuditDetail",
    "audit" => "AuditPersonal",

    "timecard" when user.Department == "hr" => "CorporateSchedule",
    "timecard" when d.DayOfWeek == DayOfWeek.Saturday || d.DayOfWeek == DayOfWeek.Sunday => "OvertimeSchedule",
    "timecard" => "Schedule",

    "support" when user.Position == "lead" => "DailySupportTickets",
    "support" when user.Position == "softwaredeveloper" => "SupportTicketDetail",
    "support" => "SupportTicketSummary",
  };

  // ExecuteReport(reportToRun);
}
```

### There's always another way

And of course, don't forget that there's always another way to do things, and sometimes neither a huge `if/else` _or_ `switch/case` block result in the shortest, concisest, [DRY](https://docs.getdbt.com/terms/dry)est code you could write.

Back to the API example, here's a _much_ shorter way to do the same as all the above, using a few [ternary conditional operators](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/conditional-operator) and some [string interpolation](https://grantwinney.com/using-string-interpolation-to-craft-readable-strings/). Whether it's more readable is an exercise I'll leave to the reader. ;)

```csharp
var latMsg = latitude > 0 ? "northern hemisphere" : latitude == 0 ? "equator" : "southern hemisphere";
var lngMsg = longitude > 0 ? "east of" : longitude == 0 ? "on" : "west of";

Console.WriteLine(response?.Message == "success"
    ? $"The ISS is over the {latMsg}, {lngMsg} the prime meridian."
    : " ¯\\_(ツ)_/¯ ");
```

If you found this content useful, and want to learn more about a variety of C# features, check out [this GitHub repo](https://github.com/grantwinney/CSharpDotNetExamples), where you'll find links to plenty more blog posts and practical examples!
