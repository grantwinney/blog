---
categories:
  - C#
  - Coding
date: 2019-11-13T17:45:00Z
description: ""
draft: false
cover:
  image: photo-1515879218367-8466d910aaa4.jpg
slug: csharp-assign-code-to-a-variable
summary: Did you know most languages have a way to pass around code to other functions, so you can call (invoke) it in other parts of your application? In C#, it's called a delegate.
tags:
  - csharp
  - coding
title: Assign C# code to a variable and then run it
---


It'd be ridiculous for a language to not have a way for you to reference a particular value, so you could pass it around in your application.

string name = "Grant";
int height = 71;
bool isMale = true;
Employee e = new Employee(name, height, isMale);

But did you know most languages have a way to pass around references to code too, so you can pass the code around and call (invoke) it in other parts of your application?

In C#, the type that lets you reference a method is called a delegate, and there are several different constructs that allow you to define a delegate... depending on what exactly you'd like to do.


Action

The Action delegate lets you reference a method that does not return a value.


Single Line

For example, you might define a single-line method that displays a message (with or without parameters).

var genericHi = new Action(() => Console.WriteLine("Hello World!"));
genericHi();  // Hello World!

var personalizedHi =
    new Action<string, string>((firstName, lastName) => Console.WriteLine($"Hello, {firstName} {lastName}!"));		
personalizedHi("Katie", "Smith");  // Hello, Katie Smith!


Multiple Lines

Or you could define a method that has several lines:

var sayHiToEveryone =
    new Action<string, string, string>((name1, name2, name3) =>
                                       {
                                           Console.WriteLine($"Hi {name1}!");
                                           Console.WriteLine($"Hi {name2}!");
                                           Console.WriteLine($"Hi {name3}!");
                                       });
sayHiToEveryone("Larry", "Curly", "Moe");


Note

You can also eliminate the new Action part, but then you can't use var, so not sure this is really any better. To each their own...

Action genericHi = () => Console.WriteLine("Hello World!");
genericHi();  // Hello World!

Action<string, string> personalizedHi =
    (firstName, lastName) => Console.WriteLine($"Hello, {firstName} {lastName}!");
personalizedHi("Katie", "Smith");  // Hello, Katie Smith!


Func

The Func delegate is very similar to Action, except that it lets you reference a method that does return a value.


Single Line

Again, you can define a single-line method with or without parameters.

var getNowMessage =
    new Func<string>(() => $"The time is now: {DateTime.Now.ToString("h:mm tt")}");
Console.WriteLine(getNowMessage());               // The time is now: 8:24 PM

var getTimeMessage =
    new Func<DateTime, string>((date) => $"The time is now: {date.ToString("h:mm tt")}");
Console.WriteLine(getTimeMessage(DateTime.Now));  // The time is now: 8:24 PM


Multiple Lines

And you can define methods with several lines:

var getDrink =
    new Func<DateTime, string>((date) =>
                               {
                                   if (date.DayOfWeek == DayOfWeek.Saturday || date.DayOfWeek == DayOfWeek.Sunday)
                                       return "🍺";
                                   else
                                       return "☕";
                               });
Console.WriteLine($"Time for a {getDrink(DateTime.Now)}.");  // Time for a ☕.


Note

And finally, you can eliminate the new Func part, but once again that prevents you from using var, so it's not any shorter.

Func<string> getNowMessage2 =
    () => $"The time is now: {DateTime.Now.ToString("h:mm tt")}";
Console.WriteLine(getNowMessage2());               // The time is now: 8:24 PM
		
Func<DateTime, string> getTimeMessage2 =
    (date) => $"The time is now: {date.ToString("h:mm tt")}";
Console.WriteLine(getTimeMessage2(DateTime.Now));  // The time is now: 8:24 PM


Try it yourself

You can play with these yourself on .NET Fiddle:


Action delegate demo (.NET Fiddle)






Func delegate demo (.NET Fiddle)





If you found this content useful, and would like to learn more about a variety of C# features, check out my CSharpDotNetFeatures repo, where you'll find links to plenty more blog posts and practical examples!

GitHub - grantwinney/CSharpDotNetExamples: Discovering and learning about the various features of the C# programming language and .NET Framework.Discovering and learning about the various features of the C# programming language and .NET Framework. - GitHub - grantwinney/CSharpDotNetExamples: Discovering and learning about the various featur…GitHubgrantwinney
bgrantwinney
