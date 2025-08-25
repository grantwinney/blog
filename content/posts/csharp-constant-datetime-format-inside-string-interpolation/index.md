+++
categories = ["C#", "Coding", "C# 6", "Date-Time Handling"]
date = 2019-04-04T20:06:50Z
description = ""
draft = false
image = "https://images.unsplash.com/photo-1501139083538-0139583c060f?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=2000&fit=max&ixid=eyJhcHBfaWQiOjExNzczfQ"
slug = "csharp-constant-datetime-format-inside-string-interpolation"
summary = "I was upgrading some code to use string interpolation, a feature introduced in C# 6, when I ran into a small snag with DateTimes and a format string stored as a constant."
tags = ["C#", "Coding", "C# 6", "Date-Time Handling"]
title = "Using a constant as a DateTime format inside string interpolation"

+++




The code in this post is available on GitHub, for you to use, expand upon, or just follow along while you read... and hopefully discover something new!



I was upgrading some code to use string interpolation, a feature introduced in C# 6, when I ran into a small snag with DateTimes and format strings.

The original code looked something like this:

var name = "Grant";
var message = string.Format("Hello {0}, the date is {1:MM/dd/yyyy}.", name, DateTime.Now);
Console.WriteLine(message);

// output: Hello Grant, the date is 04/04/2019.


Updating the string.Format to use string interpolation was straight-forward:

var name = "Grant";
var message = $"Hello {name}, the date is {DateTime.Now:MM/dd/yyyy}.";
Console.WriteLine(message);

// output: Hello Grant, the date is 04/04/2019.


But then I tried to move the format string into a constant so I could use it in several places, which didn't work:

const string DATE_FORMAT = "MM/dd/yyyy";  // ignored

var name = "Grant";
var message = $"Hello {name}, the date is {DateTime.Now:DATE_FORMAT}.";
Console.WriteLine(message);

// output: Hello Grant, the date is DATE_9OR4AT.


The output looks funky because the F and M in "DATE_FORMAT" are valid formats (for tenths of a second and month, respectively). It took a few minutes to realize it, but I just had to use the overloaded ToString() method that accepts a format string:

const string DATE_FORMAT = "MM/dd/yyyy";

var name = "Grant";
var message = $"Hello {name}, the date is {DateTime.Now.ToString(DATE_FORMAT)}.";
Console.WriteLine(message);

// output: Hello Grant, the date is 04/04/2019.


And what is string interpolation? According to MSDN, it's just syntactic sugar around the traditional String.Format method:

At compile time, an interpolated string is typically transformed into a String.Format method call. That makes all the capabilities of the string composite formatting feature available to you to use with interpolated strings as well.

In general, it looks better, is clearer to read, and is more convenient too.


Other Resources

Want to learn more? Start here:

 * $ - string interpolation (C# Reference)
 * String interpolation in C#
 * Back to Basics: String Interpolation in C#