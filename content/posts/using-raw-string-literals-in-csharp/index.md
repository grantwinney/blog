---
categories:
  - C# 11
  - .NET 7
  - C#
  - Coding
date: 2024-12-14T22:32:43Z
description: ""
draft: false
cover:
  image: tara-evans-G-AQWUTgMZo-unsplash.jpg
slug: using-raw-string-literals-in-csharp
summary: C# 11 added raw string literals, not a life-altering new feature, but they could be useful in the right circumstances. Let's see how to use them.
tags:
  - Coding
  - CSharp11
  - CSharp
  - DotNet7
title: Using Raw String Literals in C# 11 / .NET 7
---
Some of the many additions to C# and the .NET Framework are huge (like LINQ or async), while others are much smaller. I'd definitely place raw string literals in the latter group, but then maybe I'm missing something. They're still worth a look though.

Before we dig into them, let's take a brief look at what we had before that, and then see what new things they bring to the table.

> The code in this post is available on [GitHub](https://github.com/grantwinney/CSharpDotNetFeatures/tree/master/C%23%2011/RawStringLiterals?ref=grantwinney.com), for you to use, extend, or just follow along while you read... and hopefully discover something new along the way!

## Quoted Strings

Here's a normal, quoted string. They allow [escaped sequences](https://learn.microsoft.com/en-us/dotnet/standard/base-types/character-escapes-in-regular-expressions) such as tabs, newlines, quotes, etc. If we want a quote in the string, it has to be escaped:

```csharp
Console.WriteLine("Windows dir: \t\t \"C:\\Windows\"");

// Windows dir:             "C:\Windows"
```

Normal string allows escape sequences

## Verbatim Strings

Adding the `@` character to the left of a string causes escaped sequences to be ignored. If we want a quote in the string, it has to be doubled:

```csharp
Console.WriteLine(@"Windows dir: \t\t ""C:\Windows""");

// Windows dir: \t\t "C:\Windows"
```

Verbatim strings ignore escape sequences

## String Interpolation

I've written separately about [string interpolation](https://grantwinney.com/using-string-interpolation-to-craft-readable-strings/), but adding a `$` to the left of a string allows us to specify variables inline. If we want a curly braces in the string, it has to be doubled. Otherwise it behaves like a normal quoted string.

```csharp
Console.WriteLine($"Today's date is: \t\t \"{{{DateTime.Now}}}\"");

// Today's date is:                 "{12/13/2024 9:31:09 PM}"
```

String interpolation allows inline variables

## Verbatim String Interpolation

I'm not sure whether to call this a verbatim interpolated string or an interpolated verbatim string. Or a stringy verbapolation (lol). Inline variables are allowed, and double curly braces are required to show a curly brace, but otherwise they behave like a verbatim string and ignore escaped sequences.

```csharp
Console.WriteLine($@"Today's date is: \t\t ""{{{DateTime.Now}}}""");

// Today's date is: \t\t "{12/13/2024 9:31:09 PM}"
```

Verbapolated strings mix up the behaviors

## Raw String Literals (single-line)

By using triple (or more) quotes at the start and end of a string, we've defined a raw string literal. It behaves mostly like a verbatim string, with some special rules for quotes:

- They're allowed in the string, as long as they don't occur in sequences longer than whatever we started or ended the string with
- They can't butt right up to the quotes that start/end the string (hence the space at the end of the string below)

```csharp
Console.WriteLine("""Today's date is: \t "{DateTime.Now}" """);
// Today's date is: "{DateTime.Now}"

Console.WriteLine(""""Today's date is: \t """{DateTime.Now}""" """");
// Today's date is: """{DateTime.Now}"""
```

By adding the `$` string interpolation character, we can insert variables inline again. The main difference is that adding a literal curly brace to the string now requires increasing the number of `$` at the start of the string. Any number of them _less_ than that in the string is assumed to be a literal brace. I can't figure out why that behavior changed, but I assume they had a reason. 🤷‍♂️

```csharp
Console.WriteLine($"""Today's date is: \t "{DateTime.Now}" """);
// Today's date is: "12/13/2024 9:31:09 PM"

Console.WriteLine($$"""Today's date is: \t "{{{DateTime.Now}}}" """);
// Today's date is: "{12/13/2024 9:31:09 PM}"

Console.WriteLine($$$"""Today's {{date}} is: \t "{{{{{DateTime.Now}}}}}" """);
// Today's {{date}} is: "{{12/13/2024 9:31:09 PM}}"
```

## Raw String Literals (multi-line)

Here's where the behavior really changes, although as I alluded to at the beginning of this article, this doesn't seem like a earth-shattering addition to me. Am I missing something?

A raw string literal can span multiple lines, and doesn't require new lines in the string. In fact, they wouldn't be interpreted anyway, since the behavior is largely like a verbatim string.

```csharp
// Multi-line raw string literals follow the same rules as above;
// None of the lines may appear to the left of the closing set of quotes

Console.WriteLine("""
    Now this is interesting!
            Is this indented? \t "{DateTime.Now}"
    """);

// Now this is interesting!
//         Is this indented? \t "{DateTime.Now}"
```

Raw string literals behave largely like verbatim strings

If we add the `$` to the start though, then we can add variables inline. And while escaped characters are ignored when placed directly in the string, we can store them in a separate variable and let interpolation do the work for us. That's a rawverbapolated string, for those keeping track.

```csharp
var tab = $"\t";
Console.WriteLine($"""
    Now this is interesting!\n\n\n
            Is this indented? {tab} "{DateTime.Now}"
    """);

// Now this is interesting!\n\n\n
//         Is this indented?        "12/13/2024 9:44:39 PM"
```

We can increase the number of quotes and dollar signs around the string too, just like before, when we have a need to display multiple quotes or literal curly braces inside the raw string literal:

```csharp
Console.WriteLine($"""""
    Now this is interesting!
            Is this indented? {tab} """"{DateTime.Now}""""
    """"");

// Now this is interesting!
//         Is this indented?        """"12/13/2024 9:44:39 PM""""
```

```csharp
Console.WriteLine($$"""
    Now this is interesting!
            Is this indented? {{tab}} "{{{DateTime.Now}}}"
    """);

// Now this is interesting!
//         Is this indented?        "{12/13/2024 9:44:39 PM}"
```

And finally, we can use raw string literals in a whole lot of cases other than just writing out lines to a console. I can imagine a few, one of them being a multi-line message box like this:

![](https://grantwinney.com/content/images/2024/12/image-4.png)

## Learning More

Microsoft has a lot more to say about raw string literals, and strings in general, than what I can post here. They've spread info out over a few different docs, but if you're interested then they're definitely worth a looksie:

- [What's new in C# 11 | Raw string literals](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-11#raw-string-literals)
- [Strings - C# | Raw string literals](https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/#raw-string-literals)
- [Built-in reference types - C# reference | String literals](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/reference-types#string-literals)
- [$ - string interpolation | Interpolated raw string literals](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated#interpolated-raw-string-literals)

If you found this content useful, and would like to learn more about a variety of [C#](https://grantwinney.com/tag/csharp/) features, check out my [CSharpDotNetFeatures repo](https://github.com/grantwinney/CSharpDotNetFeatures), where you'll find links to plenty more blog posts and practical examples!