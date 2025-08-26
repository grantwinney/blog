---
categories:
- Tools of the Trade
date: "2018-07-18T04:41:38Z"
description: ""
draft: false
cover:
  image: pexels-mateusz-dach-409479.jpg
slug: how-do-i-convert-my-code-from-c-to-vb-net-or-vice-versa
summary: If you work with the .NET Framework long enough, you may eventually find
  yourself tasked with converting one language to another, either by request or necessity.
  But conversion isn't always necessary - it's possible (and easy!) to have one solution
  with multiple languages.
tags:
- Tools of the Trade
title: Convert code from C# to VB.NET and back
---


If you work with the .NET Framework long enough, you may eventually find yourself tasked with converting one .NET language to another. There are hundreds of questions on Stack Overflow for converting C# to VB.NET, VB.NET to C#, and even C# to F# - and maybe thousands more that aren't tagged. But first, ask yourself...


Does it really need to be converted?

First, consider whether or not that other code really has to be translated, because you don't need to!

If it's a relatively small block of code, by all means just translate it and be done. A little upfront effort, and you'll have the undying gratitude of your team... or at least yourself when you need to revisit it six months later. But what if it's a third-party library from GitHub that's frequently updated, and you'd like to keep pulling in the latest changes? Every update shouldn't mean another several days of translation.

Or what if it's a legacy app that your company invested 10 years in, complete with hundreds of tests proving it works (or what if it has NO tests?? 😱 ) There may be thousands of hours worth of slight tweaks and bug fixes that you'll never fully incorporate into a rewrite, no matter how diligent you are. There's a time to refactor, but there's a time to leave things alone.

💡You can have several .NET languages in one solution, as long as they're separated by project.

That's right. It's been possible for a long time, but I'd bet those fancy blue shoes that even some experienced devs don't know it - it's easy to live entirely within a single language. So move that legacy VB.NET or C# code into its own project and reference it from the project you're working in. I wrote a quickie example (pictured at the top of this post - grab the source code) that has a C# project referencing F# and VB.NET projects.

All you need to do is open the project with the language you want to use, and add references to the projects that use the other .NET languages. Here's a C# project with references to F# and VB.NET projects:

If you check out my sample app, you'll see the modules in the other two languages are treated by C# like any other C# static class.


Try translating it automatically

So you gave it a solid minute or two, but the thought of dealing with multiple languages just makes you itch all over?

If it's small enough to not warrant a whole separate project, then you might start off by trying an online translation service. Here's a few, but unless it's a very simple piece of code you're likely to still have a few errors to resolve. Still, they might get you pretty far along.

 * Telerik Code Converter
 * developerFusion Code Converter
 * CodeTranslator by Carlos Aguilar Mares
 * DotNetSpider Code Converters
 * Roslyn Code Converter by SharpDevelop

People seem to have mixed experiences with the automatic translators, at least the online ones. Although C# and VB.NET are closer in functionality than they have been in the past, sometimes things just don't translate nicely.

If those don't work, here are some other tools. These ones need to be installed, and some of them cost money, but they'll almost certainly work more reliably. To use decompilers, you'll need to compile the source code you want to translate by pasteing it into Visual Studio, building it, and checking the bin folder for the DLL file. Then the tool can decompile it into the target language for you.

 * Telerik JustDecompile decompiles to C# or VB.NET (free)
 * ILSpy can decompile assemblies to C#. (free)
 * JetBrains dotPeek decompiles to C# as well. (free)
 * DevExtras .NET CodeReflect decompiles to C# or VB.NET. (free)
 * .NET Reflector can decompile assemblies into C# or VB.NET, so compile the source language and then decompile into the target language. ($95 - $195)
 * Tangible Software Solutions has converters for VB.NET to C#, C# to VB.NET, and C++ and Java too. ($119 - $499)
 * VBConversions converts VB.Net to C# and if the testimonials they post are honest, then it's pretty impressive... 110,000 lines of code converted perfectly? ($50/month - $500)


And if all else fails?

If the automatic translators fail, the only options left are to hire someone or get your hands dirty. Here's a couple books that might help. The second one is a few years old, but the first one looks intriguing.

 * C#-Visual Basic Bilingual Dictionary: Visual Studio 2015 Edition, by Tim Patrick
 * Beginning ASP.NET 4.5: in C# and VB, by Imar Spaanjaars

And here's a few more links to documentation:

 * .NET Documentation - Microsoft Docs with guides and examples for C#, VB.NET, F#, etc.
 * Languages features in C# and VB - dotnet/roslyn Wiki
 * 101 LINQ samples - Code Samples | Microsoft Docs
 * Converting C# knowledge to VB.NET - any potential problems? (an old Stack Overflow thread, but may still be helpful)
