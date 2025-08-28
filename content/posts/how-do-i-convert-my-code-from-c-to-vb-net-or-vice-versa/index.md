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
If you work with the .NET Framework long enough, you may eventually find yourself tasked with converting one .NET language to another. There are hundreds of questions on Stack Overflow for converting [C# to VB.NET](https://stackoverflow.com/questions/tagged/c%23-to-vb.net), [VB.NET to C#](https://stackoverflow.com/questions/tagged/vb.net-to-c%23), and even [C# to F#](https://stackoverflow.com/questions/tagged/c%23-to-f%23) - and maybe thousands more that aren't tagged. But first, ask yourself...

---

## Does it really need to be converted?

First, consider whether or not that other code really has to be translated, because you don't __need__ to!

If it's a relatively small block of code, by all means just translate it and be done. A little upfront effort, and you'll have the undying gratitude of your team... or at least yourself when you need to revisit it six months later. But what if it's a third-party library from GitHub that's frequently updated, and you'd like to keep pulling in the latest changes? Every update shouldn't mean another several days of translation.

Or what if it's a legacy app that your company invested 10 years in, complete with hundreds of tests proving it works __(or what if it has NO tests??__ 😱 __)__ There may be thousands of hours worth of slight tweaks and bug fixes that you'll never fully incorporate into a rewrite, no matter how diligent you are. There's a time to refactor, but [there's a time to leave things alone](https://www.joelonsoftware.com/2000/04/06/things-you-should-never-do-part-i/).

****💡****_****You can have several .NET languages in one solution, as long as they're separated by project.****_

That's right. It's been possible for a [long time](https://stackoverflow.com/questions/862723/use-vb-net-and-c-sharp-in-the-same-application), but I'd bet those fancy blue shoes that even some experienced devs don't know it - it's easy to live entirely within a single language. So move that legacy VB.NET or C# code into its own project and reference it from the project you're working in. I wrote a quickie example __(pictured at the top of this post -__ [__grab the source code__](https://github.com/grantwinney/BlogCodeSamples/tree/master/CSharpAndVbNetTogether)__)__ that has a C# project referencing F# and VB.NET projects.

All you need to do is open the project with the language you want to use, and add references to the projects that use the other .NET languages. Here's a C# project with references to F# and VB.NET projects:

![netlanguagereferences](https://grantwinney.com/content/images/2018/07/netlanguagereferences.png)

If you [check out my sample app](https://github.com/grantwinney/BlogCodeSamples/tree/master/CSharpAndVbNetTogether), you'll see the modules in the other two languages are treated by C# like any other C# static class.

---

## Try translating it automatically

So you gave it a solid minute or two, but the thought of dealing with multiple languages just makes you itch all over?

If it's small enough to not warrant a whole separate project, then you might start off by trying an online translation service. Here's a few, but unless it's a very simple piece of code you're likely to still have a few errors to resolve. Still, they might get you pretty far along.

- [Telerik Code Converter](http://converter.telerik.com/)
- [developerFusion Code Converter](https://www.developerfusion.com/tools/)
- [CodeTranslator](https://www.carlosag.net/tools/codetranslator/) by Carlos Aguilar Mares
- [DotNetSpider Code Converters](http://www.dotnetspider.com/convert/)
- [Roslyn Code Converter](https://codeconverter.icsharpcode.net/) by SharpDevelop

People seem to have mixed experiences with the automatic translators, at least the online ones. Although C# and VB.NET are closer in functionality than they have been in the past, sometimes things just don't translate nicely.

If those don't work, here are some other tools. These ones need to be installed, and some of them cost money, but they'll almost certainly work more reliably. To use decompilers, you'll need to compile the source code you want to translate by pasteing it into Visual Studio, building it, and checking the `bin` folder for the DLL file. Then the tool can decompile it into the target language for you.

- [Telerik JustDecompile](https://www.telerik.com/products/decompiler.aspx) decompiles to C# or VB.NET __(free)__
- [ILSpy](https://github.com/icsharpcode/ILSpy/releases) can decompile assemblies to C#. __(free)__
- [JetBrains dotPeek](http://www.jetbrains.com/decompiler/) decompiles to C# as well. __(free)__
- [DevExtras .NET CodeReflect](http://www.devextras.com/decompiler/) decompiles to C# or VB.NET. __(free)__
- [.NET Reflector](https://www.red-gate.com/products/dotnet-development/reflector/) can decompile assemblies into C# or VB.NET, so compile the source language and then decompile into the target language. __($95 - $195)__
- Tangible Software Solutions has converters for [VB.NET to C#](https://www.tangiblesoftwaresolutions.com/product_details/vb-to-csharp-converter.html), [C# to VB.NET](https://www.tangiblesoftwaresolutions.com/product_details/csharp-to-vb-converter.html), and C++ and Java too. __($119 - $499)__
- [VBConversions](http://www.vbconversions.com/) converts VB.Net to C# and if the testimonials they post are honest, then it's pretty impressive... 110,000 lines of code converted perfectly? __($50/month - $500)__

---

## And if all else fails?

If the automatic translators fail, the only options left are to hire someone or get your hands dirty. Here's a couple books that might help. The second one is a few years old, but the first one looks intriguing.

- [C#-Visual Basic Bilingual Dictionary: Visual Studio 2015 Edition](https://www.amazon.com/gp/product/0692433694/ref=as_li_qf_asin_il_tl?ie=UTF8&tag=gwin04-20&creative=9325&linkCode=as2&creativeASIN=0692433694&linkId=da35c09eb1e79b589bc9cb04ecbe5179), by Tim Patrick
- [Beginning ASP.NET 4.5: in C# and VB](https://www.amazon.com/Beginning-ASP-NET-4-5-C-VB/dp/1118311809), by Imar Spaanjaars

And here's a few more links to documentation:

- [.NET Documentation - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/index) with guides and examples for C#, VB.NET, F#, etc.
- [Languages features in C# and VB - dotnet/roslyn Wiki](https://github.com/dotnet/roslyn/wiki/Languages-features-in-C%23-6-and-VB-14)
- [101 LINQ samples - Code Samples | Microsoft Docs](https://docs.microsoft.com/en-us/samples/dotnet/try-samples/101-linq-samples/)
- [Converting C# knowledge to VB.NET - any potential problems?](https://stackoverflow.com/questions/1337253/converting-c-sharp-knowledge-to-vb-net-any-potential-problems) (an old Stack Overflow thread, but may still be helpful)