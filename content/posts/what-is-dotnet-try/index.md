---
categories:
- Documentation
- .NET
- Tools of the Trade
date: "2021-05-29T21:40:16Z"
description: ""
draft: false
cover:
  image: photo-1498050108023-c5249f4df085.jpg
slug: what-is-dotnet-try
summary: Do you prefer reading or doing? How about both? DotNet Try pulls in C# code
  from your project and turns your docs into an interactive experience.
tags:
- Documentation
- .NET
- Tools of the Trade
title: What is DotNet Try?
---
Some of my favorite sources of documentation are the ones that include interactive code snippets you can run right on the site. For example, any of the fantastic MDN web docs, like this one for [Array.prototype.map()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map). They've really figured out how to do documentation right. 👍

But this isn't about MDN, it's about another tool I stumbled across while I was looking for some resources on LINQ. Microsoft has a tool for creating interactive C# documentation using markdown files, called DotNet Try (or Try .NET, depending on where on their site you're reading about it.. naming things is hard).

## Installing DotNet Try

Before we delve into it, you'll have to install it. The details are [here](https://github.com/dotnet/try/blob/main/DotNetTryLocal.md).

Download the SDKs like they suggest. .NET Core 3.0 is outdated now, but you can find the latest versions [here](https://dotnet.microsoft.com/download/dotnet). Before you bother downloading anything though, double-check your apps... you may already have what you need.

![](https://grantwinney.com/content/images/2021/05/image-12.png)

![500](https://grantwinney.com/content/images/2021/05/image-11.png)

When that's done, you can use the `dotnet` command line tool to [install other tools](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet#global-tool-path-and-local-tools-commands), like DotNet Try. Open the command line of your choice and run this command:

```none
dotnet tool update -g Microsoft.dotnet-try
```

## Kicking the Tires

Now go back to their [help doc](https://github.com/dotnet/try/blob/main/DotNetTryLocal.md#getting-started) and check out the whole "Getting Started" section if you want to get more familiar with it. I got a __"your connection is not private message",__ but clicking "proceed to localhost" got me to something much more interesting.

![](https://grantwinney.com/content/images/2021/05/image-7.png)

Play around with it a bit, change the code, progress through their examples, try different things out.. maybe break it. lol️

## Behind the Curtain

To understand what's going on, find the directory where you ran the `dotnet try demo` command and check out the markdown files. Let me just say that I really like they're using markdown, and not html or (gasp) some one-off syntax.

__(If you're not familiar with markdown, find a__ [__good tutorial__](https://www.howtogeek.com/448323/what-is-markdown-and-how-do-you-use-it/) __and get familiar with it. It's used on GitHub as well as a lot of forums, commenting systems, blogs, etc. But I digress...)__

![](https://grantwinney.com/content/images/2021/05/image-9.png)

Open the QuickStart.md file in VSCode or some other editor, change the markdown a bit, and refresh the page in your browser. You should see the changes. So they're parsing the markdown into HTML... neat but not earth shattering.

![](https://grantwinney.com/content/images/2021/05/image-10.png)

The cool bit is what they call a "code fence". You specify a source file to read from, and give it the name of region in the file, and it parses whatever's inside the region, displays it on the page, and makes it executable.

````none
```csharp --source-file ./Snippets/Program.cs --project ./Snippets/Snippets.csproj --region run1
```
````

So how's that work? As far as I can tell, displaying and running your code snippet are two different steps.

When you refresh the page, it reads the source file, extracts whatever region you specify, and tosses that on the page, even if it's invalid. You do get some red squigglies if something's wrong though.

When you __run__ the sample code, it compiles the project and actually runs it - hence the need for the switch statement in the Main method. If it fails to compile, you get syntax errors in the browser.

![](https://grantwinney.com/content/images/2021/05/image-17.png)

Fix the code (in my case, an invalid method name and missing `using` directive), and press Run again. Good to go! Oddly, I realized you can't just insert `using` directives directly into the web page. I mean, sure that'd be invalid in your c# app so it makes sense, but I guess that makes the point that showing just one part of your code like this does make it confusing as to what you can and cannot do in the browser.

![](https://grantwinney.com/content/images/2021/05/image-18.png)

## Other Samples

They've got [some other samples](https://github.com/dotnet/try-samples) available in a different repo, so be sure to check those out too if you're curious. I stumbled on this project looking for some good LINQ tutorials, so let's try that one out.

Clone the above repo somewhere on disk, change to the directory with 101 LINQ samples, and run the `dotnet try` command. That should open the samples, easy peasy.

![](https://grantwinney.com/content/images/2021/05/image-19.png)

![](https://grantwinney.com/content/images/2021/05/image-20.png)

There's not a ton in there, but there are some samples of new additions to the C# 7 and 8 specs, so might check those out later...

I think when I've got time, I'll write separate posts trying to make this work with a WinForms project, as well as a .NET Core project, perhaps with an API. [Swagger](https://docs.microsoft.com/en-us/aspnet/core/tutorials/web-api-help-pages-using-swagger?view=aspnetcore-5.0) is a nice tool for exposing the top layers of an API, but this might be a good way to document tricky or useful methods, or even expose them for use to a dev team without having to open the project separately. Hmm... the wheels are turning.

## Caveats

The only thing that bugged me was the very limited options for specifying which code you want to display. It'd be nice to just specify a method name, instead of having to clutter up your source code with region names to support your documentation.