+++
date = 2019-07-08T21:19:14Z
description = ""
draft = false
slug = "ghostsharp"
title = "GhostSharp"

+++


<style type="text/css">
    #badges img { display: inline; }
</style>    

<p id="badges" style="padding-top:10px;">
    <a href="https://github.com/grantwinney/GhostSharp"><img src="https://github.com/grantwinney/GhostSharp/actions/workflows/dotnet.yml/badge.svg"/></a>
    <a href="https://opensource.org/licenses/MIT"><img src="https://img.shields.io/badge/License-MIT-green.svg" alt="License: MIT"></a>
    <a href="https://www.nuget.org/packages/GhostSharp"><img src="https://img.shields.io/nuget/v/GhostSharp.svg" alt="NuGet"></a>
    <a href="https://www.codefactor.io/repository/github/grantwinney/ghostsharp"><img src="https://www.codefactor.io/repository/github/grantwinney/ghostsharp/badge" alt="CodeFactor"></a>
    <!--<a href="https://www.codetriage.com/grantwinney/ghostsharp"><img src="https://www.codetriage.com/grantwinney/ghostsharp/badges/users.svg" alt="Open Source Helpers"></a>-->
    <img src="https://img.shields.io/github/languages/top/grantwinney/GhostSharp.svg" alt="Language">
    <a href="https://twitter.com/intent/tweet?url=https%3A%2F%2Fgithub.com%2Fgrantwinney%2FGhostSharp&text=GhostSharp,%20a%20C%23%20Wrapper%20for%20the%20Ghost%20API&hashtags=tryghost,api"><img src="https://img.shields.io/twitter/url/http/shields.io.svg" /></a>
</p>

GhostSharp is a C# [wrapper](__GHOST_URL__/what-is-an-api-wrapper-and-how-do-i-write-one/) around the [Ghost API v3.0](https://ghost.org/docs/api/v3/), a RESTful JSON API built into the core of the [Ghost blogging platform](https://ghost.org/). I wrote it as a personal challenge, to get acquainted with the Ghost API, but also to familiarize myself with [C# 7.x](https://docs.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-7), [8.x](https://docs.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-8), [.NET Core](https://docs.microsoft.com/en-us/dotnet/core/about), and [.NET Standard](https://devblogs.microsoft.com/dotnet/introducing-net-standard/).

Check out the [official Ghost API docs](https://docs.ghost.org/api/), and read about my own experience [implementing it](__GHOST_URL__/gems-ci-and-the-ghost-content-api-20/)  [the first time around](__GHOST_URL__/ghost-admin-api-v20/).

## Source Code

The [source code for GhostSharp](https://github.com/grantwinney/GhostSharp) is available on GitHub. It's covered by a full suite of unit and integration tests, which you can configure and run against your Ghost instance, or just [check out the build history here](https://travis-ci.org/github/grantwinney/GhostSharp/builds). Instructions on using it are in the [readme](https://github.com/grantwinney/GhostSharp), or you could [install it via NuGet](https://www.nuget.org/packages/GhostSharp/) (recommended).

I've implemented those endpoints that are [marked as stable](https://docs.ghost.org/api/admin/#endpoints)  _(posts, pages, images, themes, site)_. No sense covering things that are just going to change...

## Contributions / Issues

[Open an issue](https://github.com/grantwinney/GhostSharp/issues/new), and include errors, unexpected behavior, steps to reproduce, etc. **The more details, the better!** Feel free to [open a PR](https://github.com/grantwinney/GhostSharp/compare) if you figure out how to fix it. I don't always have a ton of time, but I'll address issues as time permits.

