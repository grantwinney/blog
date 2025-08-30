---
date: "2019-07-08T21:19:14Z"
description: ""
draft: false
slug: ghostsharp
title: GhostSharp
---
![](https://github.com/grantwinney/GhostSharp/actions/workflows/dotnet.yml/badge.svg) ![](https://img.shields.io/nuget/v/GhostSharp.svg) ![](https://www.codefactor.io/repository/github/grantwinney/ghostsharp/badge) ![](https://www.codetriage.com/grantwinney/ghostsharp/badges/users.svg) ![](https://img.shields.io/github/languages/top/grantwinney/GhostSharp.svg)

GhostSharp is a C# [wrapper](https://grantwinney.com/what-is-an-api-wrapper/) around the [Ghost API](https://docs.ghost.org/content-api/), a RESTful JSON API built into the core of the [Ghost blogging platform](https://ghost.org/). I wrote it as a personal challenge, to get acquainted with the Ghost API, but also to familiarize myself with [C# 7.x](https://docs.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-7), [8.x](https://docs.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-8), [.NET Core](https://docs.microsoft.com/en-us/dotnet/core/about), and [.NET Standard](https://devblogs.microsoft.com/dotnet/introducing-net-standard/).

Check out the [official Ghost API docs](https://docs.ghost.org/api/) here.

## Source Code

The [source code for GhostSharp](https://github.com/grantwinney/GhostSharp) is available on GitHub. It's covered by a full suite of unit and integration tests, which you can configure and run against your Ghost instance, or just [check out the build history here](https://travis-ci.org/github/grantwinney/GhostSharp/builds). Instructions on using it are in the [readme](https://github.com/grantwinney/GhostSharp), or you could [install it via NuGet](https://www.nuget.org/packages/GhostSharp/) (recommended).

I've implemented those endpoints that are [marked as stable](https://docs.ghost.org/admin-api/#endpoints) _(posts, pages, images, themes, site)_. No sense covering things that are just going to change...

## Contributions / Issues

[Open an issue](https://github.com/grantwinney/GhostSharp/issues/new), and include errors, unexpected behavior, steps to reproduce, etc. **The more details, the better!** Feel free to [open a PR](https://github.com/grantwinney/GhostSharp/compare) if you figure out how to fix it. I don't always have a ton of time, but I'll address issues as time permits.