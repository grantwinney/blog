+++
categories = ["GraphQL"]
date = 2019-09-21T02:48:40Z
description = ""
draft = false
image = "__GHOST_URL__/content/images/2019/09/graphql_banner.jpg"
slug = "what-is-graphql-and-how-does-it-differ-from-rest"
summary = "GraphQL is an alternative for REST, not a replacement. Let's take a brief look at how they differ."
tags = ["GraphQL"]
title = "What is GraphQL and how does it differ from REST?"

+++


Finding a new API can be like discovering a gateway to a vast amount of data that might be otherwise inaccessible. A couple of my favorites have led to photos from the Mars Rover and 300 year old newspapers. These APIs are often implemented using REST, the standard for making web resources accessible for over a decade, but there's a different way to access resources called GraphQL.

The first time I heard about GraphQL was a few years ago in some article about Facebook, but I didn't pay much attention to it at the time. Thanks to frequent data breaches, one doesn't really need an API to access Facebook's data.. but I digress.

I didn't realize until recently that Facebook actually developed it and, a few years ago, open sourced it too. Let's take a closer look at what it is and how we might use it.


GraphQL vs REST

As much as we'd like to think there's one "best" way to do things, GraphQL is an alternative for REST, not a replacement. And while there's quite a few differences between them, the major difference is that GraphQL lets us build a query to get exactly (and only) the data we're interested in.

An API that implements the REST interface allows us to (for example) very easily GET some data about an entity. By default, we get the whole shebang. If we can limit or otherwise customize the returned dataset somehow, it's only because the developers wrote code to explicitly support those limits and customizations. How that looks will differ with every API.

Take the Ghost API as an example, which is built into the blog engine I use for this site. You can request data on individual posts, authors, etc, which is all standard fare. On top of that though, the devs provided a few query parameters to affect the returned data:

 * include returns more data, like full author details for a post
 * fields returns less data, by specifying which fields should be returned
 * formats returns more data, by returning data in multiple formats
 * filter returns less data, by filtering by certain attributes
 * limit and page return less data by implementing paging
 * order doesn't even filter data, but it affects paging results so it's tacked on

Each of those items had to be explicitly coded, and while the Ghost API offers more customization than a lot of other APIs I've seen, they can only offer so much. If I want to "filter" by an attribute they don't support, I have to request more data than I need and filter it out locally. If I want to "include" some other entity they didn't plan for, I have to make multiple requests and stitch things together client side.

The flexibility in GraphQL is that it allows (forces, really) a client to create their own query to get just the data they want, in just the way they want it. And it also provides certain tools to enable the server to provide that data and only that data. In other words, GraphQL out-of-the-box returns the smallest amount of data needed, whereas REST returns the largest.

There seems to be a pretty rich toolset for GraphQL, including:

 * Some sort of IDE in the browser to play around with GraphQL
 * Server libraries in C#, Python, and more (what's a server library?)
 * GraphQL clients for C# and Python (what do they mean by client??)
 * Various other tools (what do these do?!?)

Not to mention there's an entire GraphQL spec to check out and, uh, this. 😵

Here's a nice introductory video by Scott Tolinski. It's a quick overview of GraphQL in 15 minutes.

If you'd like to see me use it to access the GitHub API (which uses GraphQL), I wrote a separate post here:

Using the GraphiQL IDE to access a GraphQL APIGraphQL is bundled with GraphiQL, which lets us run queries right in the browser! Let’s see how GitHub uses it and try kicking the tires.Grant WinneyGrant