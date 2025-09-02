---
categories:
- GraphQL
date: "2019-09-21T02:48:40Z"
description: ""
draft: false
cover:
  image: graphql_banner.jpg
slug: what-is-graphql-and-how-does-it-differ-from-rest
summary: GraphQL is an alternative for REST, not a replacement. Let's take a brief look at how they differ.
tags:
- graphql
title: What is GraphQL and how does it differ from REST?
---
Finding a new [API](https://grantwinney.com/what-is-an-api/) can be like discovering a gateway to a vast amount of data that might be otherwise inaccessible. A couple of my favorites have led to [photos from the Mars Rover](https://grantwinney.com/what-is-nasa-api/) and [300 year old newspapers](https://grantwinney.com/searching-historical-newspapers-with-the-chronicling-america-api/). These APIs are often implemented using [REST](https://en.wikipedia.org/wiki/Representational_state_transfer), the standard for making web resources accessible for over a decade, but there's a different way to access resources called [GraphQL](https://en.wikipedia.org/wiki/GraphQL).

The first time I heard about GraphQL was a few years ago in some article about Facebook, but I didn't pay much attention to it at the time. Thanks to frequent data breaches, one doesn't really need an API to access Facebook's data.. but I digress.

I didn't realize until recently that Facebook actually developed it and, a few years ago, open sourced it too. Let's take a closer look at what it is and how we might use it.

## GraphQL vs REST

As much as we'd like to think there's one "best" way to do things, GraphQL is an alternative for REST, not a replacement. And while [there's quite a few differences between them](https://phil.tech/api/2017/01/24/graphql-vs-rest-overview), the major difference is that GraphQL lets us build a query to get exactly (and only) the data we're interested in.

An API that implements the REST interface allows us to (for example) very easily `GET` some data about an entity. By default, we get the whole shebang. If we can limit or otherwise customize the returned dataset somehow, it's only because the developers wrote code to explicitly support those limits and customizations. How that looks will differ with every API.

Take the [Ghost API](https://docs.ghost.org/content-api) as an example, which is built into the blog engine I use for this site. You can request data on individual posts, authors, etc, which is all standard fare. On top of that though, the devs provided a few query parameters to affect the returned data:

- `include` returns _more_ data, like full author details for a post
- `fields` returns _less_ data, by specifying which fields should be returned
- `formats` returns _more_ data, by returning data in multiple formats
- `filter` returns _less_ data, by filtering by certain attributes
- `limit` and `page` return _less_ data by implementing paging
- `order` doesn't even filter data, but it affects paging results so it's tacked on

Each of those items had to be explicitly coded, and while the Ghost API offers more customization than a lot of other APIs I've seen, they can only offer so much. If I want to "filter" by an attribute they don't support, I have to request more data than I need and filter it out locally. If I want to "include" some other entity they didn't plan for, I have to make multiple requests and stitch things together client side.

The flexibility in GraphQL is that it allows (forces, really) a client to create their own query to get _just_ the data they want, in _just_ the way they want it. And it also provides certain tools to enable the server to provide that data and only that data. In other words, GraphQL out-of-the-box returns the smallest amount of data needed, whereas REST returns the largest.

There seems to be a pretty rich toolset for GraphQL, including:

- Some sort of [IDE](https://github.com/graphql/graphiql) in the browser to play around with GraphQL
- Server libraries in [C#](https://graphql.org/code/#c-net), [Python](https://graphql.org/code/#python), and more _(what's a server library?)_
- GraphQL clients for [C#](https://graphql.org/code/#c-net-1) and [Python](https://graphql.org/code/#python-1) _(what do they mean by client??)_
- Various other [tools](https://graphql.org/code/#tools) _(what do these do?!?)_

Not to mention there's an entire [GraphQL spec](https://github.com/graphql/graphql-spec) to check out and, uh, [_this_](https://github.com/chentsulin/awesome-graphql). 😵

Lastly, there's a [quick overview of GraphQL in 15 minutes](https://www.youtube.com/watch?v=VjXb3PRL9WI) on YouTube, and here's a piece I wrote about [accessing the GraphQL API](https://grantwinney.com/using-graphiql-to-access-a-graphql-api/).
