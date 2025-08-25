+++
categories = ["GraphQL"]
date = 2019-09-26T03:38:30Z
description = ""
draft = false
image = "__GHOST_URL__/content/images/2019/09/graphql_banner-1.jpg"
slug = "using-graphiql-to-access-a-graphql-api"
summary = "GraphQL is bundled with GraphiQL, which lets us run queries right in the browser! Let's see how GitHub uses it and try kicking the tires."
tags = ["GraphQL"]
title = "Using the GraphiQL IDE to access a GraphQL API"

+++


In my last post, I just wanted to understand what GraphQL is versus REST. I learned that it's about flexibility, and getting exactly what you need in the format you need it. Now I want to look at an actual implementation.

Facebook spent years developing GraphQL and then open-sourced it. When GitHub began moving to a new version of their API several years ago, they migrated from REST to GraphQL. Their reasoning is very similar to what I've read elsewhere and experienced myself.

Our responses were bloated and filled with all sorts of *_url hints in the JSON responses to help people continue to navigate through the API to get what they needed. Despite all the information we provided, we heard from integrators that our REST API also wasn’t very flexible. It sometimes required two or three separate calls to assemble a complete view of a resource. It seemed like our responses simultaneously sent too much data and didn’t include data that consumers needed.


Queries

One of the tools available with GraphQL is GraphiQL, which allows users of your API to design queries right in the browser and see immediate results. This is a tremendous time-saver!

With REST, I've always used Postman to manage my queries without having to have a fullblown app in place from the get-go, but it still involves trial and error. I read the API's docs, figure out what to call and how, get the result and inspect it, make adjustments, make more calls to other endpoints, etc. Occasionally, an API provider produces their own "API Explorer" of sorts, if we're lucky.

GraphiQL is a ready-to-go "API Explorer". It integrates API docs right into the experience with "typeaheads" (similar to intellisense in Visual Studio), which helps us figure out what to query and then shows us the results. Let's try out GitHub's API explorer.

 * Allow it to access your GitHub account... even though it is GitHub. 🤨
 * Click the "Execute Query" triangle in the upper-left to run the default query... info about you!
 * Click the "Docs" button on the right side to view the API documentation. Note the two root types - query and mutation. A query is similar to a REST GET, while mutation is similar to POST or DELETE. Stick with query for now.
 * As you drill down, you'll see objects to query, parameters to restrict your queries, and other child objects. It's like you're getting to browse their database!

Here's a few queries I tried running:


Mutations

Once you've run a few queries, try out mutations. You've already granted access to everything in your account to the tool, so you can update (mutate) pretty much anything in your account. Here's a short screen capture where I'm performing two actions:

 1. A query to get the ID associated with an open issue in one of my repos
 2. A mutation to add a few reactions to the issue

As with running the queries, having the documentation on the right side is great. I was able to drill down and see that addReaction requires an AddReactionInput type, which consists of three things - and only two are required (notice the ! ).

The only thing that seemed unintuitive was the requirement to have a body in the mutation, as if it's required to return something even though if we were doing a REST POST we wouldn't care about anything except a return code of 200 OK.