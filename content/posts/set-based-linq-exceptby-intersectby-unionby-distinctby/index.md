---
categories:
- LINQ
- csharp-10
- dotnet-6
- csharp
- Coding
date: "2024-12-08T22:59:12Z"
description: ""
draft: false
cover:
  image: dimitry-b-S9T2A1dPRiY-unsplash.jpg
slug: set-based-linq-exceptby-intersectby-unionby-distinctby
summary: The .NET team has made some helpful additions to LINQ in recent years. Today
  let's check out the various set-based updates from C# 10 / .NET 6.
tags:
- linq
- csharp-10
- dotnet-6
- csharp
- coding
title: Set-based LINQ - ExceptBy, IntersectBy, UnionBy, DistinctBy
---
Microsoft recently released C# 13 with a couple new additions to LINQ, which [I wrote about last week](https://grantwinney.com/using-linq-countby-and-aggregateby-in-csharp/). That got me thinking about other recent additions to LINQ, like [MaxBy and MinBy](https://grantwinney.com/using-minby-and-maxby-in-csharp/). Continuing down the list, let's check out some set-based methods, including ExceptBy, IntersectBy, UnionBy, and DistinctBy.

> The code in this post is available on [GitHub](https://github.com/grantwinney/CSharpDotNetFeatures/tree/master/C%23%2010/SetBasedLinqMethods), for you to use, extend, or just follow along while you read... and hopefully discover something new along the way!

## But first...

Let's define a couple classes and create a short list of books and movies, so we can try out the different LINQ methods against some data.

```csharp
internal class Book
{
    public string Name { get; set; }
    public int PublishYear { get; set; }
}

internal class Movie
{
    public string Name { get; set; }
    public int ReleaseYear { get; set; }
}

var books = new List<Book>
{
    new() { Name = "How to Stop Time",   PublishYear = 2017 },
    new() { Name = "Little Women",       PublishYear = 1868 },
    new() { Name = "Catcher in the Rye", PublishYear = 1951 },
    new() { Name = "The Princess Bride", PublishYear = 1973 },
};

var oldBooks = new List<Book>
{
    new() { Name = "Little Women",        PublishYear = 1868 },
    new() { Name = "The Maid Of Orleans", PublishYear = 1801 },
};

var movies = new List<Movie>
{
    new() { Name = "The Princess Bride", ReleaseYear = 1987 },
    new() { Name = "Good Will Hunting",  ReleaseYear = 1997 },
    new() { Name = "Inception",          ReleaseYear = 2010 },
    new() { Name = "Little Women",       ReleaseYear = 1994 },
    new() { Name = "Little Women",       ReleaseYear = 2019 },
    new() { Name = "Little Women",       ReleaseYear = 1949 },
};
```

## ExceptBy

The older `Except` method allows us to ask for all the items in one collection, minus the items in a second collection, as long as our code has some way to determine what makes any two items equal. Primary types, records, or classes where we've implemented certain interfaces work as easily as:

```csharp
var newerBooks = books.Except(oldBooks);
```

In many cases though, we need to give the code a hint about our classes. Using the newer [`ExceptBy`](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.exceptby) method lets us do that with a single property in a class, like "Name" in the sample class above:

```csharp
var newerBooks = books.ExceptBy(oldBooks.Select(ob => ob.Name), b => b.Name);
Console.WriteLine($"Newer books: {string.Join(", ", newerBooks.Select(b => b.Name))}");

// Newer books: How to Stop Time, Catcher in the Rye, The Princess Bride
```

We can compare two _different_ class types too, i.e. asking for all the books _except_ those whose name occurs in the movies list:

```csharp
var booksOnly = books.ExceptBy(movies.Select(m => m.Name), b => b.Name);
Console.WriteLine("Books that aren't movies: " +
    string.Join(", ", booksOnly.Select(x => x.Name)));

// Books that aren't movies: How to Stop Time, Catcher in the Rye
```

Or we can go the other way, and get all movies that are _not_ also books:

```csharp
var moviesOnly = movies.ExceptBy(books.Select(b => b.Name), m => m.Name);
Console.WriteLine("Movies that aren't books: " +
    string.Join(", ", moviesOnly.Select(x => x.Name)));

// Movies that aren't books: Good Will Hunting, Inception
```

## IntersectBy

Whereas `Except` and `ExceptBy` remove everything from one list that _is_ in a second list, the `Intersect` and [`IntersectBy`](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.intersectby) methods removes everything from the first that _isn't_ in the second. It finds the intersection of two collections, or items that are in both.

Just like with `ExceptBy`, we can specify a property (like the name) that makes them equal:

```csharp
var moviesANDbooks = books.IntersectBy(movies.Select(m => m.Name), b => b.Name);
Console.WriteLine("Books that are also movies: " +
    string.Join(", ", moviesANDbooks.Select(x => x.Name)));

// Books that are also movies: Little Women, The Princess Bride
```

## UnionBy

Instead of considering what _is_ in the second collection like `ExceptBy`, or what _isn't_ in it like `IntersectBy`, the [`UnionBy`](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.unionby) method just grabs a unique list of everything in both. If the collections contain the same class type, we just specify the parameter name it should use for the comparison:

```csharp
var allBooks = books.UnionBy(oldBooks, b => b.Name);
Console.WriteLine("All the books: " +
    string.Join(", ", allBooks.Select(x => x.Name)));

// All the books: How to Stop Time, Little Women,
// Catcher in the Rye, The Princess Bride, The Maid of Orleans
```

If they're different types, like movies and books, we need to convert one type to the other in order to "combine" them:

```csharp
var moviesORbooks = books.UnionBy(movies.Select(movie =>
    new Book { Name = movie.Name, PublishYear = 2000 }), book => book.Name);
Console.WriteLine("Books, movies, or both: " +
    string.Join(", ", moviesORbooks.Select(x => x.Name)));

// Books, movies, or both: How to Stop Time, Little Women,
// Catcher in the Rye, The Princess Bride, Good Will Hunting, Inception
```

## DistinctBy

And finally, there's [`DistinctBy`](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.distinctby), where we can specify the property by which it can be determined how to get a unique list of some item. When there's multiple items with the same value for the property, it goes with the first one:

```csharp
var uniqueMovies = movies.DistinctBy(m => m.Name);
Console.WriteLine("Unique list of movies: " +
    string.Join(", ", uniqueMovies.Select(x => $"{x.Name} ({x.ReleaseYear})")));

// Unique list of movies: The Princess Bride (1987), Good Will Hunting (1997),
// Inception (2010), Little Women (1994)
```

## Specifying Multiple Properties

All the examples above compared a single property to define equality, but in large collections that might not be enough. When it isn't, we can specify more than one property by just creating a tuple on-the-fly.

Here's another `ExceptBy` example, but it compares the name _and_ the publish year:

```csharp
var newerBooks_multiple = books.ExceptBy(
    oldBooks.Select(ob => (ob.Name, ob.PublishYear)), b => (b.Name, b.PublishYear));
Console.WriteLine($"Newer books: " +
    $"{string.Join(", ", newerBooks_multiple.Select(b => b.Name))}");
```

We can use Tuples when comparing collections of different types too, as long as the types of the arguments in the Tuple are the same:

```csharp
var booksOnly_multiple = books.ExceptBy(
    movies.Select(m => (m.Name, m.ReleaseYear)), b => (b.Name, b.PublishYear));
Console.WriteLine("Books that aren't movies: " +
    string.Join(", ", booksOnly_multiple.Select(x => x.Name)));
```

You probably get the idea, but here's one more – a distinct list of movies, where "distinct" is a unique title and release year:

```csharp
var uniqueMovies = movies.DistinctBy(m => (m.Name, m.ReleaseYear));
Console.WriteLine("Unique list of movie/year combos: " +
    string.Join(", ", uniqueMovies.Select(x => $"{x.Name} ({x.ReleaseYear})")));

// Unique list of movie/year combos: The Princess Bride (1987),
// Good Will Hunting (1997), Inception (2010), Little Women (1994),
// Little Women (2019), Little Women (1949)
```

## Learning More...

There's more details in the MS docs, although they're pretty dry.

- [Enumerable.ExceptBy Method (System.Linq) | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.exceptby)
- [Enumerable.IntersectBy Method (System.Linq) | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.intersectby)
- [Enumerable.UnionBy Method (System.Linq) | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.unionby)
- [Enumerable.DistinctBy Method (System.Linq) | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.distinctby)

A better page is the one that shows [example usages of Set operations](https://learn.microsoft.com/en-us/dotnet/csharp/linq/standard-query-operators/set-operations).

If you found this content useful, and would like to learn more about a variety of [C#](https://grantwinney.com/tags/csharp/) features, check out my [CSharpDotNetFeatures repo](https://github.com/grantwinney/CSharpDotNetFeatures), where you'll find links to plenty more blog posts and practical examples!