---
categories:
- LINQ
- C#
- C# 10
- .NET 6
- Coding
date: "2024-12-05T22:24:55Z"
description: ""
draft: false
cover:
  image: sophie-elvis-BiZ-_6kNjbI-unsplash.jpg
  relative: true
slug: using-minby-and-maxby-in-csharp
summary: The .NET team has made some helpful additions to LINQ over the last few years.
  Today let's check out MinBy and MaxBy from C# 10 / .NET 6.
tags:
- LINQ
- C#
- C# 10
- .NET 6
- Coding
title: Using MinBy and MaxBy in C# 10 / .NET 6
---
Microsoft made a couple new additions to LINQ as part of the C# 13 / .NET 9 release a few weeks ago, and, since I happen to really like LINQ, I wrote about [how to use them](https://grantwinney.com/using-linq-countby-and-aggregateby-in-csharp/). That got me thinking about other recent additions I might've missed in the last few releases, so I started [looking back](https://learn.microsoft.com/en-us/dotnet/maui/whats-new). And whatdya know, we got a slew of updates to LINQ in C# 10 / .NET 6 a few years ago.

Let's take a look at two of them - MaxBy and MinBy - right after a brief overview of what we had before. Makes it a little easier to appreciate the new stuff!

> The code in this post is available on [GitHub](https://github.com/grantwinney/CSharpDotNetFeatures/tree/master/C%23%2010/MaxByMinBy), for you to use, extend, or just follow along while you read... and hopefully discover something new along the way!

## Finding a Simple 'Max'

Normally, when we want to find the max or min value of some collection of primitive types, then a simple call to `Max` or `Min` will do. With a list of integers, for example, the max number is the highest. No surprise there.

```csharp
var maxNumber = new[] { 4, 3, 1, 17, 9, 0 }.Max();
Console.WriteLine($"The max number is: {maxNumber}");

// The max number is: 17
```

Finding the max value in a list of integers

## Implementing `IComparer<T>`

For other types, we can implement the [`IComparer<T>` interface](https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.icomparer-1), telling `Max` and `Min` what to use for the comparison. It can be really complex, or as simple as looking at one property:

```csharp
internal record Employee(string Name, string Dept, decimal Salary,
    DateOnly HireDate, int SecurityLevel) : IComparable<Employee>
{
    public int CompareTo(Employee? other)
    {
        if (other == null)
            return 1;

        return Salary > other.Salary ? 1 : Salary < other.Salary ? -1 : 0;
    }
}
```

Employee record, implementing `IComparable<T>`

In this case, the max or min `Employee` is just whoever makes the most or least:

```csharp
var employees = new List<Employee>
{
    new("Ed",  "Accounting",  78_000, new(2010,3,2), 1),
    new("Ned", "Accounting", 120_000, new(2000,2,6), 2),
    new("Ted", "Accounting",  94_000, new(2020,1,1), 0),
    new("Bob", "Accounting",  55_000, new(2015,7,2), 3)
};

var max = employees.Max();  // Ned
var min = employees.Min();  // Bob
Console.WriteLine($"{max!.Name} makes the most at {max.Salary:C}, " +
    $"while {min!.Name} makes the least at {min.Salary:C}.");

// Ned makes the most at $120,000.00, while Bob makes the least at $55,000.00.
```

## Using MinBy and MaxBy

**The new** *`*MaxBy*`* **and** *`*MinBy*`* **methods let us specify a property to sort by, which allows for more flexbility in simple cases.** We can sort employees by hire date, for example, and then sort by their security level immediately after:

```csharp
var newestHire = employees.MaxBy(e => e.HireDate);
var oldestHire = employees.MinBy(e => e.HireDate);

Console.WriteLine($"{newestHire!.Name} was the newest hire on {newestHire.HireDate}, " +
    $"while {oldestHire!.Name} was hired long ago on {oldestHire.HireDate}.");

// Ted was the newest hire on 1/1/2020, while Ned was hired long ago on 2/6/2000.


var mostSecurePersonnel = employees.MaxBy(e => e.SecurityLevel);
var leastSecurePersonnel = employees.MinBy(e => e.SecurityLevel);

Console.WriteLine($"{mostSecurePersonnel!.Name} has the highest security level, " +
    $"while {leastSecurePersonnel!.Name} has the lowest.");

// Bob has the highest security level, while Ted has the lowest.
```

Before, if we weren't implementing the interface, then we had to order the collection first and grab the first item, like this:

```csharp
var maxBySalary = employees.OrderByDescending(e => e.Salary).First();
var minBySalary = employees.OrderBy(e => e.Salary).First();
```

That isn't much longer, but `MaxBy` and `MinBy` just _read_ better. **The new methods make it easier to tell at a glance what the code is doing**, and that's not a trivial thing. We don't have to reason out what the `OrderBy` is doing, nor do we have to open the class and investigate the `CompareTo` method.

Like anything though, there's limitations. Since we can only specify one property, we still need something like this to consider multiple properties:

```csharp
var mostCompensatedEmployee = employees
    .OrderByDescending(e => e.Salary)   // what if two people earn the same salary?
    .ThenByDescending(e => e.Overtime)  // then we should consider overtime too
    .ThenByDescending(e => e.Bonus)     // and their bonus, as a final tie-breaker
    .First();
```

We can also use methods that provide a useful value, like the `Count()` method on a collection. One more example and then I'm done! Here's two companies, and a quick use of `MaxBy` to grab the one with more employees:

```csharp
var warnerEmps = new Company("Warner Bros", new List<Employee>
{
    new("Yakko", "IT",  78_000, new(2010,3,2), 1),
    new("Wakko", "IT", 90_000,  new(2000,2,6), 2),
    new("Dot", "IT", 90_000,  new(2000,2,6), 2),
});

var acmeEmps = new Company("Acme Inc", new List<Employee>
{
    new("Wile E Coyote", "IT",  78_000, new(2010,3,2), 1),
    new("Road Runner", "IT", 90_000,  new(2000,2,6), 2),
});

var largestCompany = new[] { warnerEmps, acmeEmps }.MaxBy(c => c.Employees.Count());

Console.WriteLine($"{largestCompany!.Name} is the largest company.");

// Warner Bros is the largest company.
```

I, for one, welcome any and all additions to LINQ, and can imagine that in many situations `MaxBy` and `MinBy` will make the code we write a little cleaner. Every bit counts!

## Learning More

If you found this content useful, and would like to learn more about a variety of [C#](https://grantwinney.com/tag/csharp/) features, check out my [CSharpDotNetFeatures repo](https://github.com/grantwinney/CSharpDotNetFeatures), where you'll find links to plenty more blog posts and practical examples!
