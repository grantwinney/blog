---
categories:
  - Learn
date: 2024-12-03T19:23:03Z
description: ""
draft: false
cover:
  image:
slug: using-linq-countby-and-aggregateby-in-csharp
summary: It's great to see Microsoft still giving us new things in LINQ. With C# 13 / .NET 9, we get CountBy and AggregateBy, so let's see how to use them.
tags:
  - csharp-13
  - dotnet-9
  - csharp
  - linq
title: Using CountBy and AggregateBy in C# 13 / .NET 9
---
Personally, I've been a big fan of LINQ ever since it was added in C# 3 over 15 years ago. I prefer the sleek SQL-like syntax to verbose, nested `foreach` blocks. It's nice to see that Microsoft still values it enough to keep adding new things.

A few weeks ago, when they announced the [official release](https://dotnet.microsoft.com/en-us/platform/support/policy/dotnet-core#lifecycle) of C# 13 / .NET 9, we got a couple new additions to LINQ – `CountBy` and `AggregateBy`. Let's see how to use them.

## But First...

Let's assume we have an `Employee` record and a handful of employees, with a variety of departments, job titles, and salaries:

```csharp
internal record Employee(string Name, string Dept, string Title, decimal Salary);

class EmployeeHelper()
{
    internal static IEnumerable<Employee> GetEmployees()
    {
        return
        [
            new("Mary", "Accounting", "Accountant", 80_000),
            new("Jean", "Accounting", "Manager", 140_000),
            new("Bill", "Accounting", "Accountant", 90_000),
            new("Dean", "Accounting", "Accountant", 84_000),
            
            new("Suzy", "IT", "Developer", 125_000),
            new("Mike", "IT", "Manager", 160_000),
            new("Adam", "IT", "Developer", 75_000),
            
            new("Leah", "Sales", "Manager", 99_000),
            new("Glen", "Sales", "Sales Rep", 65_000),
            new("Katy", "Sales", "Sales Rep", 102_000),
            new("Mark", "Sales", "Sales Rep", 55_000),
            new("Nate", "Sales", "Manager", 110_000),
        ];
    }
}
```

We'll use this for all the examples below.

## CountBy

The new [CountBy](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.countby?view=net-9.0) method returns a count of elements, grouped by key. This is something we could already do using `GroupBy` and `Count`, but now it's more streamlined.

Traditionally, if we wanted to get the number of employees in each department, we could write a bit of LINQ like this, grouping by department and then counting each group:

```csharp
var empCountByDept = employees
    .GroupBy(e => e.Dept)
    .Select(grp => new KeyValuePair<string, int>(grp.Key, grp.Count()));
```

Using the new `CountBy` method, this becomes a _very_ short statement:

```csharp
var empCountByDept = employees.CountBy(e => e.Dept);
```

The output (in the `Program` class) for both is identical:

```none
Total employees in Accounting: 4
Total employees in IT: 3
Total employees in Sales: 5
```

The only thing we need to specify is which value to group by, and then `CountBy` takes care of counting each of the groups for us. It's as simple as that!

## AggregateBy

The `Aggregate` method is one of the few LINQ methods that I've struggled to find a good use for. It's flexible and powerful, but in many cases there are more straight-forward methods with a singular focus.

If you're unfamiliar with it, the basic process is to:

1. Start with an initial seed value (or the first element in the collection, if omitted).
2. Run some function or operation over the first two elements.
3. Loop through the remaining elements, running the same function or operation against the result of step 2 and each new element.

We can do a lot with it, but for common cases it's just not my go-to. Here's two ways to count the total number of managers, for example. One using `Aggregate` and the other using `Count`. Which is easier to read?

```csharp
var mgrCount1 =
    employees.Aggregate(0, (count, e) => e.Title == "Manager" ? count + 1 : count);

var mgrCount2 = employees.Count(e => e.Title == "Manager");
```

That's all I'll say about that. Just keep it in mind when you look at the examples below. If you start thinking, _"wait, couldn't you just use...?",_ you're probably right.

Here's how we might use `GroupBy` and `Aggregate` to create a list of departments and the total salaries for each of those departments. First, we group by department, then we return a list of `KeyValuePair` objects with the department name (stored in `deptGroup.Key`) and an aggregate value. For the aggregate, we start with 0 (the seed), and then add each employee's salary to the previous total, one by one.

```csharp
var totalSalariesByDept = employees
    .GroupBy(e => e.Dept)
    .Select(deptGroup => new KeyValuePair<string, decimal>(deptGroup.Key,
        deptGroup.Aggregate(0m, (deptSal, nextEmp) => deptSal + nextEmp.Salary)));
```

With the new `AggregateBy` method, the separate `GroupBy` goes away, just like with the `CountBy` method. The first parameter becomes the value to group by, the next is our starting value again, and finally the calculation to perform. The `deptSal` value is our ongoing total, and each employee salary is added to that, one at a time.

```csharp
var totalSalariesByDept = employees.AggregateBy(e => e.Dept, 0m,
    (deptSal, nextEmp) => deptSal + nextEmp.Salary);
```

The output from both is the same:

```none
Total salaries for Accounting: $394,000.00
Total salaries for IT: $360,000.00
Total salaries for Sales: $431,000.00
```

Let's look at one more example, querying all employees making over 100k, by job title. First, we group by titles, then aggregate an ongoing total again. Unlike the simple salary before, we now have a Tuple that stores a counter _and_ a total salary:

```csharp
var employeesOver100k = employees
    .GroupBy(e => e.Title)
    .Select(group => new KeyValuePair<string, (int Count, decimal Salary)>(group.Key,
        group.Aggregate((Count: 0, Salary: 0m), (t, nextEmp) =>
            nextEmp.Salary > 100_000 ? (t.Count + 1, t.Salary + nextEmp.Salary) : t)));
```

With the new method, `GroupBy` and explicitly creating a `KeyValuePair` goes away again, and the overall LINQ statement is shorter:

```csharp
var employeesOver100k = employees
    .AggregateBy(e => e.Title, (Count: 0, Salary: 0m), (totals, nextEmp) =>
        nextEmp.Salary > 100_000 ? (t.Count + 1, t.Salary + nextEmp.Salary) : t);
```

And here's the output from both of these:

```none
0 accountant personnel earn over 100k, for a total of $0.00
3 manager personnel earn over 100k, for a total of $410,000.00
1 developer personnel earn over 100k, for a total of $125,000.00
1 sales rep personnel earn over 100k, for a total of $102,000.00
```

## Learn More

If you're interested in other changes in .NET 9 / C# 13, a good place to start (though definitely not to end) is with the official MS docs.

- [What's new in C# 13 | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-13)
- [What's new in .NET 9 | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/core/whats-new/dotnet-9/overview)

If you found this content useful, and would like to learn more about a variety of [C#](https://grantwinney.com/tags/csharp/) features, check out my [CSharpDotNetFeatures repo](https://github.com/grantwinney/CSharpDotNetFeatures), where you'll find links to plenty more blog posts and practical examples!
