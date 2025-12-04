---
title: Using null conditional and null coalescing operators for assignment in C#
slug: csharp-null-conditional-assignment
summary: The null conditional operator just got an upgrade.. we can do assignments with it now! Let's see it in action.
description:
date: 2025-12-03T12:09:00
draft: false
postimage: /banners/default-learn-banner.webp
postimagealt:
postimagecaption:
categories:
  - Learn
tags:
  - csharp-14
  - csharp-8
  - csharp
---
We've had the [null conditional operator](https://grantwinney.com/null-conditional-and-null-coalescing-operators/) for years, since C# 6. You might want to skim that post (which I wrote a few years ago) before reading this one, if you're unfamiliar with it. Otherwise, I'll borrow a bit for a quick review, and then we'll look at what C# 14 added. And if you'd like to mess around with the code below, it's available in my [CSharpDotNetFeatures](https://github.com/grantwinney/CSharpDotNetFeatures/tree/master/C%23%2014/NullConditionalAssignment) repo on GitHub.

## Null Conditional (a review)

When we're dealing with nested classes, like the one below for example, we usually have to be really defensive to avoid the dreaded `NullReferenceException`.

```cs
public class Company
{
    public string Name { get; set; }
    public IList<Department> Departments { get; set; }
}

public class Department
{
    public string Name { get; set; }
    public IList<Employee> Employees { get; set; } = new List<Employee>();
}

public class Employee
{
    public string Name { get; set; }
}
```

That means checking for `null` repeatedly when referencing, say, an employee's name. And since collections are involved, we need to check to make sure they have valid values too. If we just wanted to get the first employee from the first department, we might do something like this:

```cs
// Getting employee name traditionally
if (company != null &&
    company.Departments != null && company.Departments.Count > 0 &&
    company.Departments[0].Employees != null &&
    company.Departments[0].Employees.Count > 0 &&
    company.Departments[0].Employees[0].Name != null)
{
    var employeeName = company.Departments[0].Employees[0].Name;
    Console.WriteLine($"Employee Name: {employeeName}");
}
else
{
    Console.WriteLine("Employee Name: N/A");
}
```

What the null conditional operators let us do is shorten the above to just this:

```cs
// Getting employee name using null-conditional operators
var employeeName = company?.Departments?[0]?.Employees?[0]?.Name ?? "N/A";
Console.WriteLine($"Employee Name: {employeeName}");
```

At every step along the way, it checks whether it can skip the rest (short-circuit), and just set `employeeName == "N/A"`:

- If `org == null`, then it skips everything else and sets employee name to "N/A".
- If `org.Departments == null`, it skips the rest and moves on.
- If `org?.Departments[0] == null` for some weird reason, it skips the rest.
- And on and on, with `.Employees`, then `.Employees[0]`, etc.

Getting a nested value involves a _lot_ less code, which is great.

## Null Conditional Assignment

What the null conditional operator hasn't helped us shorten in the past, though, is _setting_ a value. If any part of the nested objects is `null` we'll get an exception, so we have to be careful:

```cs
// Setting employee name traditionally
if (company != null &&
    company.Departments != null && company.Departments.Count > 0 &&
    company.Departments[0].Employees != null &&
    company.Departments[0].Employees.Count > 0 &&
    company.Departments[0].Employees[0].Name != null)
{
    company.Departments[0].Employees[0].Name = "John Doe";
}
```

However, with the update in C# 14 that allows using the null conditional operator when *assigning* a value, we can use the same short-circuiting code here too:

```cs
// Setting employee name using null-conditional operators
company?.Departments?[0]?.Employees?[0]?.Name = "John Doe";
```

Combined with a feature we got in C# 8, to use the null-coalescing operator `??=` during assignment, we can also quickly make sure that existing values aren't accidentally overwritten:

```cs
// Setting employee name using null-conditional and null-coalescing operators
company?.Departments?[0]?.Employees?[0]?.Name ??= "John Doe";
```

It could even be used with LINQ when searching through nested collections, to handle the case where whatever we're searching for isn't found:

```cs
private void ActivateEmployee(Company company, string dept, string name)
{
    // Activate employee, using null-conditional assignment operator
    company
        ?.Departments.SingleOrDefault(d => d.Name == "Sales")
        ?.Employees.SingleOrDefault(e => e.Name == "Greg Smith")
        ?.Active = true;
}
```

## Thoughts

I like that the feature works on _both_ sides of the assignment operator now, which is more consistent. I haven't used it in a prod environment yet (since it's brand new), but it definitely seems like it'll make things more readable ... once I'm used to reading it!

In some cases though, the (older) more verbose way could be more readable, and I think readability is really important, especially when multiple devs are working in an app. Heck, when I set an app down and pick it up a year later, _I_ might as well be a different dev for all I remember about it, lol. All-in-all though, it's a nice addition to the language!

## Learn More

- [Null-conditional operators - `?.` and `?[]`](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/member-access-operators#null-conditional-operators--and-)
- [Null-coalescing operators - `??` and `??=`](https://learn.microsoft.com/en-my/dotnet/csharp/language-reference/operators/null-coalescing-operator)
- [Null-conditional assignment - What's new in C# 14](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-14#null-conditional-assignment)
- [Null conditional assignment - C# feature specifications | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-14.0/null-conditional-assignment)
