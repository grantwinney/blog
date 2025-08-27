---
categories:
- C# 6
- Coding
- C#
date: "2022-09-27T01:13:39Z"
description: ""
draft: false
cover:
  image: photo-1587920149371-ac728dd20da4.jpg
slug: null-conditional-and-null-coalescing-operators
summary: Checking for nulls in C# is tedious, but C# 6 gave us the null-conditional
  and null-coalescing operators. Let's see how they've improved things.
tags:
- C# 6
- Coding
- C#
title: Checking for null in C#, using the null-conditional and null-coalescing operators
---
Boy, that's a catchy title. Sometimes they just roll off the tongue, ya know? 🙄

Last week, I wrote about [using string interpolation to craft readable strings](https://grantwinney.com/using-string-interpolation-to-craft-readable-strings/), and figured it might be worth investigating some of the other useful additions to C# over the last few years. So let's check out new ways (well, new compared to C#'s age) to efficiently check for nulls.

Anyone who's spent some time in C# has had a run in with the dreaded [NullReferenceException](https://stackoverflow.com/questions/4660142/what-is-a-nullreferenceexception-and-how-do-i-fix-it). The underlying cause isn't always obvious, but getting around it usually is - just check for nulls. Traditionally, the only way to safely use some deeply nested object was to check for null at every level, so a lot of older apps are littered with code like this:

```csharp
if (employee != null && employee.Name != null &&
    employee.Department != null && employee.Department.Manager != null)
{
    MessageBox.Show(employee.Name + " reports to " + employee.Department.Manager + ".");
}
```

That is just so long-winded and __boring.__ We shouldn't have to type all that repetitive code out, and we don't. C# 6 gave us a couple new tools - the null-conditional and null-coalescing operators.

> The code in this article is available on <a href="https://github.com/grantwinney/Surviving-WinForms/tree/master/ClarityConciseness/NullHandlingOperators">GitHub</a>, if you'd like to use it in your own projects or just follow along while you read.

First, let's define a few nested classes to use for examples, and then a company with a couple departments and employees to experiment with.

```csharp
public class Company
{
    public string Name { get; set; }
    public Uri URL { get; set; }
    public DateTime Founded { get; set; }
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
    public string JobTitle { get; set; }
    public DateTime? HireDate { get; set; }
}
```

```csharp
public Company DefineCompany()
{
    return new Company
    {
        Name = "SpaceX",
        URL = new Uri("https://www.spacex.com"),
        Founded = new DateTime(2002, 03, 14),
        Departments = new List<Department>
        {
            new Department
            {
                Name = "Starship Engineering",
                Employees = new List<Employee>
                {
                    new Employee
                    {
                        Name = "Geordi La Forge",
                        JobTitle = "Lead Flight Control Tech",
                        HireDate = new DateTime(2011,1,1),
                    },
                    new Employee
                    {
                        Name = "Montgomery Scott",
                        JobTitle = "Sr Propulsion Engineer",
                        HireDate = new DateTime(2022,2,2),
                    },
                    new Employee
                    {
                        Name = "B'Elanna Torres"
                    }
                }
            },
            new Department
            {
                Name = "Space Car Retrieval",
            },
        }
    };
}
```

## Null Conditional operator

The [null-conditional operator](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/member-access-operators#null-conditional-operators--and-) allows you to call a deeply-nested class member, where anything in the chain of objects might be null, and it returns null instead of throwing an exception.

In the above code, for example, the Company has a name. But what if it didn't, and you tried to get the length of it for some reason? It would throw an exception if you didn't check for null first. Null conditional operator to the rescue.

```csharp
// Returns a valid company name
var validCompanyNameLength = validCompany.Name.Length;     // 6 (SpaceX)

// If Name is null, then it short-circuits the rest of the line and returns
// null, instead of throwing an exception when Length is called
var invalidCompanyNameLength = emptyCompany.Name?.Length;  // null
```

By adding a single `?` to the second line in the right place, it stores null instead of throwing an exception. One caveat is that, even though `Length` returns an integer, the variable on the left is actually an `int?`, since it needs to be able to store null.

Here's another example, where one company has a URL, but the other does not. Since URL is null on the second line, accessing Host would normally throw an exception.

```csharp
var validUrlHost = validCompany.URL.Host;     // www.spacex.com

var invalidUrlHost = emptyCompany.URL?.Host;  // null
```

You only have to use them where you're worried about nulls too. If you've defined your classes in such a way that __if__ there's a department, then it __will__ have a collection of employees (maybe an empty one), then you can just use the null conditional operator in the one place you're worried about.

```csharp
var engineers = validCompany.Departments[0].Employees.Count();      // 3

var invalidCount = emptyCompany.Departments?[0].Employees.Count();  // null
```

****Note #1:**** The operator always applies to the variable right __before__ it. So above, if Departments is null then you're safe. But if Departments is instantiated but empty, then trying to access the first element from the collection will still throw a different exception.

****Note #2:**** Use these where it makes sense. I happen to think that, if there's no reasonable explanation for a certain variable to ever be null, then it's probably better to let it throw an exception so you can debug it, rather than aggressively preventing NullReferenceException everywhere and giving things default values where it doesn't make sense.

## Null Coalescing operator

Using the null coalescing operator with the null conditional gives you even more power, but it's still concise enough for a single line. It lets you define what the default value should be when a value is null. For example, you can replace this:

```csharp
string companyName;

if (companyName != null)
    companyName = company.Name;
else
    companyName = "unknown";
```

Or even this:

```csharp
var companyName = company.Name != null ? company.Name ? "unknown";
```

With this:

```csharp
var companyName = company.Name ?? "unknown";
```

Revisiting the earlier examples, you can use both together to avoid an exception __and__ to decide what the value should be when it's null...

```csharp
Console.WriteLine($"{validCompany.Name}'s website is {validCompany.URL.Host}.");
// SpaceX's website is www.spacex.com.

var anotherInvalidCompany = new Company { Name = "ACME" };
Console.WriteLine($"{anotherInvalidCompany.Name}'s website is {anotherInvalidCompany.URL?.Host ?? "unknown"}.");
// ACME's website is unknown.
```

If you want to count the number of employees, but some departments won't have any, then use the null coalescing operator to just say there's 0 employees.

```csharp
foreach (var dept in validCompany.Departments)
    Console.WriteLine($"{dept.Name} currently has {dept.Employees?.Count() ?? 0} employee(s).");

// Starship Engineering currently has 3 employee(s).
// Space Car Retrieval currently has 0 employee(s).
```

Here's one more example, where some employees don't have a hire date. Not sure why that would be, but out in space you've got bigger fish to fry than recording every alien who joins the crew. Or something.

```csharp
foreach (var emp in validCompany.Departments.SelectMany(x => x.Employees))
    Console.WriteLine($"{emp.Name} was hired on {emp.HireDate?.ToString("d") ?? "an unknown date"}.");

// Geordi La Forge was hired on 1/1/2011.
// Montgomery Scott was hired on 2/2/2022.
// B'Elanna Torres was hired on an unknown date.
```

And that example from the beginning? It becomes this:

```csharp
// Before
if (employee != null && employee.Name != null &&
    employee.Department != null && employee.Department.Manager != null)
{
    MessageBox.Show(employee.Name + " reports to " + employee.Department.Manager + ".");
}

// Now
if (employee?.Name != null && employee?.Department?.Manager != null)
{
    MessageBox.Show($"{employee.Name} reports to {employee.Department.Manager}.");
}
```

If you found this content useful, and want to learn more about a variety of C# features, check out [my GitHub repo](https://github.com/grantwinney/CSharpDotNetExamples), where you'll find links to plenty more blog posts and practical examples!