---
categories:
- csharp-7
- surviving-winforms
- Coding
- csharp
date: "2023-01-05T00:38:03Z"
description: ""
draft: false
cover:
  image: photo-1436731837106-3b37ef132fc2.jpg
slug: using-tuple-and-deconstruction-to-return-multiple-values
summary: A big challenge with any language is trying to group and organize things sensibly, and returning multiple values is no exception. Let's check out Tuples and deconstruction, and see how they can help us out.
tags:
- csharp-7
- surviving-winforms
- coding
- csharp
title: Using Tuples and deconstruction to return multiple values in C#
---
One of the biggie challenges when programming in any language is figuring how to group and organize things sensibly. Even a small project can get out of hand quickly, and once you've got a dozen devs working in something for years, all bets are off.

So we have methods and functions, organized into classes _(even_ [_JS has classes_](https://www.javascripttutorial.net/es6/javascript-class/) _now)_ and namespaces, separate projects and assemblies, and on and on. The tricky part of having so many ways to organize things is knowing how and when to use one of them. A lot of it's up for debate _(what isn't?)_ and some of it's not... I wouldn't recommend publishing a NuGet package for [a few lines of code](https://www.sciencealert.com/how-a-programmer-almost-broke-the-internet-by-deleting-11-lines-of-code), lol.

> The code in this article is available on <a href="https://github.com/grantwinney/Surviving-WinForms/tree/master/ClarityConciseness/TupleDeconstruction">GitHub</a>, if you'd like to use it in your own projects or just follow along while you read.

In C#, it's not uncommon to use classes to group similar logic together, and then provide properties to access whatever values are in the class - a person with a name and birthdate, a car with an engine type and model, whatever. If you're going to have lots of properties, a class probably makes sense. But what if you don't? If there's a more lightweight option than a class and a good use case for it, I'd be interested in hearing about it.. or writing about it as the case may be.

```csharp
public class Circle
{
    int radius;

    public Circle(int radius) => this.radius = radius;

    public int Diameter => radius * 2;
    public double Circumference => Math.PI * Diameter;
    public double Area => Math.PI * Math.Pow(radius, 2);
}
```

One alternative is to create a method with several "out" parameters.

```csharp
private void GetCircle(int radius, out int diameter, out double circumference, out double area)
{
    diameter = radius * 2;
    circumference = Math.PI * diameter;
    area = Math.PI * Math.Pow(radius, 2);
}
```

I used to think that was ugly since we had to define the variables before calling the method, but now they can be defined right [as we call the method](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/out-parameter-modifier#calling-a-method-with-an-out-argument) so it's a lot more palatable. That was introduced in [C# 7.0](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-version-history#c-version-70).

```csharp
GetCircle(6, out var diameter, out var circumference, out var area);

Console.WriteLine($"Diameter: {diameter}");               // 12
Console.WriteLine($"Circumference: {circumference:N2}");  // 37.70
Console.WriteLine($"Area: {area:N2}");                    // 113.10
```

Another alternative, and the one I want to really focus on, is using tuples. I used to like them even less than "out" parameters, since whatever values you returned were only accessible by referencing `.Item1`, `.Item2`, etc. You immediately lost context of what was being returned.. very unfriendly. There have been some major changes though since [C# 7.0](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-version-history#c-version-70), and it makes tuples much easier on the eyes.. brain... something. They're easier to work with.

Here's an example that's pretty similar to the one above using "out" parameters, but it returns a [tuple type](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/value-tuples) instead. Since tuple types can have [field names](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/value-tuples#tuple-field-names) too, this really seems to behave more like a class to me.

```csharp
private (int diameter, double circumference, double area) GetCircle(int radius)
{
    var diameter = radius * 2;
    var circumference = Math.PI * diameter;
    var area = Math.PI * Math.Pow(radius, 2);

    return (diameter, circumference, area);
}
```

Being able to access each element of the tuple by name makes this _soo_ much friendlier than in the past.

```csharp
// Access each tuple element individually
var circle = GetCircle(3);

Console.WriteLine($"Diameter: {circle.diameter}");               // 6
Console.WriteLine($"Circumference: {circle.circumference:N2}");  // 18.85
Console.WriteLine($"Area: {circle.area:N2}");                    // 28.27
```

You can [desconstruct a tuple](https://learn.microsoft.com/en-us/dotnet/csharp/fundamentals/functional/discards#tuple-and-object-deconstruction) if you'd like, assigning all the elements in one place, which is similar to how the "out" example works.

```csharp
// Deconstruct the tuple elements into separate local variables
var (diameter, circumference, area) = GetCircle(4);

Console.WriteLine($"Diameter: {diameter}");                      // 8
Console.WriteLine($"Circumference: {circumference:N2}");         // 25.13
Console.WriteLine($"Area: {area:N2}");                           // 50.27
```

And if you don't need all of the elements, you can even [discard them](https://learn.microsoft.com/en-us/dotnet/csharp/fundamentals/functional/discards#tuple-and-object-deconstruction). Here's an example where all I needed was the area of the circle, so I discarded the rest.

```csharp
// Deconstruct the tuple elements and discard the ones you don't need
var (_, _, area) = GetCircle(5);

Console.WriteLine($"Area: {area:N2}");                          // 78.54
```

What do you think? Like it more than classes and "out" variables? Love it? Hate it? Or do you have your own way of returning multiple values at once?

If you found this content useful, and want to learn more about a variety of C# features, check out [this GitHub repo](https://github.com/grantwinney/CSharpDotNetExamples), where you'll find links to plenty more blog posts and practical examples!
