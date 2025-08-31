---
categories:
- C# 11
- C#
- Generic Math
- Coding
- Series Generic Math Intro
date: "2023-04-05T03:50:14Z"
description: ""
draft: false
cover:
  image: photo-1630402283739-3f61474e2a3a.jpg
slug: csharp-generic-math-support
summary: What is Generic Math support in C# 11, and how do we take advantage of it?
  Let's dig in and find out! (part 3 of 3)
tags:
- C# 11
- C#
- Generic Math
- Coding
- Series Generic Math Intro
title: Generic Math Support in C# 11
---
This is post 3 in a 3-part series building up to a new C# 11 feature called [Generic Math](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-11#generic-math-support). First though, it might be helpful to read two other posts, to get familiar with [static abstract members](https://grantwinney.com/whats-a-static-abstract-interface-method-in-c/) (also new to C# 11) and [overloading operators](https://grantwinney.com/csharp-overload-arithmetic-equality-comparison-operators/) (not new, but useful).

> The code in this post is available on <a href="https://github.com/grantwinney/CSharpDotNetExamples/tree/master/C%23%2011/GenericMathSupport/GenericMathSupport">GitHub</a>, for you to use, expand upon, or just follow along while you read... and hopefully discover something new!

## Generics

Since [generics](https://learn.microsoft.com/en-us/dotnet/csharp/fundamentals/types/generics) were introduced in version 2.0 [back in 2005](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-version-history#c-version-20), it's been enhanced a few times over the years, most recently in C# 11. Generics let us write code in a way that the exact type of the data doesn't have to be known right away.

You can, for example, write a base class with generics that allows any class inheriting from it to define an "identifier" that's a different type, like this code does. Notice how Box uses an integer for its identifier, Crate uses a string, and Folder uses a Guid. We don't need to define the same `GetIdentifier()` method in each of the classes either - nice and [DRY](https://deviq.com/principles/dont-repeat-yourself).

```csharp
public class Container<T>
{
    public T Identifier { get; set; }
    public string GetIdentifier() => $"The identifier is: {Identifier}";
}

public class Box : Container<int>
{
    public Box(int identifier)
    {
        Identifier = identifier;
    }
}

public class Crate : Container<string>
{
    public Crate(string identifier)
    {
        Identifier = identifier;
    }
}

public class Folder : Container<Guid>
{
    public Folder(Guid identifier)
    {
        Identifier = identifier;
    }
}


[Test]
public void GetIdentifierWorksForAllTypesOfContainers()
{
    var boxId = 4;  // chosen by fair dice roll; randomness guaranteed
    var crateId = "absolutely_unique_id";
    var folderId = Guid.NewGuid();

    var box = new Box(boxId);
    var crate = new Crate(crateId);
    var folder = new Folder(folderId);

    Assert.Multiple(() =>
    {
        Assert.That(box.GetIdentifier(), Is.EqualTo($"The identifier is: {boxId}"));
        Assert.That(crate.GetIdentifier(), Is.EqualTo($"The identifier is: {crateId}"));
        Assert.That(folder.GetIdentifier(), Is.EqualTo($"The identifier is: {folderId}"));
    });
}
```

You can use generics in interfaces too, like the .NET framework does with [`IEnumerable<T>`](https://referencesource.microsoft.com/#mscorlib/system/collections/generic/ienumerable.cs,3acf01620172c7f0) (heavily used with LINQ).

Here's a slightly different version of the above code, replacing the generic base class with a generic interface. Anything that accepts an `IContainer<T>` can rest assured that the classes implementing the interface will have an identifier (whose type may vary per class, like with the int, string, and Guid above) and a method that prints a description about the identifier.

```csharp
public interface IContainer<T>
{
    T Identifier { get; set; }
    string GetDescription();
}

public class Box : IContainer<int>
{
    public int Identifier { get; set; }
    public string GetDescription() => $"The box id is {Identifier}.";
}

public class Crate : IContainer<Guid>
{
    public Guid Identifier { get; set; }
    public string GetDescription() => $"The guid is: {Identifier}";
}


[Test]
public void GetDescriptionWorksForAllTypesOfContainers()
{
    var boxId = 4;  // chosen by fair dice roll; randomness guaranteed
    var crateId = Guid.NewGuid();

    var box = new Box { Identifier = boxId };
    var crate = new Crate { Identifier = crateId };

    Assert.Multiple(() =>
    {
        Assert.That(box.GetDescription(), Is.EqualTo($"The box id is {boxId}."));
        Assert.That(crate.GetDescription(), Is.EqualTo($"The guid is: {crateId}"));
    });
}
```

## Generic Math

One thing we haven't really been able to do before, though, is add "static" members to an interface. Actually, since C# 8 we've apparently been able to add static methods to interfaces as long as they declare a default body. I'm sure there's a good reason for it, but I haven't used it yet.

Anyway, since [overloading an operator](https://grantwinney.com/csharp-overload-arithmetic-equality-comparison-operators/) requires defining a static method on a class, and there's never been a way to specify a static _abstract_ member in an interface (aka one without a default body), it hasn't been possible to have an interface require that classes overload certain operators. Until now.

As of C# 11, you can add [static abstract members](https://grantwinney.com/whats-a-static-abstract-interface-method-in-c/) to interfaces, which means you _can_ require classes to have to implement one or more overloaded operators. Although you can certainly test this with your own interfaces _(_[_learn more here_](https://grantwinney.com/whats-a-static-abstract-interface-method-in-c/)_),_ you can also make use of the new interfaces that C# 11 has given us. There's [IAdditionOperators](https://source.dot.net/#System.Private.CoreLib/src/libraries/System.Private.CoreLib/src/System/Numerics/IAdditionOperators.cs,67cc175feb3d46df) and [IComparisonOperators](https://source.dot.net/#System.Private.CoreLib/src/libraries/System.Private.CoreLib/src/System/Numerics/IComparisonOperators.cs,75f68921e83607a1), as well as [quite a few others](https://learn.microsoft.com/en-us/dotnet/standard/generics/math#operator-interfaces). Let's take a closer look...

### Practical Examples

First, let's look at a "Fraction" class that implements the two interfaces I mentioned above (inspired by the Fraction struct in Microsoft's [operator overloading](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/operator-overloading) docs).

Implementing the `IAdditionOperators` interface forces overloading the `+` operator. For this class, I'm performing some simple math on two fractions' numerators and denominators, and not bothering with edge-cases (like a zero denominator).

Implementing the `IComparisonOperators` interface forces overloading all the other operators in the following example. To keep things simple, I'm just performing simple division (gotta watch out for the effects of [integer division](https://mathworld.wolfram.com/IntegerDivision.html)!) and using the decimal value for comparisons.

```csharp
public class Fraction : IAdditionOperators<Fraction, Fraction, Fraction>,
                        IComparisonOperators<Fraction, Fraction, bool>
{
    public Fraction(int numerator, int denominator)
    {
        Numerator = numerator;
        Denominator = denominator;
    }

    public int Numerator { get; private set; }
    public int Denominator { get; private set; }
    public decimal Value {
        get { return Numerator / (decimal)Denominator; }
    }

    public static Fraction operator +(Fraction f1, Fraction f2)
        => new(f1.Numerator * f2.Denominator + f2.Numerator * f1.Denominator, f1.Denominator * f2.Denominator);

    public static bool operator ==(Fraction? left, Fraction? right)
        => left?.Value == right?.Value;

    public static bool operator !=(Fraction? left, Fraction? right)
        => !(left == right);

    public static bool operator <(Fraction left, Fraction right)
        => left.Value < right.Value;

    public static bool operator >(Fraction left, Fraction right)
        => left.Value > right.Value;

    public static bool operator <=(Fraction left, Fraction right)
        => left.Value <= right.Value;

    public static bool operator >=(Fraction left, Fraction right)
        => left.Value >= right.Value;
}
```

A "Fraction" class, overloading addition and comparison operators

Here's another class, this time one that represents a Folder that can hold a collection of file paths.

To implement the `IAdditionOperators` interface and overload the `+` operator, I'm just creating a new Folder and adding all the files to it.

To implement the `IComparisonOperators` interface and overload the other operators, I'm just using a count of the files. If one Folder has less files, it's considered less. If two Folders have an equal number of files, even if the paths are totally different, they're still equal. That'd be problematic in reality, but oh well... it'll work for what we need to do. :)

```csharp
public class Folder : IAdditionOperators<Folder, Folder, Folder>,
                      IComparisonOperators<Folder, Folder, bool>
{
    public Folder() { }

    public Folder(List<string> filesA, List<string> filesB)
    {
        Files.AddRange(filesA);
        Files.AddRange(filesB);
    }

    public List<string> Files { get; set; } = new List<string>();

    public static Folder operator +(Folder folder1, Folder folder2)
        => new(folder1.Files, folder2.Files);

    public static bool operator ==(Folder? left, Folder? right)
        => left?.Files.Count == right?.Files.Count;

    public static bool operator !=(Folder? left, Folder? right)
        => !(left == right);

    public static bool operator <(Folder left, Folder right)
        => left.Files.Count < right.Files.Count;

    public static bool operator >(Folder left, Folder right)
        => right.Files.Count < left.Files.Count;

    public static bool operator <=(Folder left, Folder right)
        => left.Files.Count <= right.Files.Count;

    public static bool operator >=(Folder left, Folder right)
        => left.Files.Count >= right.Files.Count;
}
```

A "Folder" class, overloading the same operators

Once we've got a couple classes that implement the new interfaces... what then? Well, we can write utilities against those interfaces, to perform mathematical operations on objects without needing to know what the exact type is ahead of time.

At runtime, when the generic methods below are called, the code will grab the _actual_ implementation of whatever class is being passed to it and figure out what it means to "sum" up fractions, or find the "least" box in a collection of boxes - according to how _you_ defined it.

```csharp
public static class Utilities
{
    public static T Sum<T>(this IEnumerable<T> items) where T : IAdditionOperators<T, T, T>, new()
    {
        if (!items.Any())
            return new();

        T sum = items.First();

        foreach (var item in items.Skip(1))
            sum += item;

        return sum;
    }

    public static T? Least<T>(this IEnumerable<T> items) where T : IComparisonOperators<T, T, bool>, new()
    {
        T? min = default;

        foreach (T item in items)
            if (min == null || item < min)
                min = item;

        return min;
    }
}
```

Functions for handling math.. in a generic way (dun dun dunnnn)

```csharp
[Test]
public void CanFindTheSumOfAllTheThings()
{
    var boxes = new List<Box>
    {
        new Box(2, 7, 2),
        new Box(10, 10, 10),
        new Box(3, 4, 3),
    };

    var folders = new List<Folder>
    {
        new Folder { Files = new List<string> { "c:/file1.txt", "c:/file2.txt" } },
        new Folder { Files = new List<string> { "d:/fileA.txt", "d:/fileB.txt" } }
    };

    var fractions = new List<Fraction>
    {
        new Fraction(1, 3),
        new Fraction(2, 6),
        new Fraction(2, 5),
    };

    Assert.Multiple(() =>
    {
        Assert.That(boxes.Sum().Height, Is.EqualTo(21));
        Assert.That(folders.Sum().Files, Has.Count.EqualTo(4));
        Assert.That(fractions.Sum().Numerator, Is.EqualTo(96));  // 96/90
    });
}

[Test]
public void CanFindTheLeastOfAllTheThings()
{
    var boxes = new List<Box>
    {
        new Box(2, 1, 2),
        new Box(10, 10, 10),
        new Box(3, 4, 3),
    };

    var folders = new List<Folder>
    {
        new Folder { Files = new List<string> { "c:/file1.txt", "c:/file2.txt", "c:/file3.txt" } },
        new Folder { Files = new List<string> { "d:/fileA.txt", "d:/fileB.txt" } }
    };

    var fractions = new List<Fraction>
    {
        new Fraction(2, 3),
        new Fraction(1, 10),
        new Fraction(3, 4),
    };

    Assert.Multiple(() =>
    {
        Assert.That(boxes.Least().Height, Is.EqualTo(1));
        Assert.That(folders.Least().Files, Has.Count.EqualTo(2));
        Assert.That(fractions.Least().Denominator, Is.EqualTo(10));
    });
}
```

A few tests, to show what's happening

And that's it! That's the new Generic Math feature in C# 11. As I mentioned earlier, [all the examples here are available on GitHub](https://github.com/grantwinney/CSharpDotNetExamples/tree/master/C%23%2011/GenericMathSupport/GenericMathSupport), if you want to mess with it more.

I found a great video on YouTube demonstrating some of these same concepts, so if that's more your style, [go watch Jasper Kent's tutorial too](https://www.youtube.com/watch?v=Sclx7F8hFso). Sometimes, seeing something presented in multiple ways drives it home that much more.. at least it does for me.

One thing I didn't show is that you can reference the [`INumber<T>`](https://source.dot.net/#System.Private.CoreLib/src/libraries/System.Private.CoreLib/src/System/Numerics/INumber.cs,0f558758e750a740) interface, which references most (all?) of the other new interfaces. You'll have to implement dozens and dozens of methods, but then your new class can basically be treated like any other number type in C#. You can read more about that, and Generic Math in general, in the [Microsoft docs](https://learn.microsoft.com/en-us/dotnet/standard/generics/math).

If you found this content useful, and want to learn more about a variety of [C#](https://grantwinney.com/tags/csharp/) features, check out [this GitHub repo](https://github.com/grantwinney/CSharpDotNetExamples), where you'll find links to plenty more blog posts and practical examples!