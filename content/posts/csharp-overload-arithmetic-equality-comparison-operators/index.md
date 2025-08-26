---
categories:
- C#
- Coding
- Series Generic Math Intro
date: "2023-04-01T03:54:20Z"
description: ""
draft: false
cover:
  image: photo-1581079289196-67865ea83118.jpg
slug: csharp-overload-arithmetic-equality-comparison-operators
summary: What's it mean to overload operators in C#? And what's that have to do with
  Generic Math in C# 11? Let's find out! (part 2 of 3)
tags:
- C#
- Coding
- Series Generic Math Intro
title: Overloading arithmetic, equality, and comparison operators in C#
---
This is post 2 in a 3-part series building up to a new C# 11 feature called [Generic Math](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-11#generic-math-support). Before tackling that though, I covered [static abstract members](https://grantwinney.com/whats-a-static-abstract-interface-method-in-c/) (also new to C# 11), and now I want to look at overload operators (not new, but worth knowing).

In C# you can [overload operators](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/operator-overloading), putting you in control of how two instances of an object are related to one another. We'll look at the different ways you can do that, but first...

## Arithmetic Operators

By overloading the arithmetic operators, you get to decide what it means to add two objects together. In Microsoft's own [example](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/operator-overloading), they create a Fraction struct that defines what should happen when two fractions are added together, subtracted, etc.

Let's define a Box class instead. We can decide to say that, if two boxes are added together, you should get back a box that could fit both of them. It'll take the larger width and depth of the two boxes, and then combine their heights, so you can stack both smaller boxes inside the larger one. Notice how the overloaded operator is a static method that takes the two instance you want to compare (add, in this case).

This is incredibly __not__ optimal, but judging by some of the packages I've gotten from Amazon with very small items inside, I might not be that far off from the industry standard. 😅

```csharp
public class Box
{
    public Box(int width, int height, int depth)
    {
        Width = width;
        Height = height;
        Depth = depth;
    }

    public int Width { get; private set; }
    public int Height { get; private set; }
    public int Depth { get; private set; }

    public static Box operator +(Box box1, Box box2)
    {
        var widestWidth = Math.Max(box1.Width, box2.Width);
        var deepestDepth = Math.Max(box1.Depth, box2.Depth);
        var combinedHeight = box1.Height + box2.Height;

        return new Box(widestWidth, combinedHeight, deepestDepth);
    }
}


[Test]
public void AddingTwoBoxes_ReturnsBoxToFitBoth()
{
    var box1 = new Box(2, 3, 7);
    var box2 = new Box(4, 6, 5);

    var box3 = box1 + box2;

    Assert.Multiple(() =>
    {
        Assert.That(box3.Width, Is.EqualTo(4));   // larger width from box2
        Assert.That(box3.Depth, Is.EqualTo(7));   // larger depth from box1
        Assert.That(box3.Height, Is.EqualTo(9));  // combined height
    });
}
```

How about another? A Folder class this time, that can hold a list of files.. If someone tries to add two Folders together, they get back a new Folder with __all__ the files in it, like this.

```csharp
public class Folder
{
    public Folder() { }

    public Folder(List<string> filesA, List<string> filesB)
    {
        Files.AddRange(filesA);
        Files.AddRange(filesB);
    }

    public List<string> Files { get; set; } = new List<string>();

    public static Folder operator +(Folder fdr1, Folder fdr2) =>
        new(fdr1.Files, fdr2.Files);
}

[Test]
public void AddingTwoFolders_CombinesTheirFiles()
{
    var folder1 = new Folder
    {
        Files = new List<string> { "c:/file1.txt", "c:/file2.txt" }
    };

    var folder2 = new Folder
    {
        Files = new List<string> { "d:/fileA.txt", "d:/fileB.txt" }
    };

    var bigFolder = folder1 + folder2;

    Assert.Multiple(() =>
    {
        Assert.That(bigFolder.Files, Has.Count.EqualTo(4));
        Assert.That(bigFolder.Files.SingleOrDefault(f => f.Contains("file2")), Is.Not.Null);
        Assert.That(bigFolder.Files.SingleOrDefault(f => f.Contains("fileA")), Is.Not.Null);
    });
}
```

Both of the above examples overloaded the `+` operator, but there's no reason you can't overload a different operator.. or all of them. We'll do one more with arithmetic, this time overloading the `*` operator.

How about a class that can hold shades of colors, kind of like a [paint swatch](https://sureswatch.com/utilizing-paint-swatches-to-revamp-your-house/)? Multiplying two of them together should combine all the colors from one swatch with colors from the other, so 3 colors on one side and 3 on the other produces 9 "mixed" colors.

```csharp
public class ColorSwatch
{
    public IEnumerable<Color> Shades { get; }
    public string Name { get; private set; }

    public ColorSwatch(string name, IEnumerable<Color> shades)
    {
        Name = name;
        Shades = shades.ToList();
    }
    
    public static IList<Color> operator *(ColorSwatch cs1, ColorSwatch cs2)
    {
        var colorMatrix = new List<Color>();
        var amount = 0.5;

        foreach (var s1 in cs1.Shades)
        {
            foreach (var s2 in cs2.Shades)
            {
                // Thank you Timwi.. stackoverflow.com/a/3722337
                byte r = (byte)(s1.R * amount + s2.R * (1 - amount));
                byte g = (byte)(s1.G * amount + s2.G * (1 - amount));
                byte b = (byte)(s1.B * amount + s2.B * (1 - amount));
                colorMatrix.Add(Color.FromArgb(r, g, b));
            }
        }

        return colorMatrix;
    }
}
```

This one's a little tougher to imagine the result of, so I wrote a quick little app that creates a colored square for each of the shades, and then a square for each "mixed" color. I won't throw the code for that here, but it's on [GitHub](https://github.com/grantwinney/CSharpDotNetExamples/tree/master/C%23%2011/GenericMathSupport/GenericMathSupport) too if you want to see it.

![](https://grantwinney.com/content/images/2023/04/image-1.png)

## Comparison Operators

There's comparison operators too, like greater than and less than. Using the Box class again, let's decide that a box is __less than__ another box if it's area is smaller; likewise, it's __greater than__ another box if it's area is larger.

This could be represented like this, where the operators are again static method that take two "box" objects to compare.

```csharp
public class Box
{
    public Box(int width, int height, int depth)
    {
        Width = width;
        Height = height;
        Depth = depth;
    }

    public int Width { get; private set; }
    public int Height { get; private set; }
    public int Depth { get; private set; }

    public int Area { get => Width * Height * Depth; }

    public static bool operator <(Box box1, Box box2)
        => box1.Area < box2.Area;

    public static bool operator >(Box box1, Box box2)
        => box2.Area < box1.Area;
}

[Test]
public void Box1IsLessThanBox2_WhenAreaIsSmaller()
{
    var box1 = new Box(2, 3, 7);
    var box2 = new Box(2, 4, 7);

    Assert.Multiple(() =>
    {
        Assert.That(box1 < box2);
        Assert.That(box2 > box1);
    });
}
```

There's other comparison (aka relational) operators too, but you probably get the point by now.

## Equality Operators

Let's check out equality operators too, and then call it a day. :)

By overriding the `==` and `!=` operators, you can decide what makes two instances equal. Building off of Microsoft's example with the [Fraction struct](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/operator-overloading), you might do something like this. Of course this isn't very robust since 1/2 and 2/4 won't be seen as equal, but you get the idea.

```csharp
public static bool operator ==(Fraction a, Fraction b)
    => a.num == b.num && a.den == b.den;

public static bool operator !=(Fraction a, Fraction b)
    => !(a == b)
```

Or maybe in that same `Box` class again, you want to be able to compare two boxes to see if they're the same size. We could compare the area again like above, or maybe a different approach is to just check all 3 dimensions to see if they're the same size, like this. Not very robust, but it'll do for our purpose!

```csharp
public class Box
{
    public Box(int width, int height, int depth)
    {
        Width = width;
        Height = height;
        Depth = depth;
    }

    public static bool operator ==(Box box1, Box box2)
        => box1.Width == box2.Width &&
           box1.Height == box2.Height &&
           box1.Depth == box2.Depth;

    public static bool operator !=(Box box1, Box box2)
        => !(box1 == box2);

    public int Width { get; private set; }
    public int Height { get; private set; }
    public int Depth { get; private set; }
}
```

I wrote a longer post about equality operators a few years ago too, if you want to read more about it and see some different examples.

[Comparing Two Objects for Equality in C#](https://grantwinney.com/csharp-compare-two-objects-for-equality/)

Next up, we'll see how operation overloading and the [static abstract concept](https://grantwinney.com/whats-a-static-abstract-interface-method-in-c/) applies to [Generic Math](https://grantwinney.com/whats-generic-math-support-in-csharp/).

If you found this content useful, and want to learn more about a variety of [C#](https://grantwinney.com/tag/csharp/) features, check out [this GitHub repo](https://github.com/grantwinney/CSharpDotNetExamples), where you'll find links to plenty more blog posts and practical examples!