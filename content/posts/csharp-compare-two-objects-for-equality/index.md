+++
categories = ["C#", "Coding"]
date = 2016-10-31T13:23:44Z
description = ""
draft = false
image = "https://images.unsplash.com/photo-1585831004044-c7531f026a9f?crop=entropy&cs=tinysrgb&fit=max&fm=jpg&ixid=M3wxMTc3M3wwfDF8c2VhcmNofDIyN3x8Y2xvbmUlMjBzaGVlcHxlbnwwfHx8fDE3MTc2MDMxMjN8MA&ixlib=rb-4.0.3&q=80&w=2000"
slug = "csharp-compare-two-objects-for-equality"
summary = "It's common to compare two objects in C# for equality, such as for a save operation. Let's take a closer look at how we define what equal means."
tags = ["C#", "Coding"]
title = "Comparing Two Objects for Equality in C#"

+++


We compare values for equality all the time in C#, so frequently that we rarely think about it most of the time:

var name = "Mike";
if (name == "")
    Console.WriteLine("Hi there, stranger.");
else
    Console.WriteLine($"Hi, {name}!");

var age = 17;
var isAdult = age >= 18;
Console.WriteLine($"{name} {(isAdult ? "is" : "is not")} an adult.");



Comparing numbers, strings, DateTime, and other out-of-the-box .NET types are the most typical examples, but what if we want to use our own type in a comparison?



The code in this article is available on GitHub, if you'd like to use it or just follow along.




Default Equality Comparison

Before we dig into that, let's consider how testing for equality works by default, without us having to do anything extra.

With few exceptions, everything we use or define in C# derives from the base Object class. In fact, we could define a class like this, with Object explicitly included:

public class Person : Object
{
    public string Name { get; set; }
    public int Age { get; set; }
}

That's unnecessary though, since it implicitly derives from Object anyway. It's enough to know that we get everything in the Object class by default, one of which is the Equals() method:

[T]he Equals(Object) method tests for reference equality, and a call to the Equals(Object) method is equivalent to a call to the ReferenceEquals method. Reference equality means that the object variables that are compared refer to the same object. (MSDN)

We can even delve into the source code and see exactly how equality is defined in the Object class:

Note that it's marked virtual, allowing us to override it – that'll be important in a moment. Also note that hovering over the == shows us a popup that there's another operation being implemented too. We can't see it in the source code, but we'll talk more about that in a minute as well.

The default test for "equality" is that two instances of a class are literally the same instance. This is true when two variables point to the same instance stored in memory. That's what we get out of the box, without doing anything else:

var person = new Person { Name = "Jay", Age = 25 };
var samePerson = person;
var newPerson = new Person { Name = "Jay", Age = 25 };

Console.WriteLine(person.Equals(samePerson));  // true, same instance
Console.WriteLine(person == samePerson);       // true, same instance

Console.WriteLine(person.Equals(newPerson));   // false, different instances
Console.WriteLine(person == newPerson);        // false, different instances

Here, samePerson references the same instance as person, so they're considered equal. However, newPerson is a new instance and thus not equal, even though the values of its properties are all the same as person.

There are cases when we want this default behavior, but more often than not we want to define our own equality. After all, if all the properties of two separate Person instances are the same, then shouldn't that be the same person?


Custom Equality Comparison

Let's create a Vehicle class now – something really simple that just stores a vehicle's make, model, and year of manufacture:

public class Vehicle
{
    public string Make { get; set; }
    public string Model { get; set; }
    public int Year { get; set; }
}

If we create two instances of the class and then try comparing them, we end up comparing the references to those two instances, just like with the Person class before:

var vehicle1 = new Vehicle { Make = "Toyota", Model = "Camry", Year = 2024 };
var vehicle2 = new Vehicle { Make = "Toyota", Model = "Camry", Year = 2024 };

Console.WriteLine(vehicle1 == vehicle2);  // false


Overriding the Equals Method

We can easily expand our Vehicle class to override the Equals() method that's defined in the Object class, and redefine what makes two Vehicle instances equal:

public class Vehicle
{
    public string Make { get; set; }
    public string Model { get; set; }
    public int Year { get; set; }

    public override bool Equals(object? obj)
    {
        if (obj is null || obj is not Vehicle)
            return false;

        var otherVehicle = (Vehicle)obj;

        if (Make != otherVehicle.Make || Model != otherVehicle.Model || Year != otherVehicle.Year)
            return false;

        return true;
    }
}

Now if we compare the two vehicles again, our code checks to make sure all three properties are the same. If they are, then the vehicles are equal:

var vehicle1 = new Vehicle { Make = "Toyota", Model = "Camry", Year = 2024 };
var vehicle2 = new Vehicle { Make = "Toyota", Model = "Camry", Year = 2024 };

Console.WriteLine(vehicle1.Equals(vehicle2));  // true
Console.WriteLine(vehicle1 == vehicle2);       // false .... why?

Well, they're mostly equal. We've overridden the Equals() method, but we have one more thing to do.


Overloading the == and != Operators

Once we override Object.Equals(), most devs are reasonably going to expect that the == operator will perform the same way and not produce different results.

Let's update the Vehicle class one more time, to overload the == and != operators so everything behaves consistently. In fact, to make life easier, let's just call Equals(). When possible, keep things DRY!

public class Vehicle
{
    public string Make { get; set; }
    public string Model { get; set; }
    public int Year { get; set; }

    public override bool Equals(object? obj)
    {
        if (obj is null || obj is not Vehicle)
            return false;

        var otherVehicle = (Vehicle)obj;

        if (Make != otherVehicle.Make || Model != otherVehicle.Model || Year != otherVehicle.Year)
            return false;

        return true;
    }

    public static bool operator ==(Vehicle x, Vehicle y)
    {
        return x.Equals(y);
    }

    public static bool operator !=(Vehicle x, Vehicle y)
    {
        return !x.Equals(y);
    }
}

Now everything should work as expected. Comparing two vehicles gives consistent results, whether using Equals() or ==:

var camry = new Vehicle { Make = "Toyota", Model = "Camry", Year = 2024 };
var alsoCamry = new Vehicle { Make = "Toyota", Model = "Camry", Year = 2024 };
var bugatti = new Vehicle { Make = "Bugatti", Model = "Chiron", Year = 2023 };

Console.WriteLine(camry.Equals(alsoCamry));  // true, custom equality logic
Console.WriteLine(camry == alsoCamry);       // true, custom equality logic

Console.WriteLine(camry.Equals(bugatti));    // false, custom equality logic
Console.WriteLine(camry == bugatti);         // false, custom equality logic


Overriding GetHashCode

Once we override the Equals() method, the compiler wants us to override the GetHashCode() method as well. Here's the tooltip that pops up over the Vehicle class:

Microsoft has a lot more to say about it in the Object.GetHashCode docs, and I suggest checking that out, but here's a few highlights (emphasis mine):

 * A hash function is used to quickly generate a number (hash code) that corresponds to the value of an object.
 * If two objects compare as equal, the GetHashCode() method for each object must return the same value.
 * Hash functions should be inexpensive to compute.
 * The GetHashCode() method should not throw exceptions.

Eric Lippert wrote about it too. He worked on the C# language, compiler, tooling, and more at Microsoft, so he's quite an authoritative source. In a nutshell though, it's enough to know that once we override the other methods, we should override this one too.

Implementing the hash code isn't difficult, really. We just need to decide which fields make a Vehicle unique, which is likely to be the same fields used in the Equal() method. If we start typing out some code to compute the hash code, VS 2022 even helpfully offers an autocompletion:

If it's possible that some fields could be null, given the last point about how GetHashCode() should never throw an exception, we might want to be a bit more robust:

public override int GetHashCode()
{
    return (Make?.GetHashCode() ?? 0) ^ (Model?.GetHashCode() ?? 0) ^ Year.GetHashCode();
}

Instead of potentially throwing a NullReferenceException, it just uses the value 0 instead.


Final Thoughts

It's useful in C# to be able to define what makes two instances of our own classes "equal". Thanks to how nearly everything derives from the Object class, and that it includes the virtual bool Equals() method for us to override, defining equality is incredibly easy!