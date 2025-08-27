---
categories:
- Surviving WinForms
- C# 4
- C# 7.2
- Coding
- C#
date: "2022-10-12T22:09:47Z"
description: ""
draft: false
cover:
  image: photo-1571171637578-41bc2dd41cd2.jpg
slug: csharp-named-arguments
summary: Named arguments in C#.. they've been around a long time, but does anyone
  use them? Let's check out another feature that helps tame wild code.
tags:
- Surviving WinForms
- C# 4
- C# 7.2
- Coding
- C#
title: Named arguments in C#
---
I've recently been spending time __(for my benefit and hopefully yours too!)__ reviewing some of the goodies C# has given us [over the years](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-version-history). They mostly help make our code clearer and more concise, like [local functions](https://grantwinney.com/local-functions-in-csharp-aka-nested-methods/) and [string interpolation](https://grantwinney.com/using-string-interpolation-to-craft-readable-strings/), [safe null handling](https://grantwinney.com/null-conditional-and-null-coalescing-operators/) and the [nameof](https://grantwinney.com/using-nameof-to-avoid-magic-strings/) operator. Others have been complete game changers, like [LINQ](https://grantwinney.com/10-resources-for-learning-linq/) and [async/await](https://grantwinney.com/using-async-await-and-task-to-keep-the-winforms-ui-more-responsive/). If you find yourself supporting a legacy app, as many of us do, I've been writing a lot about [surviving WinForms](https://grantwinney.com/tag/surviving-winforms/) in general. 😉

Today though, I'm looking at a feature that's been around for quite awhile, and that's [named arguments](https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/named-and-optional-arguments). Even though they were introduced in C# 4.0, I don't see them being used much - not in the several old codebases I've helped support, nor in online examples. I suppose in short examples online, and in newer code bases, you're not going to see methods with tons of optional arguments, but in legacy codebases.. wow. Developers over years and decades will add one more parameter.. then one more.. then two more, another one, etc., until some methods are being passed a dozen or more.

Before I go any further, one thing to mention. If you find yourself with a bunch of related, required parameters, you might try creating a class that pulls them all together.

```csharp
public void AddEmployee(string firstName, string lastName, DateTime hireDate, ...)
{
    // save employee
}
```

Something like this, perhaps. It cleans things up a bit, especially if the app passes the values down through multiple "business" and "database" layers. Anything calling the AddEmployee method is still going to have to create an instance of the Employee class and fill in all the values, but at least you only have to list out the fields once.

```csharp
public void AddEmployee(Employee employee)
{
    // save employee
}

...
...

public class Employee
{
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public DateTime HireDate { get; set; }
    // etc
}
```

But what if they're not related? What if they're just a disparate collection of values that were added by a dozen people over two dozen years? I present to you ...... a mess:

```csharp
private void ConfirmSave(string userName, DateTime hireDate, DateTime? termDate = null,
    string message = "It might be unwise to save today. Continue?", bool isTuesday = false,
    bool isFullMoon = false, decimal pi = 3.14m, int three = 4, bool justDoIt = false)
{
    if (justDoIt)
    {
        // db.Save(...);
    }
    else if (isTuesday && !isFullMoon && three == 5)
    {
        if (MessageBox.Show(message, "Are you sure about this?",
            MessageBoxButtons.YesNo, MessageBoxIcon.Question, MessageBoxDefaultButton.Button2) == DialogResult.Yes)
        {
            // db.Save(...);
        }
    }
    else if (pi <= 3.14m)
    {
        // db.Save(...);
    }
}
```

We've got some required values in there, some optional ones with default values, and some that just make no sense at all. Why does three default to 4? Don't know, that was before my time. 🤔

In ye olden days, if you wanted to set three to 10 (oh gawd) then you'd have to pass __something__ to all the values before it too. Even though they're optional parameters, you couldn't just skip ahead to the the one optional parameter you were interested in.

```csharp
// < C# 4 - required to pass a value to all optional parameters leading up to the one you're interested in
ConfirmSave(txtUsername.Text, dtpHireDate.Value, TerminationDate, null, false, false, 3.14m, 10);
```

The risk is that you end up passing "null" to a string, like the "message" parameter above for instance, because hey you're not really interested in that one. Oh oops. In the method signature, it defined a default value for message if one was not provided. But you __did__ provide a message... null.

And so the message, which was part of a prompt to the user, looks like this:

![](https://grantwinney.com/content/images/2022/10/image-3.png)

As of C# 4.0 (ye not so but still fairly olden days), we can call out parameters we're interested in, and omit the optional parameters that we aren't. So you can pass in values for the mandatory ones first (which was required), then just set three to 5. Naturally.

```csharp
// C# 4.0 - provide names for optional parameters, to pass values for only those you're actually interested in
//        - positional parameters must come *before* any named optional parameters
ConfirmSave(txtUsername.Text, dtpHireDate.Value, three: 5);
```

The other optional parameters retain their default values, like "message" having a valid yet somewhat ominous value, and pi being set to 3.14.

Later, C# 7.2 gave us a little update to this. As long as you call everything out by name, you can pass the parameters in any order. So now you can pass the optional parameter values in first, and then required ones, in whatever order your heart desires. Go nuts.

```csharp
// C# 7.2 - named parameters can be followed by positional parameters
ConfirmSave(justDoIt: true, userName: txtUsername.Text, hireDate: dtpHireDate.Value, termDate: TerminationDate);
```

There's one other use for this that comes to mind, and that's for purposes of self-documentation. Imagine you had to work with a method that just accepts a bunch of strings, all of which will accept values that are difficult to differentiate at first glance. Something silly like this:

```csharp
private void AddMovie(string title, string tagLine, string releaseYear, string productionYear,
    string consoleGameRelease, string pcGameRelease, string soundtrackRelease)
{

}
```

Which of these is easier to read? It's pretty typical to keep adding parameters on to the end and make them all optional, but with named parameters you don't even have to pass them all in in the same order.

```csharp
AddMovie("WarGames", "Is it a game, or is it real?", "1983", "1979", "1983", "1998", "1983");

AddMovie(
    title: "WarGames",
    tagLine: "Is it a game, or is it real?",
    productionYear: "1979",
    releaseYear: "1983",
    soundtrackRelease: "1983",
    consoleGameRelease: "1984",
    pcGameRelease: "1998"
);
```

As usual, it's not a golden hammer and ymmv. It __is__ an interesting feature though, and probably one that gets under-utilized. If you find a use for it, especially if it solves some unique issue you or your team's having, I'd love to hear about it below!

If you found this useful, and want to learn more about a variety of C# features, check out [my GitHub repo](https://github.com/grantwinney/CSharpDotNetExamples), where you'll find links to plenty more blog posts and practical examples.