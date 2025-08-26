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
  image: https://images.unsplash.com/photo-1571171637578-41bc2dd41cd2?crop=entropy&cs=tinysrgb&fit=max&fm=jpg&ixid=MnwxMTc3M3wwfDF8c2VhcmNofDd8fHByb2dyYW1taW5nfGVufDB8fHx8MTY2NTYwNjA0OA&ixlib=rb-1.2.1&q=80&w=2000
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


I've recently been spending time (for my benefit and hopefully yours too!) reviewing some of the goodies C# has given us over the years. They mostly help make our code clearer and more concise, like local functions and string interpolation, safe null handling and the nameof operator. Others have been complete game changers, like LINQ and async/await. If you find yourself supporting a legacy app, as many of us do, I've been writing a lot about surviving WinForms in general. 😉



The code in this article is available on GitHub, if you'd like to use it in your own projects or just follow along while you read.



Today though, I'm looking at a feature that's been around for quite awhile, and that's named arguments. Even though they were introduced in C# 4.0, I don't see them being used much - not in the several old codebases I've helped support, nor in online examples. I suppose in short examples online, and in newer code bases, you're not going to see methods with tons of optional arguments, but in legacy codebases.. wow. Developers over years and decades will add one more parameter.. then one more.. then two more, another one, etc., until some methods are being passed a dozen or more.

Before I go any further, one thing to mention. If you find yourself with a bunch of related, required parameters, you might try creating a class that pulls them all together.

public void AddEmployee(string firstName, string lastName, DateTime hireDate, ...)
{
    // save employee
}

Something like this, perhaps. It cleans things up a bit, especially if the app passes the values down through multiple "business" and "database" layers. Anything calling the AddEmployee method is still going to have to create an instance of the Employee class and fill in all the values, but at least you only have to list out the fields once.

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

But what if they're not related? What if they're just a disparate collection of values that were added by a dozen people over two dozen years? I present to you ...... a mess:

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

We've got some required values in there, some optional ones with default values, and some that just make no sense at all. Why does three default to 4? Don't know, that was before my time. 🤔

In ye olden days, if you wanted to set three to 10 (oh gawd) then you'd have to pass something to all the values before it too. Even though they're optional parameters, you couldn't just skip ahead to the the one optional parameter you were interested in.

// < C# 4 - required to pass a value to all optional parameters leading up to the one you're interested in
ConfirmSave(txtUsername.Text, dtpHireDate.Value, TerminationDate, null, false, false, 3.14m, 10);

The risk is that you end up passing "null" to a string, like the "message" parameter above for instance, because hey you're not really interested in that one. Oh oops. In the method signature, it defined a default value for message if one was not provided. But you did provide a message... null.

And so the message, which was part of a prompt to the user, looks like this:

As of C# 4.0 (ye not so but still fairly olden days), we can call out parameters we're interested in, and omit the optional parameters that we aren't. So you can pass in values for the mandatory ones first (which was required), then just set three to 5. Naturally.

// C# 4.0 - provide names for optional parameters, to pass values for only those you're actually interested in
//        - positional parameters must come *before* any named optional parameters
ConfirmSave(txtUsername.Text, dtpHireDate.Value, three: 5);

The other optional parameters retain their default values, like "message" having a valid yet somewhat ominous value, and pi being set to 3.14.

Later, C# 7.2 gave us a little update to this. As long as you call everything out by name, you can pass the parameters in any order. So now you can pass the optional parameter values in first, and then required ones, in whatever order your heart desires. Go nuts.

// C# 7.2 - named parameters can be followed by positional parameters
ConfirmSave(justDoIt: true, userName: txtUsername.Text, hireDate: dtpHireDate.Value, termDate: TerminationDate);

There's one other use for this that comes to mind, and that's for purposes of self-documentation. Imagine you had to work with a method that just accepts a bunch of strings, all of which will accept values that are difficult to differentiate at first glance. Something silly like this:

private void AddMovie(string title, string tagLine, string releaseYear, string productionYear,
    string consoleGameRelease, string pcGameRelease, string soundtrackRelease)
{

}

Which of these is easier to read? It's pretty typical to keep adding parameters on to the end and make them all optional, but with named parameters you don't even have to pass them all in in the same order.

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

As usual, it's not a golden hammer and ymmv. It is an interesting feature though, and probably one that gets under-utilized. If you find a use for it, especially if it solves some unique issue you or your team's having, I'd love to hear about it below!

If you found this useful, and want to learn more about a variety of C# features, check out my GitHub repo, where you'll find links to plenty more blog posts and practical examples.

GitHub - grantwinney/CSharpDotNetExamples: Discovering and learning about the various features of the C# programming language and .NET Framework.Discovering and learning about the various features of the C# programming language and .NET Framework. - GitHub - grantwinney/CSharpDotNetExamples: Discovering and learning about the various featur…GitHubgrantwinney
