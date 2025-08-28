---
categories:
- VS4Mac
- Testing
- Tools of the Trade
date: "2017-07-06T12:15:51Z"
description: ""
draft: false
cover:
  image: photo-1576444356170-66073046b1bc.jpg
slug: tdd-test-driven-development-using-visual-studio-for-mac
summary: Are you a Mac user and .NET fan? Did you know there's a native VS app now?
  Writing tests is important, so I decided to try out NUnit in @vs4mac.
tags:
- VS4Mac
- Testing
- Tools of the Trade
title: Unit Testing in Visual Studio for Mac
---
Last month at a user group, they selected the [magic square kata](https://github.com/gigasquid/wonderland-clojure-katas/tree/master/magic-square), which was a new one for me. Basically, you arrange 9 unique numbers in a 3x3 grid such that they add up to the same number horizontally, vertically and diagonally. I paired up with someone else who knew C#, and we tackled the kata in Visual Studio for Mac.

Although I've kicked the tires on [VS4Mac](https://visualstudio.microsoft.com/vs/mac/) a bit, one of the things I hadn't tested out was, well.. testing!

## Method 1: An NUnit Library Project

The easiest method is to just create a new "NUnit Library Project". The VS4Mac team actually added a project type that includes the NUnit package and a test file out of the box. How convenient is that??

### Create a new project

Go to: `File / New Solution / Other / .NET / NUnit Library Project`

![](https://grantwinney.com/content/images/2017/07/vs4mac-test-setup01-1.png)

![](https://grantwinney.com/content/images/2017/07/vs4mac-test-setup02-1.png)

### Create a class and some tests

There's already a "Test.cs" file ready to go, with the proper NUnit attributes and everything. Let's create a regular class and add a couple tests against it.

![](https://grantwinney.com/content/images/2017/07/vs4mac-test-setup03.png)

### Run the tests

If the "Unit Test" pane (or pad as they call it on the Mac) isn't visible, open it: `View / Pads / Unit Tests`

Click the build button (black triangle in upper-left) to see the new tests, if necessary. Or just click the "Run All" button in the Unit Tests pad.

Let's change the logic so the tests fail (if they didn't already) and check out the failure results in the "Test Results" pad at the bottom. If that pad isn't visible, open it now: `View / Pads / Test Results`

![](https://grantwinney.com/content/images/2017/07/vs4mac-test-setup04.png)

That's it! Using VS4Mac for TDD during a code kata doesn't get much easier than that. :)

## Method 2: Add NUnit to an Existing Project

But what if we already have a project and just want to add tests to it?

Start by creating a Library project to act as the "existing project":  
`File / New Solution / Other / .NET / Library`

![](https://grantwinney.com/content/images/2017/07/vs4mac-test-setup01.png)

![](https://grantwinney.com/content/images/2017/07/vs4mac-test-setup02.png)

If the "Solution" pad isn't visible on the side: go to: `View / Pads / Solution`

### Create a Test File

Right-click the project and choose `Add / New File.` Select `General / Empty Class` and name it "MagicSquareTests.cs". Alternatively, do what I did and just rename the default "MyClass.cs" as my MagicSquare class, which should look something like this:

![](https://grantwinney.com/content/images/2017/07/vs4mac-test-setup05.png)

### Add the NUnit Package via NuGet

Right-click on Packages in the Solution pad and choose "Add Packages". All we need is NUnit, not the NUnit Console Runner.

![](https://grantwinney.com/content/images/2017/07/vs4mac-test-setup06.png)

![](https://grantwinney.com/content/images/2017/07/vs4mac-test-setup07.png)

The NUnit folder should be visible under the Packages folder.

### Create a Few Tests

Let's add some new tests to run against whatever logic the old project has. In my case, I added a single function for the magic square kata, and wrote a couple tests against it that were sure to fail:

![](https://grantwinney.com/content/images/2017/07/vs4mac-test-setup08-1.png)

The test runner tells us what failed and where.

### Run / Observe / Fix / Repeat!

Try adding enough code to get the tests to pass, and run again. Green = good!

![](https://grantwinney.com/content/images/2017/07/vs4mac-test-setup09-1.png)