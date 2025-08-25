+++
categories = ["LINQ"]
date = 2021-06-02T16:25:14Z
description = ""
draft = false
image = "https://images.unsplash.com/photo-1501504905252-473c47e087f8?crop=entropy&cs=tinysrgb&fit=max&fm=jpg&ixid=MnwxMTc3M3wwfDF8c2VhcmNofDExfHxsZWFybmluZ3xlbnwwfHx8fDE2Nzk4Mzc4NTg&ixlib=rb-4.0.3&q=80&w=2000"
slug = "10-resources-for-learning-linq"
summary = "LINQ is a great addition to any C# dev's toolbelt. Used to query and manipulate data, similar to SQL, it leads to cleaner, more maintainable code."
tags = ["LINQ"]
title = "10 great resources for learning more LINQ"

+++


LINQ is something I just take for granted with C# anymore, since it's been around for over a decade, but some years back I was fascinated with it. I probably used it, like any newly-discovered golden hammer, for things I shouldn't have. Hopefully I've mellowed out in my LINQ usage since then. :)

Two things make it worth learning:

 1. It reduces extra lines of code.
 2. It reads more naturally, like SQL.

Take this fairly simple example, where we want to find the highest paid employee. In one case, we need temp variables, and a loop, and some logic to store off the value but only if it's the highest so far. In the second, we just tell it to order by largest first and grab the top value.

If all we want is the highest dollar amount, we'd still need a loop much like the first example. Or we could use LINQ and tell it to grab the max salary, done!

using System;
using System.Collections.Generic;
using System.Linq;
					
public class Program
{
	public static void Main()
	{
        // A list of employees, which would come from a database
		var emps = new List<Employee>
		{
			new Employee { Name = "Ann", Age = 31, Gender = "F", Salary = 50000 },
			new Employee { Name = "Brian", Age = 25, Gender = "M", Salary = 40000 },
			new Employee { Name = "Cara", Age = 50, Gender = "F", Salary = 65000 },
			new Employee { Name = "Dave", Age = 33, Gender = "M", Salary = 52000 }
		};
		
        // Loop through them all to find the highest salary
		var highestSalary = 0m;
		var highestSalaryName = "";
		foreach (var emp in emps)
		{
			if (emp.Salary > highestSalary)
			{
				highestSalary = emp.Salary;
				highestSalaryName = emp.Name;
			}
		}
		Console.WriteLine($"{highestSalaryName} earns the highest salary of ${highestSalary}.\r\n");
        	// output: Cara earns the highest salary of $65000.

        // Same thing, but a single line of LINQ
		var winner = emps.OrderByDescending(emp => emp.Salary).First();
		Console.WriteLine($"{winner.Name} earns the highest salary of ${winner.Salary}.\r\n");
			// output: Cara earns the highest salary of $65000.

        // If we don't care about who, our LINQ gets even shorter
		Console.WriteLine($"The highest salary is ${emps.Max(emp => emp.Salary)}.\r\n");
        	// output: The highest salary is $65000.
	}
}

public class Employee
{
	public string Name { get; set; }
	public int Age { get; set; }
	public decimal Salary { get; set; }
	public string Gender { get; set; }
}

You can browse the source code if you really want to get into the guts of things, but in essence, LINQ methods either:

 * Expect a collection (IEnumerable<T>) and pass back a collection of the same type, like filtering with Where, sorting with OrderBy, dividing into chunks with GroupBy, paring down the resultset with Select, etc.
 * Perform some operation that returns a result, like Max , Min , Count, ToList, etc.

It takes some time to get used to, but there are a lot of resources out there for learning LINQ. They're not all the same though, and a style that works for one person might not click for someone else, so let's check a few out. These are just the tip of the iceberg though.


DotNet Try

I think this used to be a static site, the contents of which are archived elsewhere, but it's gotten an upgrade. You have to do a small bit of configuration to install DotNet Try, then clone the try-samples repo (I wrote about it previously). Once you do, you get a comprehensive tutorial that advances through the different LINQ commands, complete with interactive code snippets you can change and run right in the browser.

101 LINQ samples - Code SamplesExplore LINQ queries using the try.net interactive experience.Microsoft DocsVSC-Service-Account


Channel 9

If you prefer videos instead, there's a series on Microsoft's Channel 9 that uses the above examples. So get the above working, then step through the examples with Bill Wagner and Mika Dumont.

Introduction to Language Integrated Query (LINQ) | C# Advanced [1 of 8]Learn what LINQ is, where you can explore LINQ syntax on your own, and how LINQ makes it easy to query any data source. Download the sample code and run these samples using the Try.NET interactive expChannel 9Bill Wagner


LINQSamples

LINQSamples has some good tutorials that explain important concepts, including method vs query syntax (yep, there's two ways to write LINQ statements) and deferred execution (write your statement now, execute it later when you need it).

There's also a comprehensive section on all the available LINQ commands, with examples written in C#, F#, and VB.NET.

LINQSamples | LINQ 101 Query and Lambda Expression SamplesLINQ 101 Code Samples in C#, VB and F# shown with both Lambda and Query syntax. Examples for all Operators such as Where, Select and Join based on LINQ-To-Objects.LINQSamples.com

Most sites don't provide for an interactive experience in the browser, but that's okay. If you don't have a local setup to try all these, visit .NET Fiddle, or just expand on this fiddle that I setup for my example at the top of this post.


Dot Net Tutorials

There's a lot here, but if you read nothing else, at least check out their section on the architecture of LINQ. They make a great case for how learning one syntax can save you a lot of time when you have to query different data sources.

There's a whole variety of data sources out there (sql, xml, text files, collection of in-memory objects, etc), but as long as there's a provider to translate LINQ to whatever each one natively needs, you can use one syntax for them all. For example, querying XML with LINQ is built into the .NET framework, and Entity Framework can help us query databases with LINQ.

LINQ Tutorial For Beginners and Professionals - Dot Net TutorialsIn this LINQ Tutorial article series, we covered basic and advanced concepts of LINQ in C#. This LINQ query will cover basic to advanceDot Net TutorialsAugust 10, 2019 at 4:36 pm


IAmTimCorey (YouTube)

This is a good intro to some basic LINQ commands. Tim Corey runs through examples for ordering and filtering a list of people, and he goes nice and slow. He's got a link in the comments to get his sample code too, so you can go through it with him.


Code Project #1

Here's an older article, from about the time LINQ was introduced, but it's comprehensive, most of the basics still apply anyway, and the author included source code. You'll need a (free) CodeProject account to download the zip file, but after letting VS2019 spend a few seconds upgrading the project, it ran just fine.

Note that the author's examples all use the query syntax, so you're only getting one half of the picture, but maybe that's your thing since query syntax more closely resembles SQL. I prefer the method syntax that looks more like standard C#, but that's just personal preference!

Using LINQ to Objects in C#This article provides an introduction to employing LINQ to Objects queries to support a simple win forms application; the article addresses the construction of LINQ to Objects statements and then goes on to describe how one might use LINQ to Objects within the context of an actual application.CodeProjectsalysle


Code Project #2 (Jovan Popovic)

Here's another one written a few years later, by someone who's a project manager at Microsoft.

The first third of the article provides a nice overview of some concepts in LINQ, like how it operates on collections, is similar to SQL, and how you can create and manipulate anonymous classes on-the-fly. That's the reason we were given the var keyword, btw, although it makes for easier-to-read code in general too.

It doesn't look like he provides a link to download his code, but he does layout exactly what his classes look like that he used throughout the article, so you can recreate them locally.

Using LINQ QueriesMost important LINQ queries explainedCodeProjectJovan Popovic(MSFT)


TutorialsTeacher

There's a lot going on in this one too. It covers the basics of LINQ, or you can skip to a specific keyword if you want to see what a command does in LINQ. It also covers more advanced topics like expression trees.

Trees aren't something you need to understand to use LINQ, but I would suggest reading about deferred execution. Briefly, it's possible to write a LINQ statement, but not execute it right away. You can modify a collection more before running it, or run it multiple times. Think of it like writing an SQL statement in SSMS, but then not hitting F5 until later.

LINQ Tutorials from Basics to AdvancedFree LINQ Tutorials for beginners - Learn the essentials of Learn Language Integrated Query, from the basics to advanced topics. Learn LINQ in easy steps starting from LINQ API, LINQ query syntax & method syntax, Lambda expression, standard query operators and expression tree.LOGO


MSDN Docs

They're pretty dry, but you can't get more authoritative than the source. If you run into a problem, check out the MSDN docs to see if there's a note or warning (usually in the "Remarks" section) for the LINQ command you're trying to use.

System.Linq NamespaceProvides classes and interfaces that support queries that use Language-Integrated Query (LINQ).Microsoft Docsdotnet-bot


LINQPad

Last but not least, there's LINQPad. This isn't a tutorial per se, but it's much more. You can connect to actual databases, and execute your LINQ statements against them directly. If you pay $60 for the pro edition, you get autocomplete, which anyone who's used Visual Studio with intellisense would understand how convenient that is.

LINQPad - The .NET Programmer’s Playground

There's a "Tutorial and Reference" section in the Help menu, which shows you how to use LINQPad and run simple queries.

You can modify the queries and try running them again, to see the effect. I converted their query syntax to method syntax, and made a small change, below.

LINQPad even installs a sample database for you to use, which you can run queries against. Very convenient for learning LINQ!

Good luck! If you find any other great resources and want to share them, reach out or leave a comment below...