---
categories:
- C#
- Coding
date: "2020-09-17T04:08:32Z"
description: ""
draft: false
cover:
  image: photo-1555861496-0666c8981751.jpg
slug: rethrowing-an-exception-in-csharp
summary: All programming languages have gotchas to trip you up, and C# is no exception.
  Today, let's check out the subtle (but significant) difference between "throw" and
  "throw ex".
tags:
- C#
- Coding
title: The right way to rethrow an exception in C#
---
All languages have gotchas, and C# is no different. A subtle one is the difference between `catch (Exception) throw;` and `catch (Exception ex) throw ex;`. On the surface, it seems like they'll do the same thing; in reality, the difference is really important if you care to know why your app is _really_ crashing.

Check out the following code. Can you tell what the difference will be, if any, between the two `Console` statements?

```csharp
using System;
                    
public class Program
{
    public static void Main()
    {
        Console.WriteLine("If we do a simple throw, the stacktrace should show the nitty-gritty details:\n");

        try
        {
            new PreserveStack().Do();
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.StackTrace);
        }
        
        Console.WriteLine("\nIf we do an also-simple throw ex, the stacktrace is reset:\n");

        try
        {
            new ResetStack().Do();
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.StackTrace);
        }
    }
}

public class PreserveStack
{
    public string Do()
    {
        try
        {
            return new ImportantClass().DoSomething();
        }
        catch (Exception)
        {
            throw;
        }
    }
}

public class ResetStack
{
    public string Do()
    {
        try
        {
            return new ImportantClass().DoSomething();
        }
        catch (Exception ex)
        {
            throw ex;
        }
    }
}

public class ImportantClass
{
    public string DoSomething()
    {
        return new LessImportantButJustAsSpecial().DoSomething();
    }
}

public class LessImportantButJustAsSpecial
{
    public string DoSomething()
    {
        return new NowWereInTheWeeds().DoSomething();
    }
}

public class NowWereInTheWeeds
{
    public string DoSomething()
    {
        throw new ArgumentException("What does I did?? There are so many weeds down here!");
    }
}
```

The `PreserveStack.Do` function does a `throw`, which includes a stack trace all the way down to the `NowWereInTheWeeds` class that threw the exception, allowing a developer to quickly get to the root of the problem.

The `ResetStack.Do` function does a `throw ex`. The difference? The stack trace is reset, dropping everything further down the line, hiding the true origin of the exception. Minor difference in code, major difference in the end result!

```none
If we do a simple throw, the stacktrace should show the nitty-gritty details:

   at NowWereInTheWeeds.DoSomething() in d:\Windows\Temp\5cydernb.0.cs:line 81
   at LessImportantButJustAsSpecial.DoSomething() in d:\Windows\Temp\5cydernb.0.cs:line 73
   at ImportantClass.DoSomething() in d:\Windows\Temp\5cydernb.0.cs:line 65
   at PreserveStack.Do() in d:\Windows\Temp\5cydernb.0.cs:line 41
   at Program.Main() in d:\Windows\Temp\5cydernb.0.cs:line 11

If we do an also-simple throw ex, the stacktrace is reset:

   at ResetStack.Do() in d:\Windows\Temp\5cydernb.0.cs:line 56
   at Program.Main() in d:\Windows\Temp\5cydernb.0.cs:line 22
```

I ran this against .NET Core 3.1 and .NET 4.7.2 to see if it's still an issue after all these years, and it is... but that's no surprise. Microsoft takes great pains to maintain backwards compatibility, whether in the .NET Framework or [in Windows](https://www.youtube.com/watch?v=vPnehDhGa14). Since any number of devs might have intentionally used a thing, even if it appears to be subpar, [only very seldom do they make a breaking change](https://ericlippert.com/2009/11/12/closing-over-the-loop-variable-considered-harmful-part-one/).

If you want to try it out yourself, [fiddle with it here](https://dotnetfiddle.net/BNYEy2).

## But.. why??

So, this begs the question.. since the `throw ex` resets the stacktrace, when would that behavior be desirable? The only thing I can come up with is obfuscation of some sort. If you had a library of code, and someone else wrote an app that consumed it, maybe you'd want to bury the true source of the exception (after logging it, hopefully?) from the calling app.

```csharp
public class ResetStack
{
    public string Do()
    {
        try
        {
            return new ImportantClass().DoSomething();
        }
        catch (Exception ex)
        {
            // log the original exception and stack trace

            throw ex; // reset the stack trace
        }
    }
}
```

But that seems useless anyway. If they had the compiled code, they could decompile it to debug it. If they're hitting an API, you probably want to return a useful error instead of a raw exception in any case.

The official docs failed me here. The [legacy docs](https://learn.microsoft.com/en-us/previous-versions/dotnet/netframework-4.0/ms229005(v=vs.100)) simply warn against it:

> [P]refer using an empty throw when catching and re-throwing an exception. This is the best way to preserve the exception call stack. The following code example demonstrates catching an exception and incorrectly specifying it when re-throwing the exception. This causes the stack trace to point to the re-throw as the error location....

While [newer docs](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/throw#re-throwing-an-exception) include it as an option, with the resulting behavior called out, but no opinion on whether or not it might be a bad thing. (I think it generally is.)

> You can also use the `throw e` syntax in a `catch` block to instantiate a new exception that you pass on to the caller. In this case, the stack trace of the original exception, which is available from the [StackTrace](https://docs.microsoft.com/en-us/dotnet/api/system.exception.stacktrace#System_Exception_StackTrace) property, is not preserved.