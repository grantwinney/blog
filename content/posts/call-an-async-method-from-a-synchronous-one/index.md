---
categories:
- Surviving WinForms
- Async
- Multithreading
- Coding
date: "2023-08-11T03:59:55Z"
description: ""
draft: false
cover:
  image: https://images.unsplash.com/photo-1541397436527-c5d6df584ce1?crop=entropy&cs=tinysrgb&fit=max&fm=jpg&ixid=M3wxMTc3M3wwfDF8c2VhcmNofDEzfHx3YWl0aW5nJTIwaW4lMjBsaW5lfGVufDB8fHx8MTY5MTUzMjM0N3ww&ixlib=rb-4.0.3&q=80&w=2000
slug: call-an-async-method-from-a-synchronous-one
summary: Writing async code whenever possible is great, but how do we do it when we're
  stuck with legacy (and very synchronous) code?
tags:
- Surviving WinForms
- Async
- Multithreading
- Coding
title: How to call an async method from a synchronous one, without deadlocking
---


The async/await model introduced with C# 5.0 (over a decade ago) is probably one of the best things added to the language, right up there with LINQ (introduced a few years earlier in C# 3.0). In the last few years, as I've read up more on async/await and understand it better, I try to implement it where I reasonably can. In fresh code, like a new API or a side project, that's relatively easy. Not so much in older code.

As great as async is, it can be tricky in a legacy app where it's just not feasible to update everything at once (despite that being popular advice). When you're dealing with tens of thousands (or even millions) of lines of code, organized in a dozen layers representing multiple architectures, written over a couple decades by dozens of developers, wide sweeping changes are usually a recipe for disaster. And even for those brave souls that laugh in the face of such disaster, few companies are going to happily let someone spend days on a task that should've taken a few hours, just because they decided to make the code "better". That's a tough sell any day of the week.

That being said, what I'm going to show you is an anti-pattern of sorts, and it even has a catchy name - "sync over async" - which is explained (and discouraged) by the likes of Stephen Toub, David Fowler, and Stephen Cleary (all very reliable sources in the world of C#/.NET) . I'll link to their articles at the end.

You should avoid this if you can. But what if you can't?



The code in this post is available on GitHub, for you to use, expand upon, or just follow along while you read... and hopefully discover something new!



Let's look at a few ways to do what we (sometimes) gotta do, starting with what we should never do, and finishing up with what we really ought to do.

We'll also assume there's an async method doing some really important stuff.

public async Task<string> ImportantStuffAsync(IProgress<int> progress)
{
    await Task.Delay(1000);
    progress.Report(1);

    await Task.Delay(1000);
    progress.Report(2);

    await Task.Delay(1000);
    progress.Report(3);

    return $"Done! ({DateTime.Now:G})";
}


How to deadlock an app (bad)

The most obvious way to call async code from sync code is also the most obviously wrong way. Seeing an async method and, knowing you want the result, one might be tempted to just call the method directly and then access .Result.

// Example 1 - Let's cause a deadlock
private void btnExample1_Click(object sender, EventArgs e)
{
    // As soon as we call .Result or .Wait() here, all is lost...
    var result = ImportantStuffAsync(new Progress<int>()).Result;

    // ... the UI thread is deadlocked, so just restart the app. :(
}

As far as I understand it:

 * A call to .Result or .Wait() blocks the current (UI) thread while it waits for the Task to complete.
 * When the Task is finished, it attempts to take control of the UI thread to wrap up its work (like returning the value).. but it can't.
 * The Task code can't access the UI thread until the call to .Result or .Wait() completes, but the call to .Result or .Wait() will never complete until it gets the response from the Task. Deadlock.

Possibly useless analogy time.. bear with me. This makes me think of an elevator, where the main UI thread is the elevator shaft, and the elevator itself is the single piece of work that can be handled at any time. If it appears many things are happening at once, it's because the elevator flies up and down the shaft at breakneck speed.

Occasionally, someone sticks their foot in the doorway (running some numbers, jotting some notes down, grabbing a coffee), preventing anyone else from using the elevator. That's the frozen UI. Then it finishes, the UI unfreezes, and the elevator moves again.

In this case though, the main UI thread starts the Task, and then calls the elevator to its floor and sticks its foot in the door, waiting for a response. It will not take its foot out until the Task responds. But when the Task finishes, it calls the elevator to load the result in. It will not be completely finished until the elevator arrives and it can do that. There's a stalemate. They're both trying to use the same resource, and neither will give up until the other is finished.


How to avoid deadlocks (better)

The quickest fix for the above problem is to run the async code in its own Task, which allows the code in the async method to run in a separate thread from the UI, avoiding the deadlocking issue. There's a few ways you might approach this.

One option is to just run it without waiting for the Task to even complete. The UI stays responsive, but you won't get the result, if any. And if you hoped to lock down any part of the UI while the Task was running, that won't work either.

// Example 2 - Call async method from a sync method, without bothering to wait
private void btnExample2_Click(object sender, EventArgs e)
{
    // Lock parts of the UI that should be inaccessible while the task runs
    pnlButtons.Enabled = false;

    Task.Run(() => ImportantStuffAsync(progress));

    // OOPS! The panel will be re-enabled before the Task completes
    pnlButtons.Enabled = true;
}

You could also wait for the Task to complete and get the result, if any. The downside here is that the UI freezes while the UI thread waits for the Task to complete. The upside is that it eventually unfreezes, instead of deadlocking.

// Example 3 - Call async method from a sync method, but wait until it completes (freezes UI)
private void btnExample3_Click(object sender, EventArgs e)
{
    pnlButtons.Enabled = false;

    Task.Run(() => ImportantStuffAsync(progress)).Wait();

    pnlButtons.Enabled = true;
}

// Example 4 - Call async method from a sync method, but wait for return value
private void btnExample4_Click(object sender, EventArgs e)
{
    pnlButtons.Enabled = false;

    var result = Task.Run(() => ImportantStuffAsync(progress)).Result;
    lblReturnValue.Text = result;
    
    pnlButtons.Enabled = true;
}


How to async all the things (best)

The best option though, when you can, is to mark everything async on up the chain. Since event methods can be marked async in WinForms, it means a really small change in my really small example. Just "await" the async method and then mark the click event as async too, and watch the magic happen.

// Example 5 - Call async method from another async method.. the right way
private async void btnExample5_Click(object sender, EventArgs e)
{
    pnlButtons.Enabled = false;

    var result = await ImportantStuffAsync(progress);
    lblReturnValue.Text = result;
    
    pnlButtons.Enabled = true;
}


Learning More

If you're interested in learning more, check out pretty much anything on the topic of async from Stephen Cleary. He's been writing about it since it came out, and you'll see his answers all over the SO forums, and some books, and on his blog. He's everywhere all at once, in true async fashion.

 * Async and Await

Here's a comprehensive post I found from Stephen Toub, who's worked at Microsoft since C# became a thing. Set aside a few hours (days?) to take it in though.. it's a packed post to say the least. And a couple more I came across.

 * How Async/Await Really Works in C#
 * Async/Await FAQ
 * Await, and UI, and deadlocks! Oh my!

And from David Fowler, another longtime Microsoft employee who works on .NET and ASP.NET Core, a loong list of do's and don'ts type of advice when it comes to async.

 * ASP.NET Core Diagnostic Scenarios - Asynchronous Programming

If you want to read more about multithreading and async code, I wrote a couple other posts about it too, from the perspective of using them in WinForms.

Using Async, Await, and Task to keep the WinForms UI responsiveUsing the async/await pattern in WinForms is an easy win, helping prevent one of the most annoying user experiences - a frozen UI.Grant WinneyGrant WinneyTurning a BackgroundWorker into a Task with TaskCompletionSourceSometimes a safer way to “update” old code is to leave it be and paint over it with a newer construct, like with BackgroundWorker and Tasks.Grant WinneyGrant Winney
