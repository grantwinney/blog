---
categories:
- Async
- Surviving WinForms
- Coding
date: "2024-10-07T02:43:55Z"
description: ""
draft: false
cover:
  image: multitasking-7303815_1920-1.jpg
slug: async-in-5-short-examples
summary: Async code isn't always intuitive, but practicing helps. Let's take a look
  at Async, CancellationToken, and IProgress, in a few short examples.
tags:
- Async
- Surviving WinForms
- Coding
title: Async, CancellationToken, and IProgress in 5 Short Examples
---
Learning to write code asynchronously does _not_ come naturally, at least not for this dev. We're wired to give the majority of our attention to [one thing at a time](https://www.psychologytoday.com/us/blog/creativity-without-borders/201405/the-myth-of-multitasking), so it can be difficult to write code that takes advantage of the fact that a computer can multitask _very_ well.

A few years ago, I wrote [an async article](https://grantwinney.com/using-async-await-and-task-to-keep-the-winforms-ui-more-responsive/) that shows (in a small way) how much faster it can be when we tell the computer to do many things at once. The trick is that we have to tell it which things can be done in parallel, and safely bring everything back together at the end.

I don't feel completely comfortable with `async` yet, but learning to use and get comfortable with it really intrigues me, in the same way that LINQ did years ago. Here's a few examples I put together to show off a little `async` code as well as cancellation tokens and how to report progress. I'm not running a lot of parallel code here, but I _am_ running things in a way that the task can be canceled and the UI won't be locked up.

> The code in this post is available on [GitHub](https://github.com/grantwinney/Surviving-WinForms/tree/master/Threading/SimpleAsyncExamples), for you to use, extend, or just follow along while you read... and hopefully discover something new along the way!

## A 5-second Task that just completes

Here's a really simple example that just waits 5 seconds and then completes.

- Either the Run or the Cancel button (in later examples) should be enabled – never both at once
- Task.Delay effectively pauses for x seconds, in a way that won't block the UI

```cs
// * AsyncUI form *
// Ex 1: Runs 5-second task and then completes

private async void btnRunTask1_Click(object sender, EventArgs e)
{
    btnRunTask1.Enabled = false;
    lblStatusAsync1.Text = "Running...";

    try
    {
        await SimpleAsyncMethods.Example1Async();
    }
    catch (Exception ex)
    {
        MessageBox.Show(ex.Message, "An error occurred", MessageBoxButtons.OK, MessageBoxIcon.Error);
    }

    btnRunTask1.Enabled = true;
    lblStatusAsync1.Text = "Completed!";
}

// * SimpleAsyncMethods class *
// Ex 1: Task.Delay is an easy way to simulate a long-running job

public static async Task Example1Async()
{
    await Task.Delay(5000);
}
```

For all of these examples, like the one above, there's two classes at play.

The first is the code-behind file of a WinForms app, where the button click event methods live, but you can use this kind of logic in any .NET app. The second is a static class that defines what each `Task` actually does.

## A 5-second Task that auto-cancels in 3 seconds

Here's another simple example. It also waits 5 seconds, although it checks for a pending cancellation once a second, and cancels the `Task` if needed. It also sets the `CancellationTokenSource` to automatically cancel after 3 seconds, so it'll never run to completion.

- The `CancellationTokeSource` is configured to automatically cancel after 3 seconds
- The `Task` checks for a cancellation request every second, which throws an `OperationCancelledException` if needed
- The caller is catching the `OperationCanceledException` to update the user

```cs
// * AsyncUI form *
// Ex 2: Runs 5-second task for 3 seconds and then cancels it
//  A CancellationTokenSource can be automatically canceled after a set delay

private async void btnRunTask2_Click(object sender, EventArgs e)
{
    // By using 'using', the CancellationTokenSource will be disposed of automatically
    using CancellationTokenSource cancelTokenSource2 = new(TimeSpan.FromSeconds(3));

    try
    {
        await SimpleAsyncMethods.Example2Async(cancelTokenSource2.Token);
        lblStatusAsync2.Text = "Completed!";
    }
    catch (OperationCanceledException)
    {
        // We can catch the OperationCanceledException and act on that
        lblStatusAsync2.Text = "Canceled!";
    }
}

// * SimpleAsyncMethods class *
// Ex 2: By periodically calling ThrowIfCancellationRequested, we effectively check for a
//  pending cancellation and throw an OperationCanceledException with a single line of code

public static async Task Example2Async(CancellationToken cToken)
{
    for (var i = 0; i < 5; i++)
    {
        cToken.ThrowIfCancellationRequested();
        await Task.Delay(1000);
    }
}
```

I've left out most of the code that's enabling buttons, updating labels, etc, outside of stuff directly related to the `Task` to reduce some of the clutter, but if you check out the code on GitHub, you'll see it's still doing it.

## A 10-second Task that's user cancellable

This example adds something new – a button the user can press to cancel the `Task`. It checks for a cancellation request every half-second for 10 seconds.

- The `Token.Register()` method lets us specify code to run on cancellation, instead of catching the exception
- The code will likely still throw an `OperationCanceledException`, and here we'll just ignore it

```cs
// * AsyncUI form *
// Ex 3: Runs 10-second task, during which it can be canceled

CancellationTokenSource _cancelTokenSource3;

private async void btnRunTask3_Click(object sender, EventArgs e)
{
    _cancelTokenSource3 = new();

    // Instead of catching the OperationCanceledException, we can register some code to run if the token is canceled.
    _cancelTokenSource3.Token.Register(() => lblStatusAsync3.Text = "Canceled!");

    try
    {
        await SimpleAsyncMethods.Example3Async(_cancelTokenSource3.Token);
        lblStatusAsync3.Text = "Completed!";
    }
    catch (Exception ex)
    {
        // Since cancellation is being handled by Register() above, we'll ignore it here
        if (ex is not OperationCanceledException)
            MessageBox.Show(ex.Message, "An error occurred", MessageBoxButtons.OK, MessageBoxIcon.Error);
    }
    finally
    {
        // Dispose of the CancellationTokenSource to free up resources
        _cancelTokenSource3.Dispose();
    }
}

private void btnCancelTask3_Click(object sender, EventArgs e)
{
    _cancelTokenSource3.Cancel();
}

// * SimpleAsyncMethods class *
// Ex 3: CancellationToken can be passed to, and handled by, other .NET classes that accept it 

public static async Task Example3Async(CancellationToken cToken)
{
    for (var i = 0; i < 20; i++)
    {
        await Task.Delay(500, cToken);
    }
}
```

## An endless Task that's user cancellable

This one's just a slight variation of the last one, representing some `Task` that runs a very long time (maybe any time the app is running).

- The `Task` runs forever until the user presses the Cancel button to stop it
- It checks every tenth of a second for a cancellation request

```cs
// * AsyncUI form *
// Ex 4: Runs task indefinitely, during which it can be manually canceled

CancellationTokenSource _cancelTokenSource4;

private async void btnRunTask4_Click(object sender, EventArgs e)
{
    _cancelTokenSource4 = new();
    _cancelTokenSource4.Token.Register(() => lblStatusAsync4.Text = "Canceled!");

    try
    {
        await SimpleAsyncMethods.Example4Async(_cancelTokenSource4.Token);
        lblStatusAsync4.Text = "Completed!";
    }
    catch (OperationCanceledException)
    {
        // Eat the exception or, assuming exceptions bubble up to be handled elsewhere (i.e. logging),
        //  remove this catch and allow it to be handled the same way
    }
    finally
    {
        _cancelTokenSource4.Dispose();
    }
}

private void btnCancelTask4_Click(object sender, EventArgs e)
{
    _cancelTokenSource4.Cancel();
}

// * SimpleAsyncMethods class *
// Ex 4: A task can run indefinitely until its canceled

public static async Task Example4Async(CancellationToken cToken)
{
    while (true)
    {
        cToken.ThrowIfCancellationRequested();
        await Task.Delay(100);
    }
}
```

## A 10-second Task that reports progress updates

This last example introduces one more concept – the `IProgress<T>` construct, which allows us to send progress updates from the `Task` to the caller.

- The `T` can be any type – here it's a `Tuple<int, string>` representing a percentage complete and a status message
- The `Task` returns a status update every second, which may be a message that it's been canceled
- The `Task` also throws an `OperationCanceledException` on cancellation, because that's expected; as the caller, we can choose to ignore it
- Since we're using a status message to update the user, we just eat the exception

```cs
// * AsyncUI form *
// Ex 5: Runs 10-second task that's cancellable AND reports progress as it runs

CancellationTokenSource _cancelTokenSource5;

private async void btnRunTask5_Click(object sender, EventArgs e)
{
    _cancelTokenSource5 = new();

    var progress = new Progress<(int percentage, string message)>();
    progress.ProgressChanged += (_, update) =>
    {
        lblStatusAsync5.Text = update.message;
        progBarTask5.Value = update.percentage;
    };

    try
    {
        await SimpleAsyncMethods.Example5Async(_cancelTokenSource5.Token, progress);
    }
    catch (OperationCanceledException)
    {
        // eat the exception.. nom nom nom
    }
    finally
    {
        _cancelTokenSource5.Dispose();
    }
}

private void btnCancelTask5_Click(object sender, EventArgs e)
{
    _cancelTokenSource5.Cancel();
}

// * SimpleAsyncMethods class *

// Ex 5: By checking IsCancellationRequested, we can cancel a long-running task
//  Also, the IProgress<T> construct lets us pass progress updates to the caller

public static async Task Example5Async(CancellationToken cToken, IProgress<(int,string)> progress)
{
    var secondsToRun = 10m;

    for (var i = 0; i < secondsToRun; i++)
    {
        var percentage = (int)((i + 1) / secondsToRun * 100);

        await Task.Delay(1000);

        // If we want to perform some additional logic on cancellation,
        //  we can throw an OperationCanceledException instead of using ThrowIfCancellationRequested
        if (cToken.IsCancellationRequested && percentage != 100)
        {
            progress.Report((percentage, $"Canceled at {percentage}%"));
            throw new OperationCanceledException("Ex 5 Canceled!", cToken);
        }
        else
            progress.Report((percentage, $"{percentage}% Complete!"));
    }
}
```

## Learning More

If you'd like to read more, I've written a few other articles on [async](https://grantwinney.com/tags/async/) that may or may not be helpful.

One of the best sources I've found for all things async is Stephen Cleary's blog. After reading through his 5-part _(as of the time of this writing)_ series on [Cancellation](https://blog.stephencleary.com/2022/02/cancellation-1-overview.html) recently, I decided to play around a bit and share what I learned – hence the article you just read. 😄