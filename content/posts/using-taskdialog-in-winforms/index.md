---
categories:
- .NET 5
- WinForms
- Surviving WinForms
date: "2024-12-31T23:41:30Z"
description: ""
draft: false
cover:
  image: kelly-sikkema-mC0KIWO0yYA-unsplash.jpg
slug: using-taskdialog-in-winforms
summary: In .NET 5, WinForms got a major upgrade to the MessageBox called TaskDialog.
  It's way more flexible and powerful - let's check it out!
tags:
- .NET 5
- WinForms
- Surviving WinForms
title: TaskDialog, a new message box for WinForms in .NET 5
---


Since the earliest versions of .NET, the MessageBox class has given WinForms developers a way to send notifications (usually alerts and warnings) to users. It's always been a very limited control though. Besides the message itself, we can change the icon and choose from a few different button combinations, but that's about it.

In .NET 5, we got a new control called TaskDialog that allows for much more customization. Instead of one main text area, the UI supports a more complex interface for displaying larger messages. Instead of a few preset button combos, we can create our own.. and even define new buttons.



The code in this post is available on GitHub, for you to use, extend, or just follow along while you read... and hopefully discover something new along the way!



It's great that the team behind WinForms has the bandwidth to add new features, not just bandaid old bugs. Let's compare the two controls and take a look at what we can do with TaskDialog.


Replacing MessageBox

Without considering anything fancier, it's super easy to replace a standard MessageBox. Here's a simple message to warn the user when they're about to delete some files:

The code for the above is pretty succinct. We want Yes/No buttons with No as the default:

var result = MessageBox.Show(
    caption: "Continue?",
    text: "You're about to delete the selected files! Continue?",
    icon: MessageBoxIcon.Warning,
    buttons: MessageBoxButtons.YesNo,
    defaultButton: MessageBoxDefaultButton.Button2);

if (result == DialogResult.Yes)
{
    // proceed to delete files...
}

And here's the a prompt with the same behavior, recreated using TaskDialog:

The code is very similar. We set the properties on a TaskDialogPage object and pass that to the TaskDialog.ShowDialog method:

var result = TaskDialog.ShowDialog(new TaskDialogPage
{
    Caption = "Continue?",
    Text = "You're about to delete the selected files! Continue?",
    Icon = TaskDialogIcon.Warning,
    Buttons = { TaskDialogButton.Yes, TaskDialogButton.No },
    DefaultButton = TaskDialogButton.No,
    SizeToContent = true,
});

if (result == TaskDialogButton.Yes)
{
    // proceed to delete files...
}

There's a couple interesting things to point out straightaway. Firstly, we've specified the buttons we wanted separately – we're not constrained by specific combinations of buttons. Secondly, we specified the default button using the name of the button, not the generic Button1, Button2, etc.

What else can we do?


A Friendlier Error Message

For the sake of argument, let's say we're working on an app where the decision was made to display unhandled exceptions to the user, with the full stack trace. In the static void Main() method of the Program class, we can subscribe to the ThreadException event to catch all otherwise uncaught exceptions:

Application.ThreadException += Application_ThreadException;

And then the method itself, to just display everything in a MessageBox:

private static void Application_ThreadException(
    object sender, ThreadExceptionEventArgs e)
{
    MessageBox.Show(e.Exception.ToString(), "Application Exception");
}

Here's what we get. Ouch.

With the new TaskDialog, we can do so much more:

var submitError = new TaskDialogButton("Submit Error");
submitError.Click += (s, e) => MessageBox.Show(
    "Thank you for notifying us. We're on it. Seriously.", "Submit Error");

TaskDialog.ShowDialog(new TaskDialogPage
{
    Caption = "Application Exception",
    Heading = "An exception has occurred!",
    Text = e.Exception.Message,
    Icon = TaskDialogIcon.Error,
    AllowCancel = true,
    Expander = new TaskDialogExpander
    {
        Text = e.Exception.StackTrace,
        CollapsedButtonText = "Stack Trace",
    },
    Buttons = { TaskDialogButton.OK, submitError },
    DefaultButton = TaskDialogButton.OK,
    SizeToContent = true,
});

We can hide the details in a collapsed section, so they're out of the way initially. We can add our own buttons for additional actions, like submitting the error. The AllowCancel flag allows them to hit Esc to dismiss the message, and SizeToContent widens the box to support the contents better, so the stack trace text isn't more awkwardly wrapped than necessary.


Accept Our EULA

Or maybe we need users to accept our completely uninvasive, non-creepy EULA before using our app?

var eulaSummary = "omitted for brevity";
var eulaDetail = "omitted for brevity";

var vcb = new TaskDialogVerificationCheckBox("Accept EULA");
var tdp = new TaskDialogPage
{
    Caption = "Accept EULA",
    Heading = "EULA",
    Text = eulaSummary,
    Footnote = $@"© <a href=""https://example.org/"">ShadyBiz LLC</a>, 2013 - {DateTime.Now.Year}",
    Icon = TaskDialogIcon.ShieldWarningYellowBar,
    AllowCancel = true,
    Expander = new TaskDialogExpander
    {
        Text = eulaDetail,
        CollapsedButtonText = "Nothing to see here...",
    },
    Buttons = { new TaskDialogButton("OK", false) },
    SizeToContent = true,
    EnableLinks = true,
    Verification = vcb,
};

vcb.CheckedChanged += (s, e) => tdp.Buttons[0].Enabled = vcb.Checked;
tdp.LinkClicked += (s, e) => Process.Start("explorer", e.LinkHref);

if (TaskDialog.ShowDialog(this, tdp) == TaskDialogButton.OK)
{
    // record the user's acceptance of the EULA
}

The Verification property lets us add a custom TaskDialogVerificationCheckBox, and by subscribing to its CheckedChanged event, we can force users to select it before hitting OK. There's an icon option that adds a yellow bar to really grab attention when needed, and a flag that lets us include hyperlinks in the text.


Self destruct in...

Or maybe we want to notify the user that they only have 5 seconds to take some action!!! This doesn't work very well, since once a dialog is displayed, the user must take some action to make it go away. But I want to show off another feature of the TaskDialog control and I'm running low on ideas. 😏

var pb = new TaskDialogProgressBar { Value = 5, Minimum = 0, Maximum = 5 };
var msg = "This message will self-destruct in {0} seconds...";

var tdp = new TaskDialogPage
{
    Caption = "Final Countdown",
    Text = string.Format(msg, 5),
    ProgressBar = pb,
    Icon = TaskDialogIcon.Information,
};

var tmr = new Timer() { Interval = 1000, Enabled = true };
tmr.Tick += (s, e) =>
{
    if (pb.Value > 0)
    {
        pb.Value -= 1;
        tdp.Text = string.Format(msg, pb.Value);
    }
    else
        tdp.Text = "Boom? ¯\\_(ツ)_/¯";
};

var tdResult = TaskDialog.ShowDialog(this, tdp);

if (tdResult == TaskDialogButton.OK && pb.Value == 0)
{
    // You were warned...
}

The ProgressBar property lets us add a custom TaskDialogProgressBar to show progress. Here, we've set it to 5 initially, and then we use a standard WinForms Timer to count down for 5 seconds. I love that we can add a ProgressBar, although a good use-case is escaping me at the moment. Maybe you have one you'd like to share?

If you found this content useful and would like to learn more, check out my Surviving WinForms repo, where you'll find links to plenty more blog posts and practical examples!

GitHub - grantwinney/Surviving-WinForms: WinForms ain’t going away anytime soon. A lot of us have to deal with it. Let’s make the most of it, shall we? 😎WinForms ain’t going away anytime soon. A lot of us have to deal with it. Let’s make the most of it, shall we? 😎 - grantwinney/Surviving-WinFormsGitHubgrantwinney
