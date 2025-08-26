---
categories:
- Surviving WinForms
- Debugging
- Coding
date: "2021-12-07T15:37:03Z"
description: ""
draft: false
cover:
  image: https://images.unsplash.com/photo-1493953522814-88c4f52f6509?crop=entropy&cs=tinysrgb&fit=max&fm=jpg&ixid=MnwxMTc3M3wwfDF8c2VhcmNofDU1fHxpZGVhfGVufDB8fHx8MTYzODg4NTk0NA&ixlib=rb-1.2.1&q=80&w=2000
slug: the-helpful-exception-box
summary: "If you're in a legacy codebase with a centralized \"message box\" form,
  why not add some features that make it work for you? \U0001F60F"
tags:
- Surviving WinForms
- Debugging
- Coding
title: A more helpful exception box for WinForms apps
---


I saw a suggestion like a week or two ago that had me cracking up, and I can't for the life of me remember where I saw it. Maybe LinkedIn, maybe Twitter.. I didn't mark it, so it's buried deep in my timeline never to be seen again. But the gist of it was someone asking whether just slapping a button on an error prompt that led straight to stack overflow was a legit way to help the end user.



If you'd like to follow along with the code in this post, it's available on GitHub.



After I laughed about it for a few seconds, I stopped to think... is it really that crazy? Maybe. It's not something I'd ever want to present to the user, but what about just showing it to the developers or QA? Wouldn't it be convenient, when an error pops up, to just have a link that takes you right to the place you were probably (admit it) about to search anyway?

And so I present.. an exceptional exception box.

Most of the codebases I've worked in are old. I'd wager most of the codebases out there anywhere are old. Most companies that've been around a couple decades have a homegrown project or two, with lots of layers, written by lots of devs. And at some point someone always adds their own message box class that encapsulates the C# MessageBox, with helpful methods like ShowInfo and ShowError that standardize which buttons and icons are shown for each type of prompt, and in what order, etc.

If you happen to be in that boat, you can take advantage of that to create some friendly links that take you right to StackOverflow, MSDN, or wherever else you happen to find the answers to your coding woes. Add some preprocessor directives to make sure the buttons don't show in production, a couple more buttons to open the full stack trace for easy viewing, and you're on your way to making your own special mark in that legacy codebase. 😂

public partial class ExceptionalBox : Form
{
    private readonly Exception exception;

    public ExceptionalBox(Exception exception)
        : this(exception.Message, exception) { }

    public ExceptionalBox(string userFriendlyMessage, Exception exception)
    {
        InitializeComponent();
            
        lblMessage.Text = userFriendlyMessage;
        this.exception = exception;

#if DEBUG || DEVELOPMENT || STAGING
        btnSOS.Visible = btnMS.Visible = btnCopy.Visible = btnNote.Visible = true;
#endif
    }

    private void btnSOS_Click(object sender, EventArgs e)
    {
        Process.Start(new ProcessStartInfo($"https://stackoverflow.com/search?q={exception.GetType()}+{exception?.Message.Replace(' ', '+')}")
        {
            UseShellExecute = true,
            Verb = "open"
        });
    }

    private void btnMS_Click(object sender, EventArgs e)
    {
        Process.Start(new ProcessStartInfo($"https://docs.microsoft.com/en-us/search/?terms={exception.GetType()}+{exception?.Message.Replace(" ", "%20")}")
        {
            UseShellExecute = true,
            Verb = "open"
        });
    }

    private void btnCopy_Click(object sender, EventArgs e)
    {
        Clipboard.SetText(exception.ToString());
    }

    private void btnNote_Click(object sender, EventArgs e)
    {
        var tempFile = $"{Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString())}.txt";
        File.WriteAllText(tempFile, exception.ToString());
        Process.Start("notepad.exe", tempFile);
    }
}

