---
categories:
  - .NET 9
  - WinForms
  - Coding
date: 2024-12-19T22:41:10Z
description: ""
draft: false
cover:
  image: folder-385530_1920.jpg
slug: selecting-multiple-directories-with-the-winforms-folderbrowserdialog-in-dotnet
summary: One of the smaller updates to make it into .NET 9 for WinForms was allowing multi-selection in the FolderBrowserDialog. Let's see how.
tags:
  - WinForms
  - Coding
  - DotNet9
title: Selecting multiple directories with the FolderBrowserDialog in .NET 9
---
It's great that, even after so many years, the teams at Microsoft continue to add updates to their oldest technologies with every .NET release. WinForms recently got a particularly small one, in .NET 9, that allows the FolderBrowserDialog to select multiple directories instead of one, so let's check it out (it won't take long, lol).

> The code in this post is available on [GitHub](https://github.com/grantwinney/Surviving-WinForms/tree/master/.NET%2009/FolderBrowserDialogMultiSelect), for you to use, extend, or just follow along while you read... and hopefully discover something new along the way!

## The old FolderBrowserDialog (one at a time, please)

The FolderBrowserDialog control has always given us an easy way to select a single folder in an app. After confirming the user pressed OK, we just read in the `SelectedPath` property and move on with life:

```csharp
if (fbd.ShowDialog() == DialogResult.OK)
    MessageBox.Show($"The path to process:\n\n{fbd.SelectedPath}");
```

![](https://grantwinney.com/content/images/2024/12/image-8.png)

There's a lot of other available properties with this control too, but they didn't change so I won't bother with them. They're there though. 😏

## The new FolderBrowserDialog (the more, the merrier)

The only change we have to make with the updated control in .NET 9 is to set the [`Multiselect` property](https://learn.microsoft.com/en-us/dotnet/api/system.windows.forms.folderbrowserdialog.multiselect) to `true`, either in the designer or at runtime:

```csharp
fbd.Multiselect = true;
```

Then we call it the same way but reference the new [`SelectedPaths` property](https://learn.microsoft.com/en-us/dotnet/api/system.windows.forms.folderbrowserdialog.selectedpaths), which is an array of strings:

```csharp
if (fbd.ShowDialog() == DialogResult.OK)
    MessageBox.Show($"The path(s) to process:\n\n{string.Join("\n", fbd.SelectedPaths)}");
```

![](https://grantwinney.com/content/images/2024/12/image-10.png)

![](https://grantwinney.com/content/images/2024/12/image-9.png)

That's it! I can't decide if it's stranger that it never had this capability, or that someone decided to add it now after all these years. Did something else change that made this a priority? Or is someone who's been with WinForms since the very beginning retiring, and this was on their bucket list? Either way, we could only choose one folder at a time before, and now we can choose as many as we'd like!