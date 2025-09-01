---
categories:
  - Coding
date: 2014-12-12T07:57:57Z
description: ""
draft: false
cover:
  image: hal-gatewood-613602-unsplash.jpg
slug: winforms-passing-data-between-two-forms
summary: Passing data between two Forms is very common in WinForms. There's a couple ways to do it, and one's better than the other. Let's take a look.
tags:
  - winforms
  - csharp
title: Passing Data Between Forms in WinForms
---
In any but the smallest of WinForms apps, we'll have multiple Forms interacting with one another. And while every app we write will be different, there's really only a couple of ways for two Forms to pass data back and forth.

Let's take a closer look.

## Child Form Pushes Data Back to Parent

When one Form displays another Form to collect user input, or to display a database record for editing, the changes a user makes need to make it back to the first Form or risk being lost.

One way to pass the data is to push it back to the parent Form from the child Form. **A** *`*Form*`* **is just another class, and in order to call methods or change properties of** _**any**_ **class, you need to have a reference to an instance of it.** There's some caveats to doing it this way, which I'll point out in a minute.

Let's assume we have a simple app with just two Forms - a parent and a child. Here's the code behind `ParentForm`:

```csharp
private void btnGetUserInput_Click(object sender, EventArgs e)
{
    using (var childForm = new ChildForm(this))
    {
        childForm.ShowDialog();
    }
}

public void SetName(string name)
{
    lblName.Text = name;
}

public int Age
{
    set { lblAge.Text = value.ToString(); }
}
```

It displays `ChildForm` on a button push, sending it a reference to itself. It also defines a method and a property, made public so the child Form can access them.

Now let's look at the code behind `ChildForm`:

```csharp
private readonly ParentForm parentForm;

public ChildForm(ParentForm form)
{
    InitializeComponent();
    parentForm = form;
}

private void btnSaveInput_Click(object sender, EventArgs e)
{
    parentForm.SetName(txtName.Text);

    parentForm.Age = int.TryParse(txtAge.Text, out var age) ? age : 0;
}
```

In the constructor, it accepts the reference from `ParentForm` and stores it. When the "Save Input" button is pushed, it uses that reference to call the method and property on `ParentForm`, passing the data back to it.

So what's so bad about this code?

### Problems With This Approach

I see two issues with this code, and the first is reusability. Imagine that next week, we want to use `ChildForm` from another, new Form. `ChildForm` will need to be reworked because it currently has a constructor that expects to be passed `ParentForm`. We'll have to add more code and more complexity to `ChildForm`, so it's not easily reusable.

The second issue is that `ChildForm` has knowledge it doesn't need. There is no reason for `ChildForm` to know about other forms, user controls, class libraries, etc that could potentially use it. In general, a thing being _called_ should know very little (or nothing) about the thing calling it.

Imagine as our app grows, we have two Forms calling `ChildForm` to get a name and age. One calls a new instance of `ChildForm`, passing an instance of itself, and has a public `EmployeeName` property that `ChildForm` can call:

```csharp
private void btnGetUserInput_Click(object sender, EventArgs e)
{
    using (var childForm = new ChildForm(this))
    {
        childForm.ShowDialog();
    }
}

public string EmployeeName
{
    set { lblName.Text = value; }
}
```

The other does the same, except it has a public `SetStudentName()` method for `ChildForm` to call instead:

```csharp
private void btnGetUserInput_Click(object sender, EventArgs e)
{
    using (var childForm = new ChildForm(this))
    {
        childForm.ShowDialog();
    }
}

public void SetStudentName(string name)
{
    lblStudentName.Text = name;
}
```

The logic in `ChildForm` increases in complexity with each new object calling it:

```csharp
private readonly ParentForm1 parentForm1;
private readonly ParentForm2 parentForm2;

public ChildForm(ParentForm1 form)
{
    InitializeComponent();
    parentForm1 = form;
}

public ChildForm(ParentForm2 form)
{
    InitializeComponent();
    parentForm2 = form;
}

private void btnSaveInput_Click(object sender, EventArgs e)
{
    if (parentForm1 != null)
        parentForm1.EmployeeName = txtName.Text;
    else if (parentForm2 != null)
        parentForm2.SetStudentName(txtName.Text);
}
```

Now `ChildForm` needs to know about two potential callers, as well as which property or method to call, depending on how it was called. We could try to simplify things, perhaps by introducing an `IParentForm` interface, but no matter what, things are getting ugly.

Let's look at a better way...

## Parent Form Pulls Data from Child

If we make the data available from the second Form, then let the individual callers retrieve as much or as little of the data as they need, then `ChildForm` doesn't need to change at all, no matter how many other objects are referencing it.

The easiest way to do this is to create public "getter" methods on `ChildForm`:

```csharp
public partial class DetailForm : Form
{
    public DetailForm()
    {
        InitializeComponent();
    }

    public new string Name => txtName.Text;

    public int Age => int.TryParse(txtAge.Text, out int result) ? result : 0;
}
```

The two fields on `ChildForm`, to collect a name and age, are both made accessible to other forms, classes, etc that might need them. It doesn't know for sure that a caller _will_ need them, or anything else about potential callers – and that's a good thing.

Here's how one Form might call `ChildForm`, and it only cares about the name:

```csharp
private void btnGetUserInput_Click(object sender, EventArgs e)
{
    using (var childForm = new ChildForm())
    {
        childForm.ShowDialog();

        lblEmployeeName.Text = childForm.Name;
    }
}
```

Then another Form calls `ChildForm`, this time keeping both name _and_ age:

```csharp
private void btnGetUserInput_Click(object sender, EventArgs e)
{
    using (var childForm = new ChildForm())
    {
        childForm.ShowDialog();

        lblStudentName.Text = childForm.Name;
        lblAge.Text = childForm.Age.ToString();
    }
}
```

The `ChildForm` is left cleaner. It requires no special knowledge of its callers, and has greater reusability and maintainability. The callers can grab as much or as little data as they need, or do nothing at all.

## Final Thoughts

There's two practical choices for passing data between two Forms:

- While we're still in Form2, push data back to Form1.
- After we return to Form1, pull data from Form2.

The second option leads to easier code maintenance and greater usability.

I hope this helped clarify a few things. If it still seems unclear, or you see a possible error somewhere, leave a comment below and we’ll figure it out!