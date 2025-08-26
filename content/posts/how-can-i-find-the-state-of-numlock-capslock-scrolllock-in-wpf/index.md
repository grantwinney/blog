---
categories:
- WPF
- C#
- Coding
date: "2019-11-12T00:30:00Z"
description: ""
draft: false
cover:
  image: https://images.unsplash.com/flagged/photo-1552911942-1f6cea756cf0?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=2000&fit=max&ixid=eyJhcHBfaWQiOjExNzczfQ
slug: how-can-i-find-the-state-of-numlock-capslock-scrolllock-in-wpf
summary: If you're writing a WPF application and need to find the state of the Num
  Lock, Caps Lock, or Scroll Lock keys, you're in luck - there's a method for that.
tags:
- WPF
- C#
- Coding
title: How can I find the state of NumLock, CapsLock or ScrollLock in WPF?
---


If you're writing a WPF application and need to find the state of the Num Lock,
Caps Lock, or Scroll Lock keys, you can use the Keyboard.IsToggled
[https://msdn.microsoft.com/en-us/library/system.windows.input.keyboard.iskeytoggled(v=vs.110).aspx] 
method (introduced in .NET 3.0):

var isNumLockToggled = Keyboard.IsKeyToggled(Key.NumLock);
var isCapsLockToggled = Keyboard.IsKeyToggled(Key.CapsLock);
var isScrollLockToggled = Keyboard.IsKeyToggled(Key.Scroll);

Add this using directive to the top of your class, if it's not already there:

using System.Windows.Input;

Internally, the IsToggled()
[http://referencesource.microsoft.com/#PresentationCore/Core/CSharp/System/Windows/Input/Keyboard.cs,22f8500adfc561fb] 
method checks to see whether or not a KeyStates.Toggled
[http://referencesource.microsoft.com/#PresentationCore/Core/CSharp/System/Windows/Input/KeyStates.cs,78ceabc4eeaa31fc] 
flag is set for the specified key.

[Flags]
public enum KeyStates : byte
{
    None = (byte) 0,
    Down = (byte) 1,
    Toggled = (byte) 2,
}
