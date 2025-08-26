---
categories:
- Security
- Questions
date: "2019-06-14T16:14:00Z"
description: ""
draft: false
cover:
  image: photo-1519791746699-ffdaa2d9ec36.jpg
slug: minification-vs-obfuscation
summary: Mozilla announced they'll no longer accept extensions with obfuscated code.
  It's good news for users, maybe not so much for developers. Obfuscated code is (intentionally)
  nearly impossible to understand, and could easily be malicious. Let's unpack and
  break down a few concepts.
tags:
- Security
- Questions
title: What is minification vs obfuscation?
---


Mozilla recently announced that they'll no longer accept extensions with obfuscated code. This is good news for anyone who uses browser extensions in Firefox, since such code is (intentionally) nearly impossible to understand, and could easily (but not necessarily, as I'll explain later) be malicious.

From their source code submission guidelines:

"Extensions using obfuscated code are not permitted, regardless of whether they are hosted on addons.mozilla.org (AMO) or not. Extensions using obfuscated code are in violation of our Add-on Policy and are subject to being blocked."

"Code is considered obfuscated if the logic and meaning is transformed in a way intended to make it difficult for a human to understand or reverse-engineer. A commonly used tool is JavaScript Obfuscator, and there are a number of other tools that can conceal code functionality."

"Not all code that is difficult to read is obfuscated, and we specifically allow minified code to be submitted along with [source code]. With minification, the intent is to reduce the file size of the code. Techniques used here include reducing the length of variable and function names or removing whitespaces, comments and other redundant syntax elements."

There's a few different concepts packed in there, so let's break it down. Keep in mind though that it mostly comes down to intent - one is for the user's benefit; the other is for the developer's.


What's minified code?

Minifying your code means reducing its size to make it faster - download faster, parse faster, maybe even run faster. Let's just consider JavaScript, though these concepts could apply to any language.

// let the world know you exist

let name = 'Grant';

alert(`Hello World, this is ${name}!`);

I could "minify" this by hand, just by removing anything that's unnecessary - blank lines, comments, long variable names, etc. This does exactly the same thing while cutting the file size in half (97 bytes to 50).

let n='Grant';alert(`Hello World, this is ${n}!`);

While it doesn't matter so much for compiled languages like C#, it can make a large difference for JavaScript and other interpreted languages. Every space, extra line, or long variable name causes longer transmission times from some server to your computer, more data usage on a user's phone, and more time to load code into memory and run it.

Minifying code has real benefits, and there are plenty of online tools to help you do it.


What's obfuscated code?

Obfuscating your code means making it as unreadable as possible, to thwart others from understanding it. Best case scenario, it might be to hide intellectual property. Worst case, it's to hide some malicious intent. Here's the same "hello world" script, run through the JavaScript Obfuscator Tool:

var _0x4c59=['Grant','Hello\x20World,\x20this\x20is\x20'];
(function(_0x283f45,_0x4aad64){
    var _0x4bcaa6=function(_0x226500){
        while(--_0x226500){_0x283f45['push'](_0x283f45['shift']());}
    };
    _0x4bcaa6(++_0x4aad64);
}(_0x4c59,0x14c));
var _0x46ec=function(_0x3f6a4c,_0x5a9af4){
    _0x3f6a4c=_0x3f6a4c-0x0;
    var _0x24debb=_0x4c59[_0x3f6a4c];
    return _0x24debb;
};
let name=_0x46ec('0x0');
alert(_0x46ec('0x1')+name+'!');

Copy it and run it from the browser console - it does the same thing, but it's 5x the size of the original unminified version. This definitely doesn't help the end-user, and it could be doing anything in addition to displaying an alert.


Blurring the lines

A minified file might be so difficult to understand that it might as well be obfuscated. Compare jQuery before and after minification. Good luck understanding the minified version. It still looks vaguely like English (and it's 30% of the original size!), but it'd take a lot of work to figure out what it's doing.

On the other hand, obfuscators can usually minimize code too. Even with longer names and crazy looking code, the generated file will still be smaller than the original size... but nowhere near as small as the plain minified file.


Why bother at all?

If you're an optimist, you might argue that someone wrote some JavaScript code (such as a browser extension) for profit, and is just trying to protect their intellectual property. Or a company may mandate that their programmers obfuscate all code, for the same reason. You can't hide your code forever by obfuscating it, but you can certainly slow down all but the most determined. A better option might be to have a thin UI that makes API calls to the proprietary stuff running on a server somewhere.

If you're a pessimist, then someone using an obfuscator is trying to hide something malicious. It might not be, but it's nearly impossible to know for sure (although watching the browser console might provide some clues). So Mozilla drew a line in the sand - if you're making your code nearly impossible to read, it raises bright red flags and they won't allow it.

Again, it mostly comes down to intent - one is for the user's benefit; the other is for the developer's. If you trust the author, that's one thing. But if you don't 100% trust them, then the obfuscated code could be doing just about anything.


What does this mean for developers?

It depends on what you might be trying to do with your browser extension. If you're writing something you find useful and sharing it with the world for free, then you're unlikely to care about obfuscating your code. Likewise, if you're writing something as an employee that supports a larger service, it's likely everything really important is tucked away server side, and you're making API calls to it... so again, obfuscation isn't really necessary.

If you were planning on retiring on a fully client-side Firefox extension, and you protected your intellectual property with obfuscation, well.... you'll need to come up with another plan.

One of the more interesting parts is where they state, "regardless of whether they are hosted on addons.mozilla.org (AMO) or not". It sounds as if not only are they going to block extensions in the Addons store, but they're going to block obfuscated addons from other sources too - which suggests they're running some heuristics to determine whether loaded extensions have obfuscated code and they'll just disable them. I'm not sure what to make of that...


Further reading...

If you want to learn more about different forms of obfuscation, check out:
What is Obfuscation and how does it apply to Java, Android, .NET and iOS applications?

And I highly, highly recommend (seriously, can't recommend it enough) checking out the Chrome Extension Source Viewer (available for Firefox, Chrome and Opera, as well as any site that can load Chrome extensions like Brave). If you doubt an extension at all, you can easily check out its source code before adding it. I've used it quite a few times, and discovered some very interesting and shady things in the process.
