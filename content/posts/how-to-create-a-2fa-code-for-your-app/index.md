---
categories:
- 2FA
- Coding
- Security
date: "2019-08-13T03:54:37Z"
description: ""
draft: false
cover:
  image: finger-2081169_1280.jpg
slug: how-to-create-a-2fa-code-for-your-app
summary: I use 2FA on every site that supports it, but I'd never given much thought
  to how a 2FA code is generated. Let's learn how!
tags:
- 2FA
- Coding
- Security
title: Create a TOTP 2FA code for your app
---


I've been using 2FA on every site that supports it for quite some time, but I've never given much thought to how a 2FA code is created. I enable it, scan the QR code, and print the backup codes. The rest is magic. 🧙‍♂️

But no more! Today is the day we figure out how to generate a 2FA code...


A few basics...

If you've used 2FA and know what TOTP is, skip this section. For everyone else...


What is 2FA?

For the totally uninitiated it just means "two factor authentication", or a second way to authenticate yourself, often by generating a random code using an app on your phone. More generically, it means proving who you are by providing a combination of something you have, something you know, something you are, etc.

Even if you don't think you've ever used 2FA before, if you have...

 * Ever inserted an ATM card (have) and entered a PIN (know)?
 * Or inserted a credit card (have) and entered a zip code (know)?
 * Presented your insurance card (have) and verified your birth date (know)?
 * Shown your top-secret government id (have) and submitted to a retinal scan (are)?

This stuff used to apply to relatively few things though - a bank account and a few credit cards. Now we have dozens (or hundreds) of logins, each of which secures a site that holds some of our private data, and the more ways you have to prove who you are, the less likely someone can pretend to be you.

There's a lot of ways to do 2FA, but some are more common than others...

 * Enter a password (know), and answer canned questions (know). (awful - check out how many known data breaches included security questions and answers, which most people reuse)
 * Enter a password (know), and answer custom questions (know). (meh)
 * Enter a pw (know), then a code that's texted to your phone (have). (not the best)
 * Enter a pw (know), then a randomly generated, one-time code from your phone that expires in 30 seconds (have), aka TOTP.


What is TOTP?

A time-based one-time password (TOTP) is just one way to do 2FA... but it's very common. A TOTP only works for a certain period of time (usually 30 seconds), and it only works once (so trying to log in twice with the same password should fail).


What's it look like?

A website generates a QR code to scan with an app like andOTP, Microsoft Authenticator, 1Password, etc. The QR code represents a URI with a few pieces of data, including a random string that's unique to you, which is encoded in base32 (more on that later). From then on, the app generates a new random code every 30 seconds, which you use at login. And in case your phone catches fire, they provide some recovery codes to print and save.

If someone guesses your password, or hacks your email and requests a password recovery, they still can't login unless they also have access to your 2FA app (which usually means having physical access to your phone as well).


How are they generated?

From the user's perspective, it starts with a QR code that represents a URI, as I mentioned above. Using a service like ZXing Decoder on one of those QR codes, we see it holds a few pieces of data, as outlined here: Key URI Format


Dissecting a QR code

Let's take a minute to break that "Raw text" line down:

 * Type: The "totp" indicates this is a time-based one-time code.
 * Label: The "csinfotest" is a label used to identify the code in the authenticator app.
 * Issuer: The "Namecheap - ...." param is also a label. (because reasons...)
 * Secret: A base32 encoded key (more on that in a moment) that's unique for each user, and used in conjunction with the current time to generate the TOTP code.

"But", you might say, "that Key URI Format webpage mentions other fields too!"

Yep, there are optional parameters; but when I tested a code that used them, the results were inconsistent. Google's and Microsoft's apps ignored the extra params, while andOTP and LastPass show an 8 digit number for 15 seconds, but not the same number. 🤔

For now, I'd suggest just setting the 4 basic fields, to generate a code that'll work everywhere.


What is base32 encoding?

Of the four pieces of data mentioned above, the one that makes this whole thing work is the base32 "secret" string, so let's review what base32 encoding / decoding is.

Basically, it means converting a string that could have any data in it to a string that's a set of 32 characters. The exact set of 32 characters can vary depending on the use case, but they're often selected for specific reasons, such as:

 * a legacy system that can't handle certain characters,
 * the need to be easily human readable (so not using O and 0, l and 1, etc),
 * use as a file name (so not \ or /)

If you're having trouble sleeping, check out RFC 4648 concerning base16, base32, and base64, particularly section 3.4 on choosing the alphabet. It's riveting.

For a more practical example, let's check out a base32 encoder that uses the set of standard ASCII characters [a-z0-9] except for the letters i, l, o, and s (for readability). Encode a string with extended ASCII characters, such as "Écrire, c'est une façon de parler sans être interrompu.", and we'll get a string like:

t5hq4ubjcmp20rt7cntq883ndtjj0tk1wxqpw834cmg70rbjdhjq483kc5q7687aeht6a839dtu6awkjdxpq0x9e

Decode it and we should get back the original result.


Generating our own codes

Hopefully it's becoming apparent that creating our own 2FA codes isn't terribly complicated. The important part is generating a good random string for each user (which we'll store), and being able to base32 encode it.

Looking at those 4 basic fields again, here's what we'll use:

 * Type: "totp"
 * Issuer: A product name, like "Acme"
 * Label: The format "Product:Account Name", like "Acme:jdoe@gmail.com"
 * Secret: A random string (Google calls it an "arbitrary key value"), base32 encoded so that users who can't scan the QR code can still type the secret in manually

otpauth://totp/${encodeURI(label)}?secret=${secret}&issuer=${encodeURI(issuer)};

Once we've got everything, we need a library that can convert it to a QR code. For JavaScript, check out the 2FA QR code generator which uses jQuery.qrcode. For C#, there's the QRCoder library. We should avoid reinventing the wheel where we can, and look for established libraries when possible.

A user scans the QR code, at which point their app and our system will both be storing the secret code. No matter what 2FA app they used to scan the code originally, it should be capable of combining the secret with the current time and generating a single-use code that's good for 30 seconds.


Authenticating users (verifying TOTP)

The very last part of the process is validating that the code is good. When someone enters a 2FA code from their app, we need to retrieve their secret from the database and generate the same code according to a certain algorithm.

You can try to implement that algorithm yourself (not recommended) or just look for a reliable library in the language of your choice like JavaScript or Otp.NET in C#. Since the time on a server versus a user's device may be slightly out of sync, it's possible they'll get a code that's different than the one we calculate, so it's a good idea to calculate several codes (one for the previous 30 seconds, and one for the next 30 seconds) and validate for all of them. It's also worth noting that if they're using a device that has the wrong time, their generated 2FA codes might never validate.

If you'd like to see an implementation written in C#, check this out.


A practical example in C#

I create an example app in C#. The label, issuer and secret will be prepopulated at startup, but feel free to change them. As you do, the QR code is regenerated.



The example used here is available on GitHub (compiled version), for you to use, expand upon, or just follow along while you read... and hopefully discover something new!



When you're ready, scan it with your phone to add it like any other 2FA code.

The numbers at the very bottom, in parentheses, represent the number of steps, or possible codes that could've been generated since the Unix epoch in 1970:

= seconds since unix epoch / time between codes, usually 30 seconds

One of the recommendations, as I mentioned previously, is to allow ±1 step to handle a case where the server and the user's device have slightly different times, or the user is a bit slow to enter the code and it changes in the meantime. That's why I display three codes (past, current, next).

During the elapsed time between the two screenshots, you can see a new "Current Code" has been generated, and the previous "Current Code" is now the current "Previous Code". lol
