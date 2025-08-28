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
I've been using 2FA on [every site that supports it](https://twofactorauth.org/) for quite some time, but I've never given much thought to how a 2FA code is created. I enable it, scan the QR code, and print the backup codes. The rest is magic. 🧙‍♂️

But no more! Today is the day we figure out how to generate a 2FA code...

## A few basics...

If you've used 2FA and know what TOTP is, skip this section. For everyone else...

### What is 2FA?

For the totally uninitiated it just means "two factor authentication", or a second way to authenticate yourself, often by generating a random code using an app on your phone. More generically, it means proving who you are by providing a combination of something you __have,__ something you __know,__ something you __are,__ etc.

Even if you don't think you've ever used 2FA before, if you have...

- Ever inserted an ATM card (have) __and__ entered a PIN (know)?
- Or inserted a credit card (have) __and__ entered a zip code (know)?
- Presented your insurance card (have) __and__ verified your birth date (know)?
- Shown your top-secret government id (have) __and__ submitted to a retinal scan (are)?

This stuff used to apply to relatively few things though - a bank account and a few credit cards. Now we have dozens (or hundreds) of logins, each of which secures a site that holds some of our private data, and the more ways you have to prove who you are, the less likely someone can pretend to be you.

There's [a lot of ways to do 2FA](https://auth0.com/learn/two-factor-authentication/), but some are more common than others...

- Enter a password (know), and answer canned questions (know). __(awful - check out how many__ [__known data breaches__](https://haveibeenpwned.com/PwnedWebsites) __included security questions and answers, which most people reuse)__
- Enter a password (know), and answer custom questions (know). __(meh)__
- Enter a pw (know), then a code that's texted to your phone (have). __(__[__not the best__](https://www.wired.com/2016/06/hey-stop-using-texts-two-factor-authentication/)__)__
- Enter a pw (know), then a randomly generated, one-time code from your phone that expires in 30 seconds (have), aka TOTP.

### What is TOTP?

A time-based one-time password (TOTP) is just one way to do 2FA... but it's very common. A TOTP only works for a certain period of time (usually 30 seconds), and it only works once (so trying to log in twice with the same password should fail).

### What's it look like?

A website generates a QR code to scan with an app like [andOTP](https://github.com/andOTP/andOTP), [Microsoft Authenticator](https://support.microsoft.com/en-us/account-billing/download-microsoft-authenticator-351498fc-850a-45da-b7b6-27e523b8702a), [1Password](https://support.1password.com/one-time-passwords/), etc. The QR code represents a URI with a few pieces of data, including a random string that's unique to you, which is encoded in base32 __(more on that later)__. From then on, the app generates a new random code every 30 seconds, which you use at login. And in case your [phone catches fire](https://time.com/4485396/samsung-note-7-battery-fire-why/), they provide some recovery codes to print and save.

![](https://grantwinney.com/content/images/2019/07/QR-code.png)

![](https://grantwinney.com/content/images/2019/07/Backup-Codes-.png)

If someone guesses your password, or hacks your email and requests a password recovery, they still can't login unless they also have access to your 2FA app (which usually means having physical access to your phone as well).

---

## How are they generated?

From the user's perspective, it starts with a QR code that represents a URI, as I mentioned above. Using a service like [ZXing Decoder](https://zxing.org/w/decode.jspx) on one of those QR codes, we see it holds a few pieces of data, as outlined here: [Key URI Format](https://github.com/google/google-authenticator/wiki/Key-Uri-Format)

![](https://grantwinney.com/content/images/2019/05/qr-code-decrypted.png)

### Dissecting a QR code

Let's take a minute to break that __"Raw text"__ line down:

- Type: The "totp" indicates this is a time-based one-time code.
- Label: The "csinfotest" is a label used to identify the code in the authenticator app.
- Issuer: The "Namecheap - ...." param is also a label. __(__[__because reasons__](https://github.com/google/google-authenticator/wiki/Key-Uri-Format#issuer)__...)__
- Secret: A base32 encoded key __(more on that in a moment)__ that's unique for each user, and used in conjunction with the current time to generate the TOTP code.

__"But",__ you might say, __"that Key URI Format webpage mentions other fields too!"__

Yep, there are optional parameters; but when I tested a code that used them, the results were inconsistent. Google's and Microsoft's apps ignored the extra params, while andOTP and LastPass show an 8 digit number for 15 seconds, but not the __same__ number. 🤔

For now, I'd suggest just setting the 4 basic fields, to generate a code that'll work everywhere.

### What is base32 encoding?

Of the four pieces of data mentioned above, the one that makes this whole thing work is the base32 "secret" string, so let's review what base32 encoding / decoding is.

Basically, it means converting a string that could have any data in it to a string that's a set of 32 characters. The exact set of 32 characters can vary depending on the use case, but they're often selected for specific reasons, such as:

- a legacy system that can't handle certain characters,
- the need to be easily human readable (so not using `O` and `0`, `l` and `1`, etc),
- use as a file name (so not `\` or `/`)

If you're having trouble sleeping, check out [RFC 4648](https://tools.ietf.org/html/rfc4648#section-3.3) concerning base16, base32, and base64, particularly section 3.4 on choosing the alphabet. It's riveting.

For a more practical example, let's check out a [base32 encoder](https://www.browserling.com/tools/base32-encode) that uses the set of standard ASCII characters `[a-z0-9]` except for the letters `i`, `l`, `o`, and `s` (for readability). [Encode a string](https://www.browserling.com/tools/base32-encode) with __extended__ ASCII characters, such as __"Écrire, c'est une façon de parler sans être interrompu."__, and we'll get a string like:

```none
t5hq4ubjcmp20rt7cntq883ndtjj0tk1wxqpw834cmg70rbjdhjq483kc5q7687aeht6a839dtu6awkjdxpq0x9e
```

[Decode it](https://www.browserling.com/tools/base32-decode) and we should get back the original result.

### Generating our own codes

Hopefully it's becoming apparent that creating our own 2FA codes isn't terribly complicated. The important part is generating a good random string for each user (which we'll store), and being able to base32 encode it.

Looking at those 4 basic fields again, here's what we'll use:

- Type: "totp"
- Issuer: A product name, like "Acme"
- Label: The format "Product:Account Name", like "Acme:jdoe@gmail.com"
- Secret: A random string __(Google calls it an "arbitrary key value"),__ base32 encoded so that users who can't scan the QR code can still type the secret in manually

```none
otpauth://totp/${encodeURI(label)}?secret=${secret}&issuer=${encodeURI(issuer)};
```

Once we've got everything, we need a library that can convert it to a QR code. For JavaScript, check out the [2FA QR code generator](https://stefansundin.github.io/2fa-qr/) which uses [jQuery.qrcode](https://larsjung.de/jquery-qrcode/). For C#, there's the [QRCoder](https://www.nuget.org/packages/QRCoder/) library. We should avoid reinventing the wheel where we can, and look for established libraries when possible.

A user scans the QR code, at which point their app and our system will both be storing the secret code. No matter what 2FA app they used to scan the code originally, it should be capable of combining the secret with the current time and generating a single-use code that's good for 30 seconds.

### Authenticating users (verifying TOTP)

The very last part of the process is validating that the code is good. When someone enters a 2FA code from their app, we need to retrieve their secret from the database and generate the same code according to a certain algorithm.

You can try to [implement that algorithm](https://en.wikipedia.org/wiki/Time-based_One-time_Password_algorithm#Algorithm) yourself (not recommended) or just look for a reliable library in the language of your choice like [JavaScript](https://jsfiddle.net/russau/ch8PK/) or [Otp.NET in C#](https://github.com/kspearrin/Otp.NET). Since the time on a server versus a user's device may be slightly out of sync, it's possible they'll get a code that's different than the one we calculate, so it's a good idea to calculate several codes (one for the __previous__ 30 seconds, and one for the __next__ 30 seconds) and validate for all of them. It's also worth noting that if they're using a device that has the wrong time, their generated 2FA codes might never validate.

If you'd like to see an implementation written in C#, [check this out](https://grantwinney.com/a-sample-csharp-app-for-generating-and-verifying-totp-2fa-codes/).

---

## A practical example in C#

I create an example app in C#. The label, issuer and secret will be prepopulated at startup, but feel free to change them. As you do, the QR code is regenerated.

When you're ready, scan it with your phone to add it like any other 2FA code.

![](https://grantwinney.com/content/images/2019/08/phonescanqrcode.jpg)

![](https://grantwinney.com/content/images/2019/08/phoneqrcodeadded.jpg)

![](https://grantwinney.com/content/images/2019/08/codevalid.png)

![](https://grantwinney.com/content/images/2019/08/codeinvalid.png)

Enter the code from your phone into the bottom field (left image) to verify it's valid; enter an invalid TOTP code (right image) and it tells you.

The numbers at the very bottom, in parentheses, represent the number of steps, or possible codes that could've been generated since the Unix epoch in 1970:

```none
= seconds since unix epoch / time between codes, usually 30 seconds
```

One of the recommendations, as I mentioned previously, is to allow ±1 step to handle a case where the server and the user's device have slightly different times, or the user is a bit slow to enter the code and it changes in the meantime. That's why I display three codes __(past, current, next).__

During the elapsed time between the two screenshots, you can see a new "Current Code" has been generated, and the previous "Current Code" is now the current "Previous Code". lol

![](https://grantwinney.com/content/images/2024/09/image-2.png)