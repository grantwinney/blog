---
categories:
- API
date: "2018-02-06T12:36:34Z"
description: ""
draft: false
cover:
  image: photo-1518688248740-7c31f1a945c4.jpg
slug: passwordrandom-api
summary: The PasswordRandom API provides random values - and not just passwords as
  the name would seem to suggest. It also generates GUIDs, random numbers, characters,
  etc.
tags:
- API
title: Generate random passwords, numbers and GUIDs with the PasswordRandom API
---
Some of the API's I've written about give you access to data - public data like [ISS sightings](https://grantwinney.com/day-11-iss-notify-api/) and [amazing space photos](https://grantwinney.com/day-9-hubblesite-api/), and personal data like your [Instagram photos](https://grantwinney.com/day-2-instagram-api/) and [Dropbox files](https://grantwinney.com/day-4-dropbox-api/) - but an API can return other things too. Here's an API that returns random numbers, GUIDs and other values, and provides an opportunity to customize what it returns.

Two things before you get started:

- If you're unfamiliar with APIs, you might want to [read this first](https://grantwinney.com/what-is-an-api/) to familiarize yourself with them.
- You may want to install [Postman](https://www.getpostman.com/), which allows you to access API endpoints without having to write an app, as well as save the calls you make and sync them online between your computers.

## What does it provide?

The [PasswordRandom API](http://www.passwordrandom.com/api) provides random values - and not __just__ passwords as the name would seem to suggest. It also generates GUIDs, random numbers, characters, etc. Better yet, it has parameters for the __number__ of results you get back (like 10 GUIDs at once), and other ways to configure or limit the numbers and passwords you get back too.

If you're developing an app that will, for whatever reason, generate a GUID or return such a value to the user, you have a choice. Some languages have built-in libraries that return a GUID for you. Others require downloading a third-party and taking on another dependency. If you didn't want that dependency for some reason, you could call this API instead. As for everything else it offers.. well, you may find a reason to use it, so let's check it out.

## Taking it out for a spin

This API doesn't require any authentication, nor does it have documented rate limits. You can request the format (plain, xml, json) and other filters too.

### GUIDs

You can specify the number of globally unique identifiers (GUIDs) you'd like back, as well as the format in which to return them.

`http://www.passwordrandom.com/query?command=guid&count=5&format=json`

And that returns 5 GUIDs in JSON format, like this:

```json
{
    "char": [
        "f81660fd-6fb2-4172-81cc-7688ebb6c586",
        "f67ff2cf-a60b-4d7b-9d21-25d28fd1628c",
        "c7e019eb-dd7f-4660-9674-1109c39d394b",
        "e339fc3c-4995-479b-8715-4c25837ec5cf",
        "0700d1f6-4b40-459e-9d8f-1b5a34265421"
    ]
}
```

### Numbers

You can also get random integers...

`http://www.passwordrandom.com/query?command=int&count=12&format=xml&min=-12&max=18`

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<random>
    <result>-3</result>
    <result>7</result>
    <result>13</result>
    <result>-12</result>
    <result>-11</result>
    <result>2</result>
    <result>-1</result>
    <result>6</result>
    <result>-10</result>
    <result>6</result>
    <result>8</result>
    <result>7</result>
</random>
```

... as well as random floats.

`http://www.passwordrandom.com/query?command=double&count=12&format=xml&min=-12&max=18`

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<random>
    <result>2.8868</result>
    <result>9.0036</result>
    <result>-3.4164</result>
    <result>9.5505</result>
    <result>-0.0881</result>
    <result>-10.8226</result>
    <result>17.7975</result>
    <result>4.9651</result>
    <result>1.9549</result>
    <result>2.0954</result>
    <result>-4.7204</result>
    <result>-6.0978</result>
</random>
```

Strangely, getting random floats with a range only works if the range is integers. If you try to specify a max of 18.2, it returns values all over the place... even outside the default range of 0 to 100.

`http://www.passwordrandom.com/query?command=double&count=6&format=xml&min=0.0&max=18.0`

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<random>
    <result>136.7259</result>
    <result>135.1663</result>
    <result>36.1727</result>
    <result>25.7964</result>
    <result>80.7496</result>
    <result>112.1617</result>
</random>
```

### Passwords

A note about generating passwords using an API like this. I probably wouldn't recommend it for __passwords,__ but it could be useful for generating randomized strings. If you did use it for passwords, it's not like the site serving the API could know what account you'll use it for, but then it'd help if it were at least served over SSL so no one watching the network could sniff it...

Here's 8 random passwords:

`http://www.passwordrandom.com/query?command=password&count=8&format=json`

```json
{
    "char": [
        "RiuXA6.44ld",
        "SyePA8)31qr",
        "RyiHY8%55ss",
        "MiuVE3#22vr",
        "QuyNO6|26gx",
        "BuuPY7`29vl",
        "ZouTE5~18jv",
        "ZeoJI2[03bm"
    ]
}
```

If you click on the [scheme](http://www.passwordrandom.com/pronounceable-password-generator) link next to "password" field, you'll find a lot more options to generate the __exact__ string you need.

For example, if I needed a password for a system that required a length of 8, and at least one upper, one number, and one special symbol, I could request:

`http://www.passwordrandom.com/query?command=password&count=3&scheme=LrrrNrr!`

```none
BFal0is!
TOGY3Xa+
LnqD8Ae#
```

Or if you just wanted to generate 10 random phone numbers for some mockup:

`http://www.passwordrandom.com/query?command=password&count=10&scheme=%2Bn(nnn)nnn-nnnn`

```none
+1(597)499-5436
+8(080)704-9577
+6(916)072-4624
+3(418)217-7998
+8(470)258-2650
+4(995)881-2964
+3(648)883-2888
+8(178)047-6459
+3(029)629-6601
+3(016)479-8471
```