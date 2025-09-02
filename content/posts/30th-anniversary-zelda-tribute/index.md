---
categories:
  - Retro
date: 2020-09-09T16:28:52Z
description: ""
draft: false
cover:
  image:
slug: 30th-anniversary-zelda-tribute
summary: A few years ago, on the 30th anniversary of the Legend of Zelda, Scott Lininger and Mike Magee open sourced a 3D version of the original LoZ. The site was taken down, but the code's still available to run!
tags:
  - retro
  - nodejs
title: A 30th anniversary Zelda tribute, in Node.js
---
Nearly 30 years ago, I remember being at one family thing or another, when my cousin brought out his new SNES game console and hooked it up to a little color TV. He popped in a game that I immediately fell in love with (hey I was like 12), although I had no idea who or what Zelda was.

Some little green-clad forest dude gets a message in a dream, wakes up to find his uncle slipping off in the middle of a stormy night (note to self, grab a lantern before climbing down a well), and within 30 seconds the player is off exploring. Right from the start, it had me, and I'll bet I've played it through 20 times over the years.

![](Dream.webp)

Before Link to the Past though, there was the original Legend of Zelda for the NES. I never got into that one quite as much, but I definitely played it through a few times. I haven't played either of them in a long time though, which is why I geeked out on a project I stumbled onto the other day - [an opensource 3D version of the original Legend of Zelda](https://github.com/scottlininger/zelda30tribute) that runs right in your browser!

A few years ago, on the 30th anniversary of the LoZ, Scott Lininger and Mike Magee released it on a website that, well.. got [smacked down by Nintendo](https://www.facebook.com/zelda30tribute/posts/485743838275370) pretty quickly. The author said he had no hard feelings - clearly he expected it, since Nintendo is known for [aggressively protecting its IP](https://www.polygon.com/2016/9/2/12770344/nintendo-slaps-metroid-2-remake-and-500-plus-fangames-with-takedown-orders/).

But, although they issue DMCA requests to take sites down all the time, they don't seem to pursue the hosted source code. Or didn't in this case... I don't know how all that legal nonsense works. It's good for you and me though, because we can still try out the fan-made 30th anniversary homage to Zelda. It's not even close to being finished, and hasn't been touched in years, but the potential was pretty epic.

## Play it local

To run it on your own PC, just [install NodeJS](https://nodejs.org/en/download/) (I got a warning from Windows Defender to allow it, the first time I fired up the game) and execute the following script from wherever you want to download and play the game. It clones the repo, removes one line that causes an error, and starts it up.

```bash
#/bin/bash

git clone "https://github.com/scottlininger/zelda30tribute"
cd "zelda30tribute"
sed -i "s/sys\.puts//" "./nodeserver.js"
start "http://localhost:9378/www/index.html"
node nodeserver.js
```

You should get some output like this:

```none
$ ./runzeldarun.sh
Cloning into 'zelda30tribute'...
remote: Enumerating objects: 846, done.
remote: Total 846 (delta 0), reused 0 (delta 0), pack-reused 846
Receiving objects: 100% (846/846), 73.36 MiB | 13.05 MiB/s, done.
Resolving deltas: 100% (259/259), done.
Updating files: 100% (802/802), done.
(node:2268) [DEP0025] DeprecationWarning: sys is deprecated. Use util instead.
```

And then... \<insert the sounds of angelic choirs here\>

![](30th-anniversary-zelda-tribute/image.webp)

## Play it remote

If you don't want to install NodeJS locally, you can host it remotely on DigitalOcean. Word of warning though - don't post the IP address somewhere public or assign it a domain name. Then people will find it, or you'll be tempted to share it, and Nintendo will send Ganon after you.

If you don't already have an account, [sign up](https://m.do.co/c/448f25462030) for one. They provide one-click servers (droplets), and there's a [NodeJS droplet](https://do.co/2PQEqgd) that'll work perfectly. Just click the blue "Create NodeJS Droplet" button and choose the most minimal server config available. After a few minutes, you should be able to open the IP address in your browser and see the sample NodeJS app running.

![](30th-anniversary-zelda-tribute/image-1.webp)

![](2020-09-05-23_05_43-Create-Droplets---DigitalOcean---Brave.webp)

![](2020-09-05-23_19_39-Your-NodeJS-Droplet---Brave.webp)

Once it's up and running, use your favorite terminal tool like [Git Bash](https://git-scm.com/downloads) to connect to your new VM. Type in `ssh root@your-ip` to connect, and then type in the "root" password you set during the setup process. If you get a prompt asking if you're sure you want to continue, I'd suggest typing yes or the rest of this is going to be really boring.

![](30th-anniversary-zelda-tribute/image-2.webp)

You can run the same file as for the local version, with two changes - opening the port in UFW and _not_ opening the site. And don't forget to `chmod +x your-file.sh` so you can execute the script.

```bash
#/bin/bash

git clone "https://github.com/scottlininger/zelda30tribute"
cd "zelda30tribute"
sed -i "s/sys\.puts//" "./nodeserver.js"
ufw allow 9378
node nodeserver.js
```

Go to http://161.35.118.133:9378/www/index.html (with _your_ IP address obviously) and try it out!

## Ah, ah, ah, ah, stayin' alive

For all the game's awesomitude (real word), it's got a few bugs and glitches. Hit boxes on enemies are a little off, and it seems to be far easier for them to hit you. I suggest going into `www/js/game.js` and making a little tweak around line 73 to increase your hit points to 5000. That's a few more hearts than you could find on the NES, but seems like a fair handicap.

```bash
  /*
   * The avatar.
   */
  this.avatar = new ace.Avatar(this);
  this.avatar.hitPoints = 5000;
```

If you want to have a little more fun, modify the state a little further down. You can add most of the original items to your inventory, but unfortunately nothing really works besides the sword, bombs and raft. The whistle works, if all you're looking to do is make a whistle noise.

```javascript
// The current "save game" state, with everything important we've done.
this.state = {inventory: {
                 bow: 1,
                 itemwoodensword: 1,
                 raft: 1,
                 whistle: 1,
                 bombs: 1,
                 candle: 1,
                 boomerang: 1,
                 potion: 1,
                 ladder: 1,
                 ring: 1,
               },
               coins: 500,
               bombs: 5000,
               keys: 500,
               canvasScaleX: 1,
               canvasScaleY: 1,
               hasCompassByDungeon: {},
               hasMapByDungeon: {},
               hasVisitedRoomByDungeon: {},
               maxHitPoints: 3
             };
```

Armed with invincibility, have a look around. They put in a ton of work into getting the basic layout of overworld in place and navigable. Most of the overworld enemies are present, though they only drop rupees.. ironic, since all items in the few working caves are free.

![](2020-09-06-01_59_42-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-07-12_19_11-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-07-12_19_35-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-07-12_25_31-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-07-12_25_59-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-07-12_44_59-Zelda-30-Year-Tribute---Brave.webp)

You can even go through the first two dungeons and beat Aquamentus and Dodongo, although certain enemies and areas aren't really complete.

![](2020-09-07-12_36_45-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-07-12_45_30-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-07-12_46_33--1.webp)

![](2020-09-09-08_57_59-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-08-00_57_09--1.webp)

![](2020-09-08-00_59_23-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-09-01_03_35-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-09-09_01_23-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-08-00_58_33-Zelda-30-Year-Tribute---Brave.webp)

Things fall apart a little in the second dungeon, with "flat" enemies painted onto the floor in place, rooms that are pitch black, dead ends with "demo" messages, etc.

![](2020-09-07-12_26_39-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-07-12_33_53-.webp)

![](2020-09-07-12_33_07-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-07-12_26_21-Zelda-30-Year-Tribute---Brave.webp)

![](2020-09-06-02_33_15-Zelda-30-Year-Tribute.webp)

All in all, I _so_ wish this had been worked on and was more complete. The concept is one that any classic Legend of Zelda fan could appreciate, and they obviously put a ton of work into getting it as far as they did. I guess once Nintendo came knocking, most of the fun was gone, it's open sourced so maybe someone (you?) will pick it up and finish it!

![](2020-09-09-09_02_34-.webp)
