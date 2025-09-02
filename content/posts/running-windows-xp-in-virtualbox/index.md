---
categories:
  - Retro
date: 2019-05-04T14:35:40Z
description: ""
draft: false
cover:
  image:
slug: running-windows-xp-in-virtualbox
summary: Just got an MSDN account, which always comes with some old treasures (hey, beauty's in the eye of the beholder). Take a trip back with me, to the days of Windows XP, the beginning of the .NET Framework, and even further... ;)
tags:
  - virtualization
title: Running Windows XP in VirtualBox
---
I just got access to an MSDN account with keys for various versions of Windows and Visual Studio, so.... it's retro time! Isn't it funny how something brand new comes out and we get excited, then we get annoyed with it's deficiencies, then it's forgotten when something better comes out, and finally after enough time we get all nostalgic and pull it out of mothballs?

With that in mind, let's check out the best of Windows yesteryear. But first...

**Note:** I won't share any keys or recommend where to find them, but you were allowed to use XP for 60 days without activating, so you could probably use any key you find and you'll be good for a couple months. Sure, that's annoying, but you're not seriously using this for anything serious are you?

[You can't install service packs on 64-bit systems](http://web.archive.org/web/20080430165302/http://www.microsoft.com/windowsxp/sp2/sysreqs.mspx), so you may want to choose the 32-bit option. If you're doing this from MSDN, select the _"with Service Pack 3"_ option to make life easier.

![](winxp86.webp)

Give it plenty of hard disk space if you plan on installing other apps. It's a pita to [resize the partition](https://thegenomefactory.blogspot.com/2013/06/extending-windows-xp-partition-in.html) afterwards, so give it 30 or 40 GB at least. XP had really minimal requirements, so a couple gigs of memory should be more than enough.

![](vmsetup1.webp)

![](vmsetup2.webp)

![](vmsetup3.webp)

![](vmsetup4.webp)

![](vmsetup5.webp)

![](vmsetup6.webp)

![](vmsetup7.webp)

![](vmsetup8.webp)

## Map a Host Machine Folder

After installing XP, the first thing you'll notice is that Internet Explorer is awful and horrible and you probably can't even connect to the Internet (at least until you have the service packs installed). Let's do the very first thing anyone does when they install any version of Windows... install a real browser.

Follow the suggestions in [this thread](https://askubuntu.com/q/52773/927673) to share a folder with your host machine, or run _"Devices / Insert Guest Additions CD image..."_ in VirtualBox _(_[_read more here_](https://www.virtualbox.org/manual/ch04.html)_)_ and then go to _"Devices / Shared Folders / Shared Folders Settings"_ and map a folder on your host machine. You might have to select _"Devices / Optical Drives / Remove disk"_ first, but maybe not. I mapped the "Downloads" folder, so it'll show up as a drive in the VM under _My Computer_.

![](sharedfolders.webp)

![](winexplorer.webp)

Now you can download [Chrome](https://www.google.com/chrome/) (it'll install v49, the last supported version for XP), [Firefox 52.9.0esr](https://ftp.mozilla.org/pub/firefox/releases/52.9.0esr/win32/en-US/) (Mozilla's [last supported version](https://support.mozilla.org/en-US/kb/end-support-windows-xp-and-vista), but you'll likely have to install SP2 first), or [Opera 36](https://ftp.opera.com/pub/opera-winxpvista/36.0.2130.80/win/) (you get the picture) on the host machine, drop them in the shared folder, and install them from the VM.

![](xpbrowsers.webp)

---

## Installing Service Packs

XP is ancient but we might as well update it where we can. If you selected the _"with Service Pack 3"_ option like I mentioned, then skip this section.

Otherwise, there's a couple places you can get the SPs from (at least). The easiest is from your MSDN account. Save "Service Pack 1a", "Service Pack 2 (English)" and "Service Pack 3 (x86)" to the shared folder on your host machine.

![](msdn-xp-sps.webp)

The files are ISOs, so download [Virtual CloneDrive](https://www.elby.ch/en/products/vcd.html) in the VM _(you might have to right-click the exe, open properties, and press "unblock")_ and mount each file to install it. All you have to do is choose Settings and create a half-dozen virtual drives, then mount each one to an ISO on the host machine.

![](clonedrive1.webp)

![](clonedrive2.webp)

Alternatively, you can grab them from the [Microsoft Update Catalog](http://www.catalog.update.microsoft.com):

- [Windows XP Service Pack 1, Express](http://www.catalog.update.microsoft.com/ScopedViewInline.aspx?updateid=4fcf5feb-70cf-419f-99c3-d75269e76ce7) ([download](http://www.download.windowsupdate.com/msdownload/update/v3-19990518/cabpool/sp1aexpress_5d7ed5146e86a5e10e309048d02744efe5aba1d8.exe)) - 5/15/2004
- [Windows XP Service Pack 2](http://www.catalog.update.microsoft.com/ScopedViewInline.aspx?updateid=7477af62-8f9b-4f32-9daf-7ab452e52396) ([download](http://www.download.windowsupdate.com/msdownload/update/v3-19990518/cabpool/xpsp2_33a8fef60d48ae1f2c4feea27111af5ceca3c4f6.exe)) - 9/13/2005
- [Windows XP Pro Service Pack 2, x64 Edition](http://www.catalog.update.microsoft.com/ScopedViewInline.aspx?updateid=e23c92ac-448c-45c2-8bd2-aa8021456e00) ([download](http://www.download.windowsupdate.com/msdownload/update/v3-19990518/cabpool/windowsserver2003.windowsxp-kb914961-sp2-x64-enu_7f8e909c52d23ac8b5dbfd73f1f12d3ee0fe794c.exe)) - 5/23/2008
- [Windows XP Service Pack 3](http://www.catalog.update.microsoft.com/ScopedViewInline.aspx?updateid=60b990a0-6efa-47be-8f5a-7df2c402583e) ([download](http://www.download.windowsupdate.com/msdownload/update/software/dflt/2008/04/windowsxp-kb936929-sp3-x86-enu_c81472f7eeea2eca421e116cd4c03e2300ebfde4.exe)) - 5/19/2009

## Install Updates

Although [support for XP ended 5 years ago](https://web.archive.org/web/20170217202701/https://www.microsoft.com/en-us/windowsforbusiness/end-of-xp-support), you can still grab the updates that were available. Click on _Start / All Programs / Windows Update_ and select all the updates. Install, reboot, select more updates, rinse, repeat. Good ol' XP.

![](winupdate1.webp)

![](winupdate2.webp)

![](winupdate3.webp)

![](winupdate4.webp)

![](winupdate5.webp)

![](winupdate6.webp)

## Setup a Development Environment

Now that XP has the latest updates and security patches of yesteryear, we can setup a development environment. Gotta have git. The [last version of Git for Windows supported on XP was 2.10.0](https://github.com/git-for-windows/git/releases/tag/v2.10.0.windows.1), so download the exe and install it so you can clone some repos!!

### Visual Studio 2012

Thought this would be a good one to start with. I was wrong. [VS2012 no likey XP](https://www.techulator.com/resources/7422-Visual-Studio-2012-System-requirements-hardware-requirements.aspx).

![](2019-05-02-16_42_24-InstallVS2012-1.webp)

![](2019-05-02-16_42_24-InstallVS2012-2.webp)

![](2019-05-02-16_42_24-InstallVS2012-3.webp)

### Visual Studio 2010

You can [download the free Visual Studio 2010 Express](https://visualstudio.microsoft.com/vs/older-downloads/), or the pro version from MSDN. Surprisingly, the look and feel were pretty similar to what we've got a decade later. The team hadn't yet decided to [CAPITALIZE ALL THE MENUS](https://stackoverflow.com/q/10859173/301857). NuGet integration wasn't built-in, but I'm sure a quick search would turn up someone figuring out how to do it.

![](2019-05-02-16_52_16-Microsoft-Visual-Studio-2010-Setup-1.webp)

![](2019-05-02-16_52_16-Microsoft-Visual-Studio-2010-Setup-2.webp)

![](2019-05-02-16_52_16-Microsoft-Visual-Studio-2010-Setup-3.webp)

![](2019-05-02-16_52_16-Microsoft-Visual-Studio-2010-Setup-4.webp)

![](2019-05-02-16_52_16-Microsoft-Visual-Studio-2010-Setup-5.webp)

![](2019-05-02-16_52_16-Microsoft-Visual-Studio-2010-Setup-6.webp)

![](2019-05-02-16_52_16-Microsoft-Visual-Studio-2010-Setup-7.webp)

### Visual Studio .NET 2003

Not retro enough for you? Let's go back another few years and try out [VS .NET 2003](https://en.wikipedia.org/wiki/Microsoft_Visual_Studio#.NET_2003). It introduced .NET 1.1, so sadly no generics and _definitely_ no LINQ yet. Boooo. If it complains about prereq's but you don't have a prereq CD, try installing the Windows Components it's complaining about.

![](2019-05-03-09_50_37-Visual-Basic-.NET-Setup-1.webp)

![](2019-05-03-09_50_37-Visual-Basic-.NET-Setup-2.webp)

After that, I ran Windows Update (yet again), tried to install [.NET 1.1. SP1](https://www.microsoft.com/en-us/download/details.aspx?id=33) (although it said it was already installed), rebooted, and finally ran the `setup.exe` file with a special flag that you can see in the command window.

```none
F:\>./setup/setup.exe /NO_BSLN_CHECK
```

Not sure which part of all that fixed it _(or was it a combination of everything?)_ but I got to a normal installation screen eventually. And here's the best WinForms had to offer:

![](2019-05-03-09_50_37-Visual-Basic-.NET-Setup-4.webp)

![](2019-05-03-09_50_37-Visual-Basic-.NET-Setup-5.webp)

![](2019-05-03-09_50_37-Visual-Basic-.NET-Setup-6.webp)

![](2019-05-03-09_50_37-Visual-Basic-.NET-Setup-8.webp)

![](2019-05-03-09_50_37-Visual-Basic-.NET-Setup-9.webp)

![](2019-05-03-09_50_37-Visual-Basic-.NET-Setup-10.webp)

![](2019-05-03-09_50_37-Visual-Basic-.NET-Setup-11.webp)

### Visual Basic 6.0

_Still_ not retro enough?? Let's go back a couple decades to [VB 6.0](https://en.wikipedia.org/wiki/Microsoft_Visual_Studio#6.0_.281998.29). Forget everything you thought you knew... this is before the .NET Framework altogether.

![](2019-05-01-09_15_54-Visua![](2019-05-01-09_15_54-Visual-Basic-6.0-Enterprise-Setup.webp)ual-Basic-6.0-Enterprise-Setup.png)

![](2019-05-02-15_20_21-Installation-Wizard-for-Visual-Basic-6.0-Enterprise-Edition.webp)

![](2019-05-02-15_22_21-Installation-Wizard-for-Visual-Basic-6.0-Enterprise-Edition.webp)

![](2019-05-02-15_44_13-Microsoft-Visual-Basic.webp)

Wow, when did we lose this?! There was a wizard to create an ultra modern app, complete with splash screens, toolbars, "about" screens..... _built-in interweb browsers!!_ I'm not sure why we ever progressed beyond this.

![](2019-05-04-08_20_01-Microsoft-Visual-Basic--design-.webp)

![](2019-05-04-08_20_33-Application-Wizard---Menus.webp)

![](2019-05-04-08_22_03-Microsoft-Visual-Basic--design-.webp)

![](2019-05-04-08_22_34-Application-Wizard---Standard-Forms.webp)

![](2019-05-04-08_29_45-Project1---Microsoft-Visual-Basic--design-.webp)

![](2019-05-04-08_33_24-Project1---Microsoft-Visual-Basic--design-.webp)

![](2019-05-04-08_38_03-Project1.webp)

### Visual Basic 4.0

Older! More olderer!! Okay, here's VB 4.0 from 25 years ago. Look at that GUI.. every component is a separate floating toolbar.. thing. And the humble beginning of _(gag)_ Crystal Reports. 🤢 🤮

![](2019-05-04-20_35_07-Visual-Basic-4.0-Master-Setup-01.webp)

![](2019-05-04-20_35_07-Visual-Basic-4.0-Master-Setup-02.webp)

![](2019-05-04-20_35_07-Visual-Basic-4.0-Master-Setup-03.webp)

![](2019-05-04-20_35_07-Visual-Basic-4.0-Master-Setup-04.webp)

![](2019-05-04-20_35_07-Visual-Basic-4.0-Master-Setup-05.webp)

![](2019-05-04-20_35_07-Visual-Basic-4.0-Master-Setup-06.webp)

![](2019-05-05-00_24_48-.webp)

![](2019-05-05-00_26_22-Crystal-Reports-for-Visual-Basic.webp)

![](2019-05-05-00_27_46-Crystal-Reports-for-Visual-Basic----Untitled-Report--1-.webp)

### Must. Go. OLDER!!

25 years isn't retro enough?!? You're relentless.

Check out my post from a few years back on [installing Windows 3.1](https://grantwinney.com/installing-windows-3-1-in-vmware-player/). Get a sneak peek at Visual Basic 2.0, QBasic, and all manners of 16-bit awesomeness. ;p
