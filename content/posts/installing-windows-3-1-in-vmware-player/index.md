---
categories:
- Retro
- Virtualization
date: "2013-10-24T22:14:08Z"
description: ""
draft: false
cover:
  image: photo-1487180144351-b8472da7d491.jpg
  relative: true
slug: installing-windows-3-1-in-vmware-player
summary: While looking for a copy of Windows 98 on MSDN to install some old software
  (compatibility mode under Windows 7 didn’t work), I came across Windows 3.11. Installing
  it was a little tricky though...
tags:
- Retro
- Virtualization
title: Installing Windows 3.1 in VMware Player
---
Every once in awhile it’s interesting in a nerdy way to check out some legacy technology and… I dunno… reminisce about the old days and how far we’ve come or some crap like that. Like the fact that the pen drive in my pocket is 10x bigger than the largest hard drives 15 years ago.

I decided this week to install Windows 3.1 on a virtual machine. First, [you’ll need to install DOS](https://grantwinney.com/installing-dos-6-22-in-vmware-player/). When that’s done, power off the virtual machine.

## Requirements

- [VMware Player](http://www.vmware.com/products/player/)
- [An instance of DOS 6.22 running in a virtual machine](https://grantwinney.com/installing-dos-6-22-in-vmware-player/)
- Software capable of packaging files into a floppy disk image (may end in FLV, IMA, etc), such as [WinImage](http://www.winimage.com/download.htm)_(30 day free trial)_
- Software capable of packaging files into an cd (ISO) image, such as [Folder2Iso](http://www.trustfm.net/divx/SoftwareFolder2Iso.php)_(freeware)_
- A driver to enable CD-ROM capabilities, such as [oakcdrom.sys](http://www.computerhope.com/download/hardware.htm). _(a generic CD-ROM driver that will work with the majority of all IDE CD-ROM drives)_
- A Windows 3.1 image. If you have an MSDN account, you can download a zip file with the installation files inside it, package them into an ISO image using [Folder2Iso](http://www.trustfm.net/software/utilities/Folder2Iso.php) and mount the image.

## Installing Windows

DOS doesn’t support CDs right out of the box, so we’ll need a driver. I used oakcdrom.sys, a generic CD-ROM driver. At this point, all we’ve got access to is a floppy drive.

Install [WinImage](http://www.winimage.com/winimage.htm) and package the driver into a floppy disk image.

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-001.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-002.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-003.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-004.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-005.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-006.png)

Mount the image to your virtual machine and copy the driver into `c:\dos`.

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-007.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-009.png)

You’ll need to adjust two configuration files to use the new driver:

Type `edit c:\config.sys` and add to the end of that file:

```
DeviceHigh=C:\DOS\oakcdrom.sys /D:CD1
```

And then add to the end of `C:\AUTOEXEC.BAT`:

```
LH MSCDEx /D:CD1
```

These drivers extend BIOS and DOS, respectively, to support the CD-ROM drive, and Windows 3.1 inherits that when it runs. _(credit:_ [_tomshardware.com_](http://www.tomshardware.com/forum/173467-48-error-windows-install#8953171)_)_

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-011.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-012.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-013.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-014.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-015.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-016.png)

Reboot the virtual machine to apply your changes and then mount the Windows 3.1 disk.

- If you’ve already got floppy disk or cd-rom images of Windows, then mount those now.
- If you’ve got a zip file with the installation files inside it, like the one I got from MSDN, then you’ll need to run an app (such as [Folder2Iso](http://www.trustfm.net/software/utilities/Folder2Iso.php)) that can package them into a CD ISO, which you can then mount. It’s a straight-forward process.

![Win 311 Image 010](https://grantwinney.com/content/images/2014/04/Win-311-Image-010.png)

With the CD-ROM now recognized and the Windows 3.1 disk mounted, you can boot up into DOS and type `D:` at the prompt to access the disk. Type `setup` to begin installation. I just accepted all the defaults.

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-017.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-018.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-019.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-020.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-021.png)

I opted for all the features, but at what cost? _Over_ _**2 MB**_ _of hard drive space!_ 😱

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-022.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-023.png)

Some changes to the same files we edited earlier. Windows saves a backup copy before modifying it.

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-024.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-027.png)

Wasn’t sure what to do with this. Considered skipping it, but ended up selecting the ‘generic’ printer. Maybe I’ll search for a driver that would allow me to print.

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-028.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-029.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-030.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-031.png)

I _had_ to go through the tutorial. Never too late to learn to use a mouse.

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-032.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-033.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-034.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-035.png)

Mmm… icecream. I’m a little dubious of how they calculate the number of calories in “chocolate sauce” and “nuts”. Weight-watchers app, this is not.

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-036.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-037.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-038.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-040.png)

## What's here?

Wow, notice the evolution of Notepad over 20 years. :p

![Win 311 Image 042](https://grantwinney.com/content/images/2014/04/Win-311-Image-042.png)

Something I threw together.

![Win 311 Image 043](https://grantwinney.com/content/images/2014/04/Win-311-Image-043.png)

Hmm.. what else can I install on here?

### Visual Basic 2.0

Here’s Visual Basic 2.0. Woah. Okay, that one’s improved in 20 years. Notice that a complete installation will require 18 MB! _(gasp)_

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-044.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-045.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-046.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-047.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-048.png)

### QBasic 4.5

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-049.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-050.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-051.png)

I suppose other than finding other apps to install, I’ll try to find a graphics driver, sound driver, etc. Or I’ll get bored after using it for 10 minutes because, come on, the real fun is in getting it to work. Who the heck wants to _use_ it for anything??

You can download this [graphics driver](https://sites.google.com/site/chitchatvmback/misc) and follow the instructions in the zip file; it gets you 256 colors and 1024×768 res in a VMWARE environment. You may also need [these drivers](http://www.sierrahelp.com/Patches-Updates/Patches-Updates-Misc/Win31SVGAUpdate.html).. not sure.

Installed IE too. Ouch. [Get it here.](http://www.oldapps.com/internet_explorer.php?old_internet_explorer=1?download)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-061.png)

![](https://grantwinney.com/content/images/2019/05/Win-311-Image-062.png)
