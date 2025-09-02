---
categories:
  - Retro
date: 2013-10-24T22:14:08Z
description: ""
draft: false
cover:
  image:
slug: installing-windows-3-1-in-vmware-player
summary: While looking for a copy of Windows 98 on MSDN to install some old software (compatibility mode under Windows 7 didn’t work), I came across Windows 3.11. Installing it was a little tricky though...
tags:
  - virtualization
title: Installing Windows 3.1 in VMware Player
---
Every once in awhile it’s interesting in a nerdy way to check out some legacy technology and… I dunno… reminisce about the old days and how far we’ve come or some crap like that. Like the fact that the pen drive in my pocket is 10x bigger than the largest hard drives 15 years ago.

I decided this week to install Windows 3.1 on a virtual machine. First, [you’ll need to install DOS](https://grantwinney.com/installing-dos-6-22-in-vmware-player/). When that’s done, power off the virtual machine.

## Requirements

- ~~VMware Player~~ I _think_ this was replaced by [VMware Fusion and Workstation](https://www.vmware.com/products/desktop-hypervisor/workstation-and-fusion)
- [An instance of DOS 6.22 running in a virtual machine](https://grantwinney.com/installing-dos-6-22-in-vmware-player/)
- Software capable of packaging files into a floppy disk image (may end in FLV, IMA, etc), such as [WinImage](http://www.winimage.com/download.htm)_(30 day free trial)_
- Software capable of packaging files into an cd (ISO) image, such as [Folder2Iso](http://www.trustfm.net/divx/SoftwareFolder2Iso.php)_(freeware)_
- A driver to enable CD-ROM capabilities, such as [oakcdrom.sys](http://www.computerhope.com/download/hardware.htm). _(a generic CD-ROM driver that will work with the majority of all IDE CD-ROM drives)_
- A Windows 3.1 image. If you have an MSDN account, you can download a zip file with the installation files inside it, package them into an ISO image using [Folder2Iso](http://www.trustfm.net/software/utilities/Folder2Iso.php) and mount the image.

## Installing Windows

DOS doesn’t support CDs right out of the box, so we’ll need a driver. I used oakcdrom.sys, a generic CD-ROM driver. At this point, all we’ve got access to is a floppy drive.

Install [WinImage](http://www.winimage.com/winimage.htm) and package the driver into a floppy disk image.

![](Win-311-Image-001.webp)

![](Win-311-Image-002.webp)

![](Win-311-Image-003.webp)

![](Win-311-Image-004.webp)

![](Win-311-Image-005.webp)

![](Win-311-Image-006.webp)

Mount the image to your virtual machine and copy the driver into `c:\dos`.

![](Win-311-Image-007.webp)

![](Win-311-Image-009.webp)

You’ll need to adjust two configuration files to use the new driver:

Type `edit c:\config.sys` and add to the end of that file:

```
DeviceHigh=C:\DOS\oakcdrom.sys /D:CD1
```

And then add to the end of `C:\AUTOEXEC.BAT`:

```
LH MSCDEx /D:CD1
```

These drivers extend BIOS and DOS, respectively, to support the CD-ROM drive, and Windows 3.1 inherits that when it runs.

![](Win-311-Image-011.webp)

![](Win-311-Image-012.webp)

![](Win-311-Image-013.webp)

![](Win-311-Image-014.webp)

![](Win-311-Image-015.webp)

![](Win-311-Image-016.webp)

Reboot the virtual machine to apply your changes and then mount the Windows 3.1 disk.

- If you’ve already got floppy disk or cd-rom images of Windows, then mount those now.
- If you’ve got a zip file with the installation files inside it, like the one I got from MSDN, then you’ll need to run an app (such as [Folder2Iso](http://www.trustfm.net/software/utilities/Folder2Iso.php)) that can package them into a CD ISO, which you can then mount. It’s a straight-forward process.

![Win 311 Image 010](Win-311-Image-010.webp)

With the CD-ROM now recognized and the Windows 3.1 disk mounted, you can boot up into DOS and type `D:` at the prompt to access the disk. Type `setup` to begin installation. I just accepted all the defaults.

![](Win-311-Image-017.webp)

![](Win-311-Image-018.webp)

![](Win-311-Image-019.webp)

![](Win-311-Image-020.webp)

![](Win-311-Image-021.webp)

I opted for all the features, but at what cost? _Over_ _**2 MB**_ _of hard drive space!_ 😱

![](Win-311-Image-022.webp)

![](Win-311-Image-023.webp)

Some changes to the same files we edited earlier. Windows saves a backup copy before modifying it.

![](Win-311-Image-024.webp)

![](Win-311-Image-027.webp)

Wasn’t sure what to do with this. Considered skipping it, but ended up selecting the ‘generic’ printer. Maybe I’ll search for a driver that would allow me to print.

![](Win-311-Image-028.webp)

![](Win-311-Image-029.webp)

![](Win-311-Image-030.webp)

![](Win-311-Image-031.webp)

I _had_ to go through the tutorial. Never too late to learn to use a mouse.

![](Win-311-Image-032.webp)

![](Win-311-Image-033.webp)

![](Win-311-Image-034.webp)

![](Win-311-Image-035.webp)

Mmm… icecream. I’m a little dubious of how they calculate the number of calories in “chocolate sauce” and “nuts”. Weight-watchers app, this is not.

![](Win-311-Image-036.webp)

![](Win-311-Image-037.webp)

![](Win-311-Image-038.webp)

![](Win-311-Image-040.webp)

## What's here?

Wow, notice the evolution of Notepad over 20 years. :p

![Win 311 Image 042](Win-311-Image-042.webp)

Something I threw together.

![Win 311 Image 043](Win-311-Image-043.webp)

Hmm.. what else can I install on here?

### Visual Basic 2.0

Here’s Visual Basic 2.0. Woah. Okay, that one’s improved in 20 years. Notice that a complete installation will require 18 MB! _(gasp)_

![](Win-311-Image-044.webp)

![](Win-311-Image-045.webp)

![](Win-311-Image-046.webp)

![](Win-311-Image-047.webp)

![](Win-311-Image-048.webp)

### QBasic 4.5

![](Win-311-Image-049.webp)

![](Win-311-Image-050.webp)

![](Win-311-Image-051.webp)

I suppose other than finding other apps to install, I’ll try to find a graphics driver, sound driver, etc. Or I’ll get bored after using it for 10 minutes because, come on, the real fun is in getting it to work. Who the heck wants to _use_ it for anything??

You can download this [graphics driver](https://sites.google.com/site/chitchatvmback/misc) and follow the instructions in the zip file; it gets you 256 colors and 1024×768 res in a VMWARE environment. You may also need [these drivers](http://www.sierrahelp.com/Patches-Updates/Patches-Updates-Misc/Win31SVGAUpdate.html).. not sure.

Installed IE too. Ouch.

![](Win-311-Image-061.webp)

![](Win-311-Image-062.webp)
