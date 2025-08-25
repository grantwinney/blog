+++
categories = ["Retro", "Virtualization"]
date = 2013-10-24T22:14:08Z
description = ""
draft = false
image = "https://images.unsplash.com/photo-1487180144351-b8472da7d491?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1080&fit=max&ixid=eyJhcHBfaWQiOjExNzczfQ"
slug = "installing-windows-3-1-in-vmware-player"
summary = "While looking for a copy of Windows 98 on MSDN to install some old software (compatibility mode under Windows 7 didn’t work), I came across Windows 3.11. Installing it was a little tricky though..."
tags = ["Retro", "Virtualization"]
title = "Installing Windows 3.1 in VMware Player"

+++


Every once in awhile it’s interesting in a nerdy way to check out some legacy
technology and… I dunno… reminisce about the old days and how far we’ve come or
some crap like that. Like the fact that the pen drive in my pocket is 10x bigger
than the largest hard drives 15 years ago.

I decided this week to install Windows 3.1 on a virtual machine. First, you’ll
need to install DOS [__GHOST_URL__/installing-dos-6-22-in-vmware-player/]. When
that’s done, power off the virtual machine.

Requirements
 * VMware Player [http://www.vmware.com/products/player/]
 * An instance of DOS 6.22 running in a virtual machine
   [__GHOST_URL__/installing-dos-6-22-in-vmware-player/]
 * Software capable of packaging files into a floppy disk image (may end in FLV,
   IMA, etc), such as WinImage [http://www.winimage.com/download.htm](30 day
   free trial)
 * Software capable of packaging files into an cd (ISO) image, such as 
   Folder2Iso [http://www.trustfm.net/divx/SoftwareFolder2Iso.php](freeware)
 * A driver to enable CD-ROM capabilities, such as oakcdrom.sys
   [http://www.computerhope.com/download/hardware.htm]. (a generic CD-ROM driver
   that will work with the majority of all IDE CD-ROM drives)
 * A Windows 3.1 image. If you have an MSDN account, you can download a zip file
   with the installation files inside it, package them into an ISO image using 
   Folder2Iso [http://www.trustfm.net/software/utilities/Folder2Iso.php] and
   mount the image.

Installing Windows
DOS doesn’t support CDs right out of the box, so we’ll need a driver. I used
oakcdrom.sys, a generic CD-ROM driver. At this point, all we’ve got access to is
a floppy drive.

Install WinImage [http://www.winimage.com/winimage.htm] and package the driver
into a floppy disk image.

Mount the image to your virtual machine and copy the driver into c:\dos.

You’ll need to adjust two configuration files to use the new driver:

Type edit c:\config.sys and add to the end of that file:

DeviceHigh=C:\DOS\oakcdrom.sys /D:CD1

And then add to the end of C:\AUTOEXEC.BAT:

LH MSCDEx /D:CD1

These drivers extend BIOS and DOS, respectively, to support the CD-ROM drive,
and Windows 3.1 inherits that when it runs. (credit: tomshardware.com
[http://www.tomshardware.com/forum/173467-48-error-windows-install#8953171])

Reboot the virtual machine to apply your changes and then mount the Windows 3.1
disk.

 * If you’ve already got floppy disk or cd-rom images of Windows, then mount
   those now.
 * If you’ve got a zip file with the installation files inside it, like the one
   I got from MSDN, then you’ll need to run an app (such as Folder2Iso
   [http://www.trustfm.net/software/utilities/Folder2Iso.php]) that can package
   them into a CD ISO, which you can then mount. It’s a straight-forward
   process.

With the CD-ROM now recognized and the Windows 3.1 disk mounted, you can boot up
into DOS and type D: at the prompt to access the disk. Type setup to begin
installation. I just accepted all the defaults.

I opted for all the features, but at what cost? Over 2 MB of hard drive space! 
😱

Some changes to the same files we edited earlier. Windows saves a backup copy
before modifying it.

Wasn’t sure what to do with this. Considered skipping it, but ended up selecting
the ‘generic’ printer. Maybe I’ll search for a driver that would allow me to
print.

I had to go through the tutorial. Never too late to learn to use a mouse.

Mmm… icecream. I’m a little dubious of how they calculate the number of calories
in “chocolate sauce” and “nuts”. Weight-watchers app, this is not.

What's here?
Wow, notice the evolution of Notepad over 20 years. :p

Something I threw together.

Hmm.. what else can I install on here?

Visual Basic 2.0
Here’s Visual Basic 2.0. Woah. Okay, that one’s improved in 20 years. Notice
that a complete installation will require 18 MB! (gasp)

QBasic 4.5
I suppose other than finding other apps to install, I’ll try to find a graphics
driver, sound driver, etc. Or I’ll get bored after using it for 10 minutes
because, come on, the real fun is in getting it to work. Who the heck wants to 
use it for anything??

You can download this graphics driver
[https://sites.google.com/site/chitchatvmback/misc] and follow the instructions
in the zip file; it gets you 256 colors and 1024×768 res in a VMWARE
environment. You may also need these drivers
[http://www.sierrahelp.com/Patches-Updates/Patches-Updates-Misc/Win31SVGAUpdate.html]
.. not sure.

Installed IE too. Ouch. Get it here.
[http://www.oldapps.com/internet_explorer.php?old_internet_explorer=1?download]