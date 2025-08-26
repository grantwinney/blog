---
categories:
- Retro
- Virtualization
date: "2019-05-04T14:35:40Z"
description: ""
draft: false
cover:
  image: franck-v-516609-unsplash.jpg
slug: running-windows-xp-in-virtualbox
summary: Just got an MSDN account, which always comes with some old treasures (hey,
  beauty's in the eye of the beholder). Take a trip back with me, to the days of Windows
  XP, the beginning of the .NET Framework, and even further... ;)
tags:
- Retro
- Virtualization
title: Running Windows XP in VirtualBox
---


I just got access to an MSDN account with keys for various versions of Windows and Visual Studio, so.... it's retro time! Isn't it funny how something brand new comes out and we get excited, then we get annoyed with it's deficiencies, then it's forgotten when something better comes out, and finally after enough time we get all nostalgic and pull it out of mothballs?

With that in mind, let's check out the best of Windows yesteryear. But first...

Note 1: If you just need to run something in XP with a minimum of fuss, check out the free Windows XP Mode app that runs XP inside of Windows 7. It doesn't work on Windows 10 easily, but there's usually a workaround.

Note 2: I won't share any keys or recommend where to find them, but you were allowed to use XP for 60 days without activating, so you could probably use any key you find and you'll be good for a couple months. Sure, that's annoying, but you're not seriously using this for anything serious are you?

You can't install service packs on 64-bit systems, so you may want to choose the 32-bit option. If you're doing this from MSDN, select the "with Service Pack 3" option to make life easier.

Give it plenty of hard disk space if you plan on installing other apps. It's a pita to resize the partition afterwards, so give it 30 or 40 GB at least. XP had really minimal requirements, so a couple gigs of memory should be more than enough.


Map a Host Machine Folder

After installing XP, the first thing you'll notice is that Internet Explorer is awful and horrible and you probably can't even connect to the Internet (at least until you have the service packs installed). Let's do the very first thing anyone does when they install any version of Windows... install a real browser.

Follow the suggestions in this thread to share a folder with your host machine, or run "Devices / Insert Guest Additions CD image..." in VirtualBox (read more here) and then go to "Devices / Shared Folders / Shared Folders Settings" and map a folder on your host machine. You might have to select "Devices / Optical Drives / Remove disk" first, but maybe not. I mapped the "Downloads" folder, so it'll show up as a drive in the VM under My Computer.

Now you can download Chrome (it'll install v49, the last supported version for XP), Firefox 52.9.0esr (Mozilla's last supported version, but you'll likely have to install SP2 first), or Opera 36 (you get the picture) on the host machine, drop them in the shared folder, and install them from the VM.


Installing Service Packs

XP is ancient but we might as well update it where we can. If you selected the "with Service Pack 3" option like I mentioned, then skip this section.

Otherwise, there's a couple places you can get the SPs from (at least). The easiest is from your MSDN account. Save "Service Pack 1a", "Service Pack 2 (English)" and "Service Pack 3 (x86)" to the shared folder on your host machine.

The files are ISOs, so download Virtual CloneDrive in the VM (you might have to right-click the exe, open properties, and press "unblock") and mount each file to install it. All you have to do is choose Settings and create a half-dozen virtual drives, then mount each one to an ISO on the host machine.

Alternatively, you can grab them from the Microsoft Update Catalog:

 * Windows XP Service Pack 1, Express (download) - 5/15/2004
 * Windows XP Service Pack 2 (download) - 9/13/2005
 * Windows XP Pro Service Pack 2, x64 Edition (download) - 5/23/2008
 * Windows XP Service Pack 3 (download) - 5/19/2009


Install Updates

Although support for XP ended 5 years ago, you can still grab the updates that were available. Click on Start / All Programs / Windows Update and select all the updates. Install, reboot, select more updates, rinse, repeat. Good ol' XP.


Setup a Development Environment

Now that XP has the latest updates and security patches of yesteryear, we can setup a development environment. Gotta have git. The last version of Git for Windows supported on XP was 2.10.0, so download the exe and install it so you can clone some repos!!


Visual Studio 2012

Thought this would be a good one to start with. I was wrong. VS2012 no likey XP.


Visual Studio 2010

You can download the free Visual Studio 2010 Express, or the pro version from MSDN. Surprisingly, the look and feel were pretty similar to what we've got a decade later. The team hadn't yet decided to CAPITALIZE ALL THE MENUS. NuGet integration wasn't built-in, but I'm sure a quick search would turn up someone figuring out how to do it.


Visual Studio .NET 2003

Not retro enough for you? Let's go back another few years and try out VS .NET 2003. It introduced .NET 1.1, so sadly no generics and definitely no LINQ yet. Boooo. If it complains about prereq's but you don't have a prereq CD, try installing the Windows Components it's complaining about.

After that, I ran Windows Update (yet again), tried to install .NET 1.1. SP1 (although it said it was already installed), rebooted, and finally ran the setup.exe file with a special flag that you can see in the command window.

F:\>./setup/setup.exe /NO_BSLN_CHECK

Not sure which part of all that fixed it (or was it a combination of everything?) but I got to a normal installation screen eventually. And here's the best WinForms had to offer:


Visual Basic 6.0

Still not retro enough?? Let's go back a couple decades to VB 6.0. Forget everything you thought you knew... this is before the .NET Framework altogether.

Wow, when did we lose this?! There was a wizard to create an ultra modern app, complete with splash screens, toolbars, "about" screens..... built-in interweb browsers!! I'm not sure why we ever progressed beyond this.


Visual Basic 4.0

Older! More olderer!! Okay, here's VB 4.0 from 25 years ago. Look at that GUI.. every component is a separate floating toolbar.. thing. And the humble beginning of (gag) Crystal Reports. 🤢 🤮


Must. Go. OLDER!!

25 years isn't retro enough?!? You're relentless.

Check out my post from a few years back on installing Windows 3.1. Get a sneak peek at Visual Basic 2.0, QBasic, and all manners of 16-bit awesomeness. ;p
