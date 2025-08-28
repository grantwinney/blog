---
categories:
- WinForms
- VS2022
- Tools of the Trade
- Visual Studio
date: "2023-01-14T20:36:55Z"
description: ""
draft: false
cover:
  image: photo-1441804238730-210ce1c2cc00.jpg
  relative: true
slug: why-doesnt-vs2022-show-my-winforms-ui
summary: Someone at work asked about whether we'd be able to use VS 2022 to work on
  our main WinForms app. It works just fine in VS 2019, so it should work in VS 2022,
  right? Except it doesn't. What we get is white screens of brokenness whenever we
  try to open a Form in the designer. But why?
tags:
- WinForms
- VS2022
- Tools of the Trade
- Visual Studio
title: Why doesn't VS 2022 show my WinForms UI at design time?
---
After migrating some newer projects at work from .NET Core 3.1 (which reached [end of support](https://devblogs.microsoft.com/dotnet/net-core-3-1-will-reach-end-of-support-on-december-13-2022/) a month ago) to .NET 6, I sent a quick message to my teammates about installing VS 2022, which is required to work on .NET 6 apps. That naturally brought up the question about whether we'd be able to use VS 2022 to work on the WinForms app that is our main bread and butter. It works just fine in VS 2019, so it should work in VS 2022, right? Except it doesn't.

Not only does VS2022 throw build errors that VS2019 doesn't, which I managed to clear up with some package upgrades and other minor changes, but it also shows white screens of brokenness whenever a Form is opened in the designer. Well.. that's gonna make things tough.

After a little research, I found a thread in which [Merrie McGaw and Klaus Löeffelmann explain why this is happening](https://developercommunity.visualstudio.com/t/Winforms-net-framework-projects-cant-d/1601210). Basically, it's because [Visual Studio is now a 64-bit app](https://devblogs.microsoft.com/dotnet/msbuild-and-64-bit-visual-studio-2022/) and the WinForms designer runs in the same process as VS. In other words, since VS moved to 64-bit, the "designer" portion of VS is also (now) 64-bit, which doesn't behave well when you try opening Forms with components that are 32-bit. I think it's taken us devs _(me, at least!)_ by surprise because Microsoft is usually good about backwards-compatibility, not to mention all the reminders we've gotten to upgrade to VS2022 and messaging ([like here](https://devblogs.microsoft.com/visualstudio/visual-studio-2022/#visual-studio-2022-is-64-bit)) that states, _"Visual Studio will continue to be a great tool for building 32-bit apps."_

The fix (for them) is to split out the form designer into a second process, running separately from Visual Studio, and do all the work of making the processes talk to each other smoothly while still looking like one big happy app. [They did it for .NET Core](https://devblogs.microsoft.com/dotnet/custom-controls-for-winforms-out-of-process-designer/#the-out-of-process-winforms-designer), which was apparently a [huge challenge](https://visualstudiomagazine.com/articles/2019/12/05/winforms-designer.aspx):

> _For example, when you drag a Button from the Toolbox onto a form – this action is handled by Visual Studio (devenv.exe process which is .NET Framework). But, once you release the mouse button to drop the Button on the form, all further actions (instantiating a Button, rendering it at a specific location, and so on) are related to .NET Core. That means .NET Framework process can no longer handle it._

The fix (for us) is to retarget apps to x64 or AnyCPU, which (if you read through the thread) is difficult and impractical in a lot of situations. Microsoft is still working on fixing it for the .NET Framework, but it's still very much a WIP and Merrie admits that even when they _do_ fix it, it doesn't mean legacy projects won't need to change: _"They may need to be built against this new architecture for full support in the out of process designer."_

The more realistic fix for us is to just keep using VS2019 (which as far as I can tell, [will receive security updates until 2029](https://devblogs.microsoft.com/visualstudio/support-ends-for-older-versions-of-visual-studio-feb2022/#what-does-this-mean-for-you)) for legacy WinForms development until they figure it all out.

## From the Experts

So all that above was me trying to summarize things. Hopefully I didn't make it more confusing, but here's a few cherry-picked quotes from the mouths (fingers?) of the Microsoft devs themselves. And further down, my own example showing off the issue, although if you're here you've probably got your own. 😔

[Merrie McGaw (response #1)](https://developercommunity.visualstudio.com/t/Winforms-net-framework-projects-cant-d/1601210#T-N1601090):

> [T]there were some scenarios related to .NET Framework 32-bit components that we could not make work in Windows Forms designer.

> For .NET Framework, the designer remained in-process. ... Since Visual Studio is now 64-bit, the .NET Framework-based designer now also runs in 64-bit, which means 32-bit code cannot be loaded at design time any longer. ... [I]f you have a .NET Framework application that is referencing a 32-bit component, this component cannot be loaded in Visual Studio 2022 Windows Forms designer, and you will see a white error screen instead. This will not affect your end users, as your application will run just fine, it only affects the designer experience inside of Visual Studio. In the future, we plan to add support for .NET Framework projects to the out of process designer. Once that support is available, it will be possible to load forms that have 32-bit specific dependencies in the designer.

> Due to the way the Framework WinForms Designer was created, the form you are designing runs in the same process as Visual Studio runs in. Unfortunately, with the move to 64bit it meant that references now must be of an architecture that x64 can work with (AnyCPU or 64-bit). If you have access to the source code of the original projects the key is to design them with the reference as AnyCPU, even if you ultimately build and release a 32bit version of the reference.

[Merrie McGaw (response #2)](https://developercommunity.visualstudio.com/t/Winforms-net-framework-projects-cant-d/1601210#T-N1616994):

> The only way for us to fix this in WinForms is a rearchitecting of the designer entirely to run out of the process of Visual Studio - and if it were to do that, you would be able to design with your references in whatever architecture suited your needs. Unfortunately moving the designer out of process is far more than a bug fix; it’s an entire re-architecting of how the design surface in one process communicates with the Visual Studio process and passes the information back and forth about control properties. We have been working on creating an out-of-process WinForms Designer for .NET applications, and we’re getting pretty happy with the user experience in this last VS release. That said, there is still more work to be done to support .NET Framework projects and it doesn’t preclude the need to do something with the references that are causing you trouble now. They may need to be built against this new architecture for full support in the out of process designer.

[Klaus Löeffelmann](https://developercommunity.visualstudio.com/t/Winforms-net-framework-projects-cant-d/1601210#T-N10222903): _(he also wrote a detailed_ [_blog post_](https://devblogs.microsoft.com/dotnet/state-of-the-windows-forms-designer-for-net-applications/) _about this)_

> The problem we are discussing in this thread is multifaceted and we are not talking about a simple bug-fix here. We are talking about conceptual work that was started in the middle of last year and will - even for quite some time - continue well into the new year.

> The question folks on this thread might have in this context: why didn’t we do all this in advance before the 64-bit conversion of Visual Studio? It’s simple: Because many customers (including myself at the time being on the “other side”) would rather have 64-bit support in VS _immediately_, since they are and were _much_ more dependent on the 64-bit need than on the not-yet-resolved 32-bit dependency. They say: For what’s still 32-bit, we’ll use VS2019, either until we’ve made the switch, or the out-of-process WinForms Designer is ready to handle the most common 32-bit scenarios.

> And that’s the status quo. We already have a rudimentary out-of-process .NET Framework 32-bit WinForms Designer under Preview Features in VS 2022.5 Preview 1, which we will continue to make more and more 32-bit legacy-compatible over the next year. You can test this out under _**Tools->Options -> Preview Features**_. So, the situation is constantly improving, and of course we will be approaching a feasible compromise spot from both sides, so to speak...

> It’s not about fixing _the one bug_ in this scenario, and there is certainly not just one single course of action which would make everything work. It’s about identifying the most diverse scenarios one after the other, prioritizing them and then making them intrinsically work again. . . . And to be honest and transparent and to say it _very_ clearly: We also have to reckon with components or scenarios that we can no longer make work at all, because the underlying core technologies used are simply too old and raise security issues that we simply must not risk deploying.

> [T]he better we work together in this respect, the faster we can make progress here. Until then, trying to target _**AnyCPU**_ for all projects in Visual Studio 2022 for many .NET Framework-only scenarios or continue using Visual Studio 2019 for the time being remains your alternative for problematic scenarios.

## From the Users

And in case someone out there is like, oh boo-hoo just retarget for Any CPU and move on with life, that's not an option for a lot of companies.

[DJ Sures](https://developercommunity.visualstudio.com/t/Winforms-net-framework-projects-cant-d/1601210#T-N1677854):

> We cannot be expected to change our target framework on the platform to x64 because your design plan was flawed and broke a significant Visual Studio feature. The 100+ third-party developers cannot “simply” be asked to re-build, test, and re-build their 721 libraries - and then ask 35,723 customers to upgrade.

[Erwin Lunger](https://developercommunity.visualstudio.com/t/Winforms-net-framework-projects-cant-d/1601210#T-N1693701):

> I use only one 32-bit ActiveX-control, but this one isn’t available as 64-bit version. The program which uses this control is very often used in our company and it would be very, very helpful if you could fix the bug. I have now to make a change in the program which is using this ActiveX-control and i am not able to do the change as of your bug.

[Troy Willmot](https://developercommunity.visualstudio.com/t/Winforms-net-framework-projects-cant-d/1601210#T-N10010079):

> I specifically have to compile to x86 because I do not have the source code for the third party components (and won’t be given it), and they do not work compiled into an ‘any cpu’ binary and run on a 64 bit OS (which almost all our customers have now) because they are 32 bit ActiveX controls. . . . I also don’t have this problem with just one vendor, so my chances of getting them all to rewrite for 64 bit (which I have been asking them to do for years) are zilch. Those solutions are for private hardware too (EFTPOS systems and the like), so there aren’t alternatives...

[HerbF](https://developercommunity.visualstudio.com/t/Winforms-net-framework-projects-cant-d/1601210#T-N10011743):

> Our solution works with Office and VSTO, which is 32-bit and requires .Net Framework 4.8. We can’t move to a newer nor a 64-bit version. If we have to leave existing VSTO behind, we may as well leave Office behind as well.

## Minimal Example

Enough of that. You can [read the thread](https://developercommunity.visualstudio.com/t/Winforms-net-framework-projects-cant-d/1601210) for lots more, but it's time for that example! I wanted to [recreate it](https://github.com/grantwinney/BlogCodeSamples/tree/master/DevTools/WinFormsDesignerInVS2022), which was dead simple. Just create a solution with two .NET Framework projects - a WinForms project and a Class Library - and set the target to x64 on the library. It doesn't seem to matter if you leave the main WinForms app as x86 _(why is that?)._

![](https://grantwinney.com/content/images/2023/01/image-23.png)

![](https://grantwinney.com/content/images/2023/01/image-24.png)

Then you can set it up any number of ways to see the problem, although here's a couple easy ones.

![](https://grantwinney.com/content/images/2023/01/image-36.png)

Add a Form to the WinForms project, which inherits from a BaseForm defined in the Class library...

![](https://grantwinney.com/content/images/2023/01/image-37.png)

... or add a Form that contains a User Control defined in the Class Library.

To see the issue:

1. Close the designer portion of the Forms, if they're open.
2. Change the "Platform target" value for the Class Library to x86 (or back to x64).
3. Rebuild the solution.
4. Open the Forms again. They're broken in the designer when targeting x86 (but the app runs), they show in the designer for x64 (but it won't run), and everything is just peachy when targeting Any CPU.

![](https://grantwinney.com/content/images/2023/01/image-28.png)

![](https://grantwinney.com/content/images/2023/01/image-27.png)

![](https://grantwinney.com/content/images/2023/01/image-29.png)

Targeting x86 for class library (designer broken; project runs)

![](https://grantwinney.com/content/images/2023/01/image-30.png)

![](https://grantwinney.com/content/images/2023/01/image-31.png)

![](https://grantwinney.com/content/images/2023/01/image-32.png)

Targeting x64 for class library (designer works; project doesn't run)

![](https://grantwinney.com/content/images/2023/01/image-33.png)

![](https://grantwinney.com/content/images/2023/01/image-34.png)

![](https://grantwinney.com/content/images/2023/01/image-35.png)

Targeting Any CPU for class library (designer works _and_ project runs)

If you want to try out the example yourself, [get the code here](https://github.com/grantwinney/BlogCodeSamples/tree/master/WinFormsDesignerInVS2022). Hopefully we'll be able to go all-in on VS 2022 soon, but I wouldn't bet on it until later this year at least.
