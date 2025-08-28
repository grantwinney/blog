---
categories:
  - .NET 8
  - WinForms
  - C# 12
  - Coding
date: 2024-12-18T15:50:31Z
description: ""
draft: false
cover:
  image: harpal-singh-_zKxPsGOGKg-unsplash.jpg
slug: how-to-use-getstockicon-for-winforms
summary: Buried deep in the list of .NET 8 improvements for WinForms is the GetStockIcon method. It gives us a way to access stock Windows icons at runtime for the OS the app is running on. Let's check it out.
tags:
  - WinForms
  - Coding
  - CSharp
  - DotNet8
  - CSharp12
title: How to Use GetStockIcon for WinForms in .NET 8
---
Scouring the features that WinForms got in .NET 8, I found one slipped in near the very bottom of the list under "[miscellaneous improvements](https://learn.microsoft.com/en-us/dotnet/desktop/winforms/whats-new/net80?view=netdesktop-9.0#miscellaneous-improvements)" called [GetStockIcon](https://learn.microsoft.com/en-us/dotnet/api/system.drawing.systemicons.getstockicon). It's a new method for grabbing Windows stock icons (i.e. save, folder, etc) at runtime, to use in the UI.

When I've wanted to add system icons to buttons, toolbars, etc in the past, it usually meant extracting them from shell32.dll, imageres.dll.mun, the [Visual Studio Image Library](https://www.microsoft.com/en-us/download/details.aspx?id=35825), etc, and then copying them into the project. Then I'd add them to an `ImageList` and hook that up to various UI elements. So I'm wondering.. does this new method give us an easier way to use system icons?

> The code in this post is available on [GitHub](https://github.com/grantwinney/Surviving-WinForms/tree/master/.NET%2008/GetStockIcon?ref=grantwinney.com), for you to use, extend, or just follow along while you read... and hopefully discover something new along the way!

## Usage

The way it's called is simple enough. We just pass a [StockIconId enum](https://learn.microsoft.com/en-us/dotnet/api/system.drawing.stockiconid) value to tell it which icon to retrieve, and then do whatever we like with the icon:

```csharp
Icon driveIcon = SystemIcons.GetStockIcon(StockIconId.DriveNet);
```

Once we have it, we could add it to an `ImageList` and then use that on a `Button` or other controls. We could also specify a size in the second parameter, like I did here with a `PictureBox` to display a larger icon that's still sharp:

```csharp
// Populate image list with default size, which should be 32 px
imageList1.Images.Add("Conn", SystemIcons.GetStockIcon(StockIconId.DriveNet));
imageList1.Images.Add("Disconn", SystemIcons.GetStockIcon(StockIconId.DriveNetDisabled));

button1.ImageKey = "Conn";
pictureBox1.Image = SystemIcons.GetStockIcon(StockIconId.DriveNet, 128).ToBitmap();
```

![](https://grantwinney.com/content/images/2024/12/WcaF5EAVqE.gif)

Or we could loop through _all_ of them, adding each to an `ImageList`, and then use that to create the world's busiest toolbar: 😏

```csharp
foreach (StockIconId icon in Enum.GetValues(typeof(StockIconId)))
    imageList2.Images.Add(icon.ToString(), SystemIcons.GetStockIcon(icon, 64));

toolStrip1.Items.AddRange(imageList2.Images.Keys.Cast<string>().Select(x =>
    new ToolStripButton(imageList2.Images[x]) { ToolTipText = x }).ToArray());
```

![](https://grantwinney.com/content/images/2024/12/image-6.png)

## Pros and Cons

The biggest limitation is only being able to call this at runtime. Of course, that's just the nature of this being a method call, but one of the best things about WinForms is its drag-and-drop designer and this definitely works against that. I'm not sure how much usage this will get if it means having a designer with blank toolbars, incomplete buttons, etc. Maybe I'm missing an obvious use case?

A nice feature, though, is that this method _"returns icons that are themed for the running version of Windows"._ If we copy icons into the project, they are what they are, unchanged no matter what version of Windows someone happens to be running. But with this new call, when someone runs our app in a different version of Windows from the one we designed it in, they'll see the icons that are normal for their OS.

## Learning More

If you want to learn more, check out the [GetStockIcon](https://learn.microsoft.com/en-us/dotnet/api/system.drawing.systemicons.getstockicon) docs, the [StockIconId](https://learn.microsoft.com/en-us/dotnet/api/system.drawing.stockiconid) enum that lists all the available images, and the [StockIconOptions](https://learn.microsoft.com/en-us/dotnet/api/system.drawing.stockiconoptions) enum that lets us set a few options like adding a link overlay to the image. Since a couple of the options involve resizing the icon, we're not allowed to specify a size _and_ options, but that also means it's not possible to request a larger image that has a link overlay.. seems like an odd choice.

Lastly, since the StockIconId page doesn't include the actual _images_ of the icons, here's a list so you can see what they look like, at least in Windows 11:

|Name|Value|Description|Image|
|---|---|---|---|
|DocumentNoAssociation|0|Document (blank page), no associated program.|![](https://grantwinney.com/content/images/2024/12/DocumentNoAssociation-2.png)|
|DocumentWithAssociation|1|Document with an associated program.|![](https://grantwinney.com/content/images/2024/12/DocumentWithAssociation-2.png)|
|Application|2|Generic application with no custom icon.|![](https://grantwinney.com/content/images/2024/12/Application-2.png)|
|Folder|3|Closed folder.|![](https://grantwinney.com/content/images/2024/12/Folder-2.png)|
|FolderOpen|4|Open folder.|![](https://grantwinney.com/content/images/2024/12/FolderOpen-2.png)|
|Drive525|5|5.25" floppy disk drive.|![](https://grantwinney.com/content/images/2024/12/Drive525-2.png)|
|Drive35|6|3.5" floppy disk drive.|![](https://grantwinney.com/content/images/2024/12/Drive35-2.png)|
|DriveRemovable|7|Removable drive.|![](https://grantwinney.com/content/images/2024/12/DriveRemovable-2.png)|
|DriveFixed|8|Fixed drive.|![](https://grantwinney.com/content/images/2024/12/DriveFixed-2.png)|
|DriveNet|9|Network drive.|![](https://grantwinney.com/content/images/2024/12/DriveNet-2.png)|
|DriveNetDisabled|10|Disabled network drive.|![](https://grantwinney.com/content/images/2024/12/DriveNetDisabled-2.png)|
|DriveCD|11|CD drive.|![](https://grantwinney.com/content/images/2024/12/DriveCD-2.png)|
|DriveRam|12|RAM disk drive.|![](https://grantwinney.com/content/images/2024/12/DriveRam-2.png)|
|World|13|Entire network.|![](https://grantwinney.com/content/images/2024/12/World-2.png)|
|Server|15|A computer on the network.|![](https://grantwinney.com/content/images/2024/12/Server-2.png)|
|Printer|16|Printer.|![](https://grantwinney.com/content/images/2024/12/Printer-2.png)|
|MyNetwork|17|My network places.|![](https://grantwinney.com/content/images/2024/12/MyNetwork-2.png)|
|Find|22|Find.|![](https://grantwinney.com/content/images/2024/12/Find-2.png)|
|Help|23|Help.|![](https://grantwinney.com/content/images/2024/12/Help-2.png)|
|Share|28|Overlay for shared items.|![](https://grantwinney.com/content/images/2024/12/Share-2.png)|
|Link|29|Overlay for shortcuts to items.|![](https://grantwinney.com/content/images/2024/12/Link-2.png)|
|SlowFile|30|Overlay for slow items.|![](https://grantwinney.com/content/images/2024/12/SlowFile-2.png)|
|Recycler|31|Empty recycle bin.|![](https://grantwinney.com/content/images/2024/12/Recycler-2.png)|
|RecyclerFull|32|Full recycle bin.|![](https://grantwinney.com/content/images/2024/12/RecyclerFull-2.png)|
|MediaCDAudio|40|Audio CD media.|![](https://grantwinney.com/content/images/2024/12/MediaCDAudio-2.png)|
|Lock|47|Security lock.|![](https://grantwinney.com/content/images/2024/12/Lock-2.png)|
|AutoList|49|AutoList.|![](https://grantwinney.com/content/images/2024/12/AutoList-2.png)|
|PrinterNet|50|Network printer.|![](https://grantwinney.com/content/images/2024/12/PrinterNet-2.png)|
|ServerShare|51|Server share.|![](https://grantwinney.com/content/images/2024/12/ServerShare-2.png)|
|PrinterFax|52|Fax printer.|![](https://grantwinney.com/content/images/2024/12/PrinterFax-2.png)|
|PrinterFaxNet|53|Networked fax printer.|![](https://grantwinney.com/content/images/2024/12/PrinterFaxNet-2.png)|
|PrinterFile|54|Print to file.|![](https://grantwinney.com/content/images/2024/12/PrinterFile-2.png)|
|Stack|55|Stack.|![](https://grantwinney.com/content/images/2024/12/Stack-2.png)|
|MediaSVCD|56|SVCD media.|![](https://grantwinney.com/content/images/2024/12/MediaSVCD-2.png)|
|StuffedFolder|57|Folder containing other items.|![](https://grantwinney.com/content/images/2024/12/StuffedFolder-2.png)|
|DriveUnknown|58|Unknown drive.|![](https://grantwinney.com/content/images/2024/12/DriveUnknown-2.png)|
|DriveDVD|59|DVD drive.|![](https://grantwinney.com/content/images/2024/12/DriveDVD-2.png)|
|MediaDVD|60|DVD media.|![](https://grantwinney.com/content/images/2024/12/MediaDVD-2.png)|
|MediaDVDRAM|61|DVD-RAM media.|![](https://grantwinney.com/content/images/2024/12/MediaDVDRAM-2.png)|
|MediaDVDRW|62|DVD-RW media.|![](https://grantwinney.com/content/images/2024/12/MediaDVDRW-2.png)|
|MediaDVDR|63|DVD-R media.|![](https://grantwinney.com/content/images/2024/12/MediaDVDR-2.png)|
|MediaDVDROM|64|DVD-ROM media.|![](https://grantwinney.com/content/images/2024/12/MediaDVDROM-2.png)|
|MediaCDAudioPlus|65|CD+ (Enhanced CD) media.|![](https://grantwinney.com/content/images/2024/12/MediaCDAudioPlus-2.png)|
|MediaCDRW|66|CD-RW media.|![](https://grantwinney.com/content/images/2024/12/MediaCDRW-2.png)|
|MediaCDR|67|CD-R media.|![](https://grantwinney.com/content/images/2024/12/MediaCDR-2.png)|
|MediaCDBurn|68|Burning CD.|![](https://grantwinney.com/content/images/2024/12/MediaCDBurn-2.png)|
|MediaBlankCD|69|Blank CD media.|![](https://grantwinney.com/content/images/2024/12/MediaBlankCD-2.png)|
|MediaCDROM|70|CD-ROM media.|![](https://grantwinney.com/content/images/2024/12/MediaCDROM-2.png)|
|AudioFiles|71|Audio files.|![](https://grantwinney.com/content/images/2024/12/AudioFiles-2.png)|
|ImageFiles|72|Image files.|![](https://grantwinney.com/content/images/2024/12/ImageFiles-2.png)|
|VideoFiles|73|Video files.|![](https://grantwinney.com/content/images/2024/12/VideoFiles-2.png)|
|MixedFiles|74|Mixed files.|![](https://grantwinney.com/content/images/2024/12/MixedFiles-2.png)|
|FolderBack|75|Folder back.|![](https://grantwinney.com/content/images/2024/12/FolderBack-2.png)|
|FolderFront|76|Folder front.|![](https://grantwinney.com/content/images/2024/12/FolderFront-2.png)|
|Shield|77|Security shield. Use for UAC prompts only.|![](https://grantwinney.com/content/images/2024/12/Shield-2.png)|
|Warning|78|Warning.|![](https://grantwinney.com/content/images/2024/12/Warning-2.png)|
|Info|79|Informational.|![](https://grantwinney.com/content/images/2024/12/Info-2.png)|
|Error|80|Error.|![](https://grantwinney.com/content/images/2024/12/Error-2.png)|
|Key|81|Key / secure.|![](https://grantwinney.com/content/images/2024/12/Key-2.png)|
|Software|82|Software.|![](https://grantwinney.com/content/images/2024/12/Software-2.png)|
|Rename|83|Rename.|![](https://grantwinney.com/content/images/2024/12/Rename-2.png)|
|Delete|84|Delete.|![](https://grantwinney.com/content/images/2024/12/Delete-2.png)|
|MediaAudioDVD|85|Audio DVD media.|![](https://grantwinney.com/content/images/2024/12/MediaAudioDVD-2.png)|
|MediaMovieDVD|86|Movied DVD media.|![](https://grantwinney.com/content/images/2024/12/MediaMovieDVD-2.png)|
|MediaEnhancedCD|87|Enhanced CD media.|![](https://grantwinney.com/content/images/2024/12/MediaEnhancedCD-2.png)|
|MediaEnhancedDVD|88|Enhanced DVD media.|![](https://grantwinney.com/content/images/2024/12/MediaEnhancedDVD-2.png)|
|MediaHDDVD|89|HD-DVD media.|![](https://grantwinney.com/content/images/2024/12/MediaHDDVD-2.png)|
|MediaBluRay|90|BluRay media.|![](https://grantwinney.com/content/images/2024/12/MediaBluRay-2.png)|
|MediaVCD|91|VCD media.|![](https://grantwinney.com/content/images/2024/12/MediaVCD-2.png)|
|MediaDVDPlusR|92|DVD+R media.|![](https://grantwinney.com/content/images/2024/12/MediaDVDPlusR-2.png)|
|MediaDVDPlusRW|93|DVD+RW media.|![](https://grantwinney.com/content/images/2024/12/MediaDVDPlusRW-2.png)|
|DesktopPC|94|Desktop computer.|![](https://grantwinney.com/content/images/2024/12/DesktopPC-2.png)|
|MobilePC|95|Mobile computer.|![](https://grantwinney.com/content/images/2024/12/MobilePC-2.png)|
|Users|96|Users.|![](https://grantwinney.com/content/images/2024/12/Users-2.png)|
|MediaSmartMedia|97|Smart media.|![](https://grantwinney.com/content/images/2024/12/MediaSmartMedia-2.png)|
|MediaCompactFlash|98|Compact Flash.|![](https://grantwinney.com/content/images/2024/12/MediaCompactFlash-2.png)|
|DeviceCellPhone|99|Cell phone.|![](https://grantwinney.com/content/images/2024/12/DeviceCellPhone-2.png)|
|DeviceCamera|100|Camera.|![](https://grantwinney.com/content/images/2024/12/DeviceCamera-2.png)|
|DeviceVideoCamera|101|Video camera.|![](https://grantwinney.com/content/images/2024/12/DeviceVideoCamera-2.png)|
|DeviceAudioPlayer|102|Audio player.|![](https://grantwinney.com/content/images/2024/12/DeviceAudioPlayer-2.png)|
|NetworkConnect|103|Connect to network.|![](https://grantwinney.com/content/images/2024/12/NetworkConnect-2.png)|
|Internet|104|Internet.|![](https://grantwinney.com/content/images/2024/12/Internet-2.png)|
|ZipFile|105|ZIP file.|![](https://grantwinney.com/content/images/2024/12/ZipFile-2.png)|
|Settings|106|Settings.|![](https://grantwinney.com/content/images/2024/12/Settings-2.png)|
|DriveHDDVD|132|HD-DVD drive.|![](https://grantwinney.com/content/images/2024/12/DriveHDDVD-2.png)|
|DriveBD|133|BluRay drive.|![](https://grantwinney.com/content/images/2024/12/DriveBD-2.png)|
|MediaHDDVDROM|134|HD-DVD-ROM media.|![](https://grantwinney.com/content/images/2024/12/MediaHDDVDROM-2.png)|
|MediaHDDVDR|135|HD-DVD-R media.|![](https://grantwinney.com/content/images/2024/12/MediaHDDVDR-2.png)|
|MediaHDDVDRAM|136|HD-DVD-RAM media.|![](https://grantwinney.com/content/images/2024/12/MediaHDDVDRAM-2.png)|
|MediaBDROM|137|BluRay-ROM media.|![](https://grantwinney.com/content/images/2024/12/MediaBDROM-2.png)|
|MediaBDR|138|BluRay-R media.|![](https://grantwinney.com/content/images/2024/12/MediaBDR-2.png)|
|MediaBDRE|139|BluRay-RE media.|![](https://grantwinney.com/content/images/2024/12/MediaBDRE-2.png)|
|ClusteredDrive|140|Clustered disk.|![](https://grantwinney.com/content/images/2024/12/ClusteredDrive-2.png)|

If you found this content useful, and would like to learn more about a variety of [C#](https://grantwinney.com/tag/csharp/) features, check out my [CSharpDotNetFeatures repo](https://github.com/grantwinney/CSharpDotNetFeatures), where you'll find links to plenty more blog posts and practical examples!