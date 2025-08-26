---
categories:
- GitHub
- Tools of the Trade
date: "2019-10-06T03:55:19Z"
description: ""
draft: false
cover:
  image: https://images.unsplash.com/photo-1512908593802-fc940f380825?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=2000&fit=max&ixid=eyJhcHBfaWQiOjExNzczfQ
slug: first-look-github-package-registry-beta
summary: Most of us host something (and some of us everything) on GitHub, especially
  since they host private repos for free too now. I've been eager to try the GitHub
  Package Registry since they announced it last May. Well, I just got access to the
  beta, so let's see what we can do!
tags:
- GitHub
- Tools of the Trade
title: What's the GitHub Package Registry?
---


A few prerequisites before we dig in...

 * If you're new to all this, check out "What's a package manager?"
 * If you want to upload a package, install the NuGet command line tools. (nuget.exe isn't a setup file - just save it somewhere and add it to your path)
 * If you want to reference your package, install Visual Studio. (it's free)
 * When you're done here, consider securing your GitHub account too.

Most of us host something (and some of us everything) on GitHub, especially since they host private repos for free too now. I've been eager to try the GitHub Package Registry since they announced it last May - I just got access to the beta.

In their own words, GPR "allows you to host your packages and code in one place. You can host software packages privately or publicly and use them as dependencies in your projects." That doesn't really answer any of my questions though, such as:

 * Will it streamline the current process of uploading packages to NuGet?
 * Is it meant as a backup to the many package registries already available?
 * Or do they hope it'll become "the one registry to rule them all"?


Create a personal access token

Everything that follows pretty much came out of these docs, so I'd recommend checking them out later, and keeping them close at hand as you read through this.

 * About GitHub Package Registry
 * Configuring NuGet for use with GitHub Package Registry
 * GitHub Package Registry: Your packages, at home with their code

The first step, no matter which language you're using to connect to the GPR, is to create a personal access token. Think of it this way - you want a third party to be able to access your data on GitHub. You could just give them your username and password and trust that they'll only access what they need. Don't do that. Ever! 🤬

Instead, create a token that grants exactly what the third party says it needs access to, and nothing more. Then it's GitHub's job to make sure it actually happens. Even though the app were granting access to is also a GitHub service, they want us to treat GPR just like anything else. It's not a bad idea actually.

So, create a new token and select the read:packages and write:packages scopes. Leave the repo scope selected! Technically, if you're repo is public you shouldn't need it... but if you're going to try using the package in VS you'll need it. I'll elaborate later. Oh, and copy the token it generates after you hit "Generate Token" or you'll be doing it over again in the next step. 😅


Push your first package

In order to do this yourself, you'll need something to publish. If you don't have a project in mind, just make a simple console app in VS that prints out "hello world!" and push it to GitHub. I created a one-off repo called "github-package-repo-first-try" for this purpose.

Checkout the "packages" tab for your repo on GitHub. When there aren't any yet, you'll get a reminder of the commands to run for pushing your first package.

First, tell NuGet it can use GPR as a source, and give it the credentials to use:

nuget sources Add -Name "GPR" \
     -Source "https://nuget.pkg.github.com/OWNER/index.json" \
     -UserName USERNAME -Password TOKEN

So for me, that'd be:

nuget sources Add -Name "GPR" -Source "https://nuget.pkg.github.com/grantwinney/index.json" -UserName grantwinney -Password <my_token>

If all goes well, you'll get a confirmation message:

Package source with Name: GPR added successfully.

Then push the package via the command line. At this point, you should open your project and run Build / Pack, or open my project, open the project properties and in the "Package" tab change the package version to "1.0.1", and then build it.

Change to the directory where the package was published, probably under bin/debug, or provide the full path.

> nuget push HelloWorld.1.0.0.nupkg -Source "GPR"

Pushing HelloWorld.1.0.0.nupkg to 'https://nuget.pkg.github.com/grantwinney'...
  PUT https://nuget.pkg.github.com/grantwinney/
  OK https://nuget.pkg.github.com/grantwinney/ 1392ms

Your package was pushed.

If you forgot to change the version number and try pushing the package, you'll get a conflict message. Just change the package number and try again.

> nuget push HelloWorld.1.0.0.nupkg -Source "GPR"

Pushing HelloWorld.1.0.0.nupkg to 'https://nuget.pkg.github.com/grantwinney'...
  PUT https://nuget.pkg.github.com/grantwinney/
  
WARNING: Error: Version HelloWorld of "1.0.0" has already been pushed.
  Conflict https://nuget.pkg.github.com/grantwinney/ 807ms
See help for push option to automatically skip duplicates.
Response status code does not indicate success: 409 (Conflict).

That's it! Here's what it looks like on GitHub after pushing packages for my GhostSharp project. I uploaded two versions of GhostSharp - 1.0.2 and 1.0.4 - and you can see them listed in the lower-right corner.


Reference your package in VS

This, unfortunately, was a crappier experience than I'd hoped for. I'm not sure if it's a problem with the GitHub Package Registry or something else, but referencing the new package from GitHub didn't work right away. Let me back up a few steps - here's how things should work.

Create a new project, which you'll use to consume the package you just pushed to the GPR. Or if you're using the project I created, there's a couple in there already - one targets .NET Core 2.2 and the other targets .NET Framework 4.7.

Right-click your project's dependencies and choose "Manage NuGet Packages...", then switch the "Package source" to GPR. You should see anything you've uploaded for any of your personal projects. I ran into problems with this at first, but I'll explain all that later.


Including assembly files (modifying the nuspec)

This is all I've ever had to do when referencing a NuGet.org package, including my own GhostSharp package. GhostSharp is a .NET Standard project, and selecting it on this screen just works.

Unfortunately, referencing my "test" .NET Standard package from the GPR didn't work. I tried it with a .NET Core app and a .NET Framework app, but nada. It's a .NET Standard app, so it should work in both of these. 😕

Restarting VS, clearing the NuGet caches, wiping out the bin/obj folders - none of it fixed it. I started suspecting something was missing from the .nuspec file VS generated but when I compared it to the GhostSharp package on NuGet.org, the layout was the same.

What ended up fixing it, although I'm still not sure why it's needed, was to include assembly files. I opened up the nupkg file that VS built, added a files node to the .nuspec file per this suggestion, then upped the version number to 1.0.1 (and renamed the nupkg file to match), and then ran the earlier command to the push it to the GPR.

<?xml version="1.0" encoding="utf-8"?>
<package xmlns="http://schemas.microsoft.com/packaging/2012/06/nuspec.xsd">
  <metadata>
    <id>HelloWorld</id>
    <version>1.0.1</version>
    <authors>HelloWorld</authors>
    <owners>HelloWorld</owners>
    <requireLicenseAcceptance>false</requireLicenseAcceptance>
    <description>A simple app for use with the GitHub package repository.</description>
    <repository url="https://github.com/grantwinney/github-package-repo-first-try" />
    <dependencies>
      <group targetFramework=".NETStandard2.0" />
    </dependencies>
  </metadata>
  <files>
    <file src="bin\Release\*.*" target="lib/net45" />
  </files>
</package>

The result? Everything. Works. WTF.

Oooookay. My GhostSharp package on NuGet.org does not have that files node. And when I download my "test" package and compare them, before and after adding the files node, there's no change at all to the package other than the .nuspec file itself. It didn't actually include anything else in the package. Yet everything works. Welcome to modern development folks.


Include repo scope on your token - even for public repos

This was the other issue I ran into, although if you left repo scope selected on your token like I told you too, hopefully you didn't run into this one.

When I initially tried to list packages from the GPR in Visual Studio, it prompted me for a password. I tried my GitHub password, then the token string - nothing. The error message in the console was... less than helpful.

It did at least show me the URI it was trying to access, and when I entered that directly into a browser window I get the same authentication prompt. I entered my GitHub token string again, and got a much better response:

{"errors":
    [{"code":"Your token has not been granted the required scopes to execute this query. The 'name' field requires one of the following scopes",
      "message":" ['repo'], but your token has only been granted the: ['read:packages', 'write:packages'] scopes. Please modify your token's scopes at: https://github.com/settings/tokens."}]
}

Note the part about the additional scope. I initially thought that modifying the token to include the public_repo scope would be enough, since that allows a third party to "access public repositories", but nopedy nope:

{"errors":
    [{"code":"Your token has not been granted the required scopes to execute this query. The 'name' field requires one of the following scopes",
      "message":" ['repo'], but your token has only been granted the: ['public_repo', 'read:packages', 'write:packages'] scopes. Please modify your token's scopes at: https://github.com/settings/tokens."}]
}

The solution was to leave the repo scope selected in the first place, which is why I told you to do it earlier. The docs said somewhere that that scope is only needed for private repos, but apparently not. I added the additional scope and entered my token as the password, and this was the response:

{
    "data":
    [
        {
            "@type": "Package",
            "authors": "grant",
            "copyright": "",
            "description": "This is a C# wrapper around the Ghost RESTful Content API, documented here: https://docs.ghost.org/api/content/",
            "iconUrl": "",
            "id": "GhostSharp",
            "isPrerelease": false,
            "language": "",
            "licenseUrl": "https://aka.ms/deprecateLicenseUrl",
            "requireLicenseAcceptance": true,
            "summary": "",
            "tags": "ghost api rest api-wrapper netstandard wrapper-api wrapper",
            "title": "GhostSharp",
            "totalDownloads": 0,
            "verified": true,
            "version": "1.0.4",
            "versions":
            [
                {
                    "version": "1.0.4",
                    "downloads": 0,
                    "@id": "https://nuget.pkg.github.com/grantwinney/GhostSharp/1.0.4.json"
                },
                {
                    "version": "1.0.2",
                    "downloads": 0,
                    "@id": "https://nuget.pkg.github.com/grantwinney/GhostSharp/1.0.2.json"
                }
            ]
        },
        {
            "@type": "Package",
            "authors": "HelloWorld",
            "copyright": "",
            "description": "A simple app for use with the GitHub package repository.",
            "iconUrl": "",
            "id": "HelloWorld",
            "isPrerelease": false,
            "language": "",
            "licenseUrl": "",
            "requireLicenseAcceptance": false,
            "summary": "",
            "tags": "",
            "title": "HelloWorld",
            "totalDownloads": 0,
            "verified": true,
            "version": "1.0.0",
            "versions":
            [
                {
                    "version": "1.0.0",
                    "downloads": 0,
                    "@id": "https://nuget.pkg.github.com/grantwinney/HelloWorld/1.0.0.json"
                }
            ]
        }
    ],
    "totalHits": 2
}

Tried it again in Visual Studio, and the rest is history.

If you'd like to see a similar tutorial but for npm instead, there were two recent posts on The DEV:

 * How to publish packages to the GitHub Package Registry
 * Create Your First Github Package


Concerns

A few issues and concerns I have about the GPR...


Ease of Use

I ran into a couple irritating issues. One is a documentation issue, but the other (with the files node) I'm not sure about yet. I may test it more, or submit a bug report... or just let it go and hope someone from Microsoft discovers this post.


Discoverability

When I'm looking for a package to use, I use NuGet.org or RubyGems - not GitHub. I might end up there after clicking a link on one of the other sites. So will they make it easy to search the GPR globally somehow? Or is this really just intended as a backup to existing package management sites?


Community

What about registries that provide some aspect of community-building, or rallying around a particular language or framework? Will the GPR have something similar? I'm not really sure what I'm looking for here...


Reliability

Regarding deleting packages you've uploaded, they state:

To avoid breaking projects that may depend on your packages, GitHub Package Registry does not support package deletion or deleting a version of a package.

Under special circumstances, such as for legal reasons or to conform with GDPR standards, you can request deleting a package through GitHub Support.

This seems reasonable, and in line with other package management sites like NuGet.org and npm. Failure to do this has wreaked havoc before. But wait a sec...

GitHub allows you to delete a repository, and repositories contain your packages. So what happens then? I tried deleting (but didn't) the repository I was using to test this, and it sure seems like it would. Do the packages float around detached? 😕

Anyway, if you get beta access or they go live with everything, I'd love to hear about your experiences too. Good luck, and have fun!
