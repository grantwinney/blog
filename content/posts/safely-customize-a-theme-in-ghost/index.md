---
categories:
- Ghost Blog
date: "2017-07-24T18:57:36Z"
description: ""
draft: false
cover:
  image: pile-of-rocks.jpg
slug: safely-customize-a-theme-in-ghost
tags:
- Ghost Blog
title: Safely Build on a Ghost Theme
---
As of this writing, my blog runs on the [Ghost platform](https://ghost.org/), and I was mildly surprised when I ran a `ghost update` the other day and suddenly my custom themes and scripts were just gone! Luckily I use [DigitalOcean](https://m.do.co/c/448f25462030) with backups enabled, and I had a backup from just a couple days before. I rolled back, verified my styles and customizations were present, then ran `ghost update` again. Wiped out.

In retrospect, this makes sense. There are going to be updates to Casper, the default Ghost theme, and how should they reconcile that with any local changes I've made? They can't reasonably, so they just overwrite it. WordPress had the concept of [child themes](https://codex.wordpress.org/Child_Themes), which allowed for extending a base theme, so I attempted to do something similar with Ghost.

If you find yourself wanting to use most of the Casper (or any other) theme, but with some customizations (and you don't want to shove them all into the "Code injection" tab in the ghost admin screen), then you may want to do what I did.

## Fork the theme in GitHub and clone it locally

Fork the theme (i.e. [Casper](https://github.com/TryGhost/Casper)) in GitHub and then clone it locally:

`git clone https://github.com/YOUR-USERNAME/YOUR-FORKED-REPO.git`

Rename the directory on your local machine so it's different than the default theme name - something like `Casper-YOUR-USERNAME`. This is necessary when uploading the child theme later - the name cannot be the same as the default.

## Modify it to your liking

Make any changes you'd like to the theme. I made small tweaks to add more social icons and whitespace, change the blockquote style, use [Prism.js](http://prismjs.com/) for code styling, etc.

## Package and upload it

Compress the local directory so you end up with a `Casper-YOUR-USERNAME.zip` file, and then use the Design tab under settings in the Ghost admin to upload your new theme. Activate it (it should prompt you). Restart Ghost using `ghost restart` if you don't see the changes applied (but they should be).

![](https://grantwinney.com/content/images/2017/07/ghost-upload-theme-1.png)

It's worth noting that [there's instructions for automating this process](https://github.com/marketplace/actions/deploy-ghost-theme) too, where pushing changes to a custom theme in GitHub will cause it to be automatically deployed to a working Ghost blog. Very convenient!

## Fetch latest official changes, periodically

To avoid missing out on future updates to the default Casper theme, [keep your fork up to date](https://gist.github.com/CristinaSolana/1885435) _(don't forget to commit your changes first)_.

```none
cd into/cloned/fork-repo
git remote add upstream git://github.com/ORIGINAL-DEV-USERNAME/REPO-YOU-FORKED-FROM.git
git fetch upstream
git pull upstream master
git push
```

If you're a fan of [git aliases](https://grantwinney.com/what-is-a-git-alias-and-how-do-i-use-it/), after you've run the above once you could add something like this to your .gitconfig file:

```none
updatecasper = !cd your/casper/location && git fetch upstream && git pull upstream master && git push
```

**NOTE:**

This method of updating my local fork worked perfectly fine for me, but you may also want to checkout the [official doc on Syncing a fork](https://help.github.com/articles/syncing-a-fork/) which suggests a `git merge` instead of a `git pull`. There's also a [comment](https://gist.github.com/CristinaSolana/1885435#gistcomment-2114661) under the gist I linked to above, that suggests a way to keep forks up-to-date all through GitHub.