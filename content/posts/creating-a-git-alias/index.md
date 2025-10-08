---
categories:
  - Learn
date: 2017-01-28T13:07:51Z
description: ""
draft: false
postimage: /banners/default-learn-banner.webp
slug: creating-a-git-alias
summary: If you're unfamiliar with Git's "alias" feature, it provides a way to create shortcuts for other Git commands, which can save you a lot of time. They’re easy to setup and maintain too. Let's see how.
tags:
  - git
title: How to Create a Git Alias
aliases:
  - /what-is-a-git-alias-and-how-do-i-use-it
---
If you're unfamiliar with Git's "alias" feature, it provides a way to easily create shortcuts for other Git commands. It can save a _lot_ of time over calling some lengthy command that's tough to remember.. or even shorter ones that are used frequently.

## Using Aliases for Shortcuts

We can create aliases for short commands, like assigning "checkout" to "co":

```bash
git config --global alias.co checkout
```

Or longer commands, like this one that displays a unique log view:

```bash
git config --global alias.hist "log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short"
```

And what if we're the sole developer of some project and want to add, commit and push files in one go?

```bash
git config alias.shove '!git add . && git commit --allow-empty-message -m "" && git push'
```

I’d recommend using that last one only on repos you maintain by yourself, since you’ll want to be more careful on team projects. That’s why I omitted `--global`.

Alternatively, we could add a `$1` place-holder to force a commit message. _(The final colon is a bit of a hack, but_ [_there’s a reason for it_](http://stackoverflow.com/a/25915221/301857)_.)_

```bash
git config --global alias.cshove '!git add . && git commit -m "$1" && git push && :'
```

Any aliases we configure are added into:

- the global `.gitconfig` file if we use `--global`, or
- a single repository’s `.git/config` file if we use `--local` or just omit the modifier _(_[_learn more here_](http://stackoverflow.com/a/2115116/301857)_)_

Here’s what it looks like in the config file itself. The `git config alias` line above is just for convenience, but we can edit the file manually too.

```none
[alias]
    co = checkout
    hist = log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short
    shove = !git add . && git commit --allow-empty-message -m \"\" && git push
    cshove = !git add . && git commit -m \"$1\" && git push && :
```

When we define aliases, we use them like any other git command, i.e. by calling `git shove`.

## Using Aliases for Typos

There’s another good use for aliases too – fixing common typos.

By default, Git suggests corrections when it catches a typo, but takes no further action. Optionally, we can tell it to just run the autocorrected command when it has a single suggestion:

```none
> git config --global help.autocorrect 30
 
> git pulll
WARNING: You called a Git command named 'pulll', which does not exist.
Continuing under the assumption that you meant 'pull'
in 3.0 seconds automatically...
Already up-to-date.
```

If we don't want to do that though, which could be a dangerous move, especially if the time is set so low it can't be canceled in time, we can setup aliases for our own common typos.

```none
> git config --global alias.pul pull
> git pul
Already up-to-date.
 
> git config --global alias.puhs push
> git puhs
Everything up-to-date
```

Here we've told Git to run `git pull` when we type `git pul` by accident, and the same with `git push` and the typo `git puhs`.

## What’s Next?

I’ve added a few aliases _I_ find useful to my [BlogCodeSamples repo](https://github.com/grantwinney/BlogCodeSamples/tree/master/DevTools/GitAliasTemplate), so I don’t lose them. Maybe you’ll find them useful too...

To use them, clone the above repo or just copy the two `.gitconfig` files somewhere locally. You can reference external files from within your own `.gitconfig` file, which leaves your existing aliases and other settings untouched. Add the following section to your `.gitconfig` file, where `/your/path` is wherever you copied the files to.

```none
[include]
    path = "/your/path/git-alias-template/alias-shortcuts.gitconfig"
    path = "/your/path/git-alias-template/alias-typos.gitconfig"
    # etc...
```

The `include` section [imports settings from other files](https://git-scm.com/docs/git-config#_includes) and merges their functionality without affecting the original config file. You can read more about aliases at [GitHowTo: Aliases](https://githowto.com/aliases) or [Git Basics – Git Aliases](https://git-scm.com/book/en/v2/Git-Basics-Git-Aliases).

If you have any helpful aliases of your own, feel free to share below! And if you’d like to learn even more interesting features of Git, you may want to check out the [GitHub Cheat Sheet](https://github.com/tiimgreen/github-cheat-sheet).
