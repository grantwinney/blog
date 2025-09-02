---
categories:
  - Tools
date: 2021-07-30T03:38:23Z
description: ""
draft: false
cover:
  image: photo-1508937199041-881f5b63be74.jpg
slug: how-to-move-a-subdirectory-of-one-repo-into-its-own-repository
summary: Need to pull a subdirectory out of repo A and create a new repo B out with it? Including full history and branches? Okay, here's how.
tags:
  - git
title: Move a subdirectory into its own Git repo, with history
---
I moved a project that was in a subdirectory of a larger, pretty much unrelated, repository. I don't know who thought it belonged there to begin with, but it was my first go at pulling something like that out into its own repo.

Everything below is for Windows, but the instructions shouldn't vary too much for other OS's. YMMV and all that.

1. Follow steps 1-4 in [GitHub's tutorial](https://docs.github.com/en/get-started/using-git/splitting-a-subfolder-out-into-a-new-repository), but pause there. The `filter-branch` command in step 5 must be pretty awful, because even the docs [advise you to use something else](https://git-scm.com/docs/git-filter-branch#_warning).. so we will.
2. Install [Python3](https://www.python.org/downloads/) if you don't already have it.
3. Copy the git-filter-repo file from [this repo](https://github.com/newren/git-filter-repo), and save it [somewhere in your path](https://helpdeskgeek.com/windows-10/add-windows-path-environment-variable/), so you can access it easily from the command line in like... 30 seconds.
4. According to the [notes for using it on Windows](https://github.com/newren/git-filter-repo/blob/main/INSTALL.md#notes-for-windows-users), you may have to change the first line of the file from `python3` to `python`. I did.
5. Run `git filter-repo --subdirectory-filter location/of/subfolder`. I had to use the `--force` command due to some error it was throwing about my repo not being a new clone, even though it was. Didn't harm anything though.
6. Pick back up with steps 6-10 in [GitHub's tutorial](https://docs.github.com/en/get-started/using-git/splitting-a-subfolder-out-into-a-new-repository). Step 8 returned nothing for me, and step 9 failed, so I ran `git remote add origin https://location_of_your_new_repo` instead, and then verified it with the command in step 10.
7. Instead of step 11, I ran `git push -u origin --all`.

The end result was a new repo, with the previous subdirectory as the root of the new repo, complete with all branches and history. Success! 🎉
