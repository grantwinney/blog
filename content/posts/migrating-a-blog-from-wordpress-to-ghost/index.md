---
categories:
- Ghost Blog
date: "2017-03-26T02:43:14Z"
description: ""
draft: false
cover:
  image: photo-1476283721796-dd935b062838.jpg
slug: migrating-a-blog-from-wordpress-to-ghost
tags:
- Ghost Blog
title: Migrating a Blog from WordPress to Ghost
---
About a week ago I decided to migrate my blog to the Ghost platform. I'd been thinking about it for awhile - even installed it once or twice to play around with it - but never fully committed. Truth is, I didn't really __want__ to switch. I knew that, however little, the process would certainly be more painful than doing nothing. So the pain of going through the switch had to be outweighed by the pain of __not__ switching. I guess that finally happened.

WordPress can do pretty much anything thanks to plugins, but that's its weakness too... __especially__ if all you want to do is blog. Over the last few years I've had to upgrade memory multiple times and setup disk swap space, slog through plugins to determine which were good and then keep them up-to-date, find workarounds for [unexpectedly dropped themes](https://wptavern.com/zerif-lite-suspended-from-wordpress-theme-directory-300k-users-left-without-updates), handle weird issues like the [white screen of death](https://codex.wordpress.org/Common_WordPress_Errors), deal with a busy interface that (despite their efforts) just leaves too much distraction on the screen while I try to write. There's just too much overhead. I don't __want__ to spend so much of my free time maintaining a platform...

__I want it to get out of my way and just let me write!__

And so here I am, writing my first post on the Ghost platform. Does any of this sound familiar to you? If you're looking to just get back to the basics, and you want to try switching from WordPress to Ghost, then read on...

## What is this tutorial (and what is it not)?

Let me set the expectations right now... I got it to all work but I'm no expert. This is my personal experience migrating (one time) from a _****self-hosted WordPress****_ installation to a _****self-hosted Ghost****_ installation, using _****DigitalOcean****_ which has a "one-click install" process for both platforms.

A few things to consider:

First, if you've been using a __managed__ WordPress installation hosted on wordpress.com, then that's pretty restricted. You don't have the ability to install plugins or directly access the disk to get your images. Your data is obviously all accessible or visitors wouldn't be able to view it, but getting to that data won't be straight-forward. You may have to contact support, but I'm not sure how easy they make it to leave their platform.

Second, DigitalOcean has their own page on [how to use install and use Ghost](https://www.digitalocean.com/community/tutorials/how-to-use-the-digitalocean-ghost-application), and Ghost has documentation on [migrating from WordPress](https://www.ghostforbeginners.com/migrating-your-wordpress-blog-to-ghost), so you may want to check those out first. Who knows, that might be all you need.

Finally, if you're just looking to have someone host and maintain Ghost for you, look no further than Ghost(Pro). [For $20/month they do everything for you](https://ghost.org/pricing/)... hosting, backups, security, letting you modify the theme and use their API to interact with your site. You still need to download the data out of WordPress to give to them __(step 1 below),__ and I'm unclear whether they give you easy access to your data once it's uploaded, but it seems like a great deal and one I may consider in the future.

## Step 1: Get your data out of WordPress

You don't want to lose those old posts! The folks at Ghost wrote up some [instructions for migrating posts from WordPress](https://help.ghost.org/hc/en-us/articles/225093168-Migrating-From-WordPress-to-Ghost), and it involves [their very own WordPress plugin](https://wordpress.org/plugins/ghost/) that does some of the work for you. It runs through your content, exporting posts, pages and tags to a file (in JSON format) which you then download and import into Ghost. It has some issues that I'll cover later, but it gets the job done.

_****Note 1:****_ If you have a lot of drafts without titles, give them something now. Otherwise the import may fail later until you edit the JSON file and add titles manually. (Alternatively, move them to trash temporarily, which the plugin seems to ignore, and then copy/paste them into the new blog later on.)

_****Note 2:****_ I ran into a timeout issue running their plugin. After 30 seconds, it'd timeout with a 500 server error. To fix it, I had to [increase the php timeout](http://www.clickonf5.org/11921/solution-for-wordpress-php-error-maximum-execution-time-of-30-seconds-exceeded/) to 120 seconds and run it again. The first suggested fix worked fine - you can find the `wp-config.php` file in the root of your blog installation, i.e: `/var/www/wp-config.php`

Now you've got your textual data, but you still need to get your images. To do that, you'll have to [setup SSH access to your server](https://www.digitalocean.com/community/tutorials/how-to-connect-to-your-droplet-with-ssh) __(you may find__ [__this__](https://www.digitalocean.com/community/tutorials/how-to-use-ssh-keys-with-digitalocean-droplets) __and__ [__this__](https://www.digitalocean.com/community/tutorials/how-to-use-ssh-keys-with-putty-on-digitalocean-droplets-windows-users) __helpful as well).__ Once you do, the images are in the `/var/www/wp-content/uploads` folder, organized by year. Just [download the whole directory using sftp](https://www.digitalocean.com/community/tutorials/how-to-use-sftp-to-securely-transfer-files-with-a-remote-server).

Another option (and the one I used) is to setup the [UpdraftPlus Backup plugin](https://wordpress.org/plugins/updraftplus/) in WordPress. I was already using it to backup my site to a Google Drive account once a week so I just manually kicked off the job. Then it was as simple as downloading the file from Google Drive and unzipping it to get all my images.

## Step 2: Find some place to call home

I mentioned the easy route - Ghost(Pro) - but I'm trying to minimize costs so I went the self-hosting way. [I've been using DigitalOcean for several years](https://m.do.co/c/448f25462030) so I stuck with them - they even provide a one-click droplet for Ghost that makes it super-easy to get up and running with the latest version. Press the "Create Droplet" button and select Ghost from the "One-click apps". This is a huge convenience over [deploying Ghost yourself](http://docs.ghost.org/pl/installation/deploy/).

![](https://grantwinney.com/content/images/2017/03/ghost_install_1.png)

I selected the $10/month 1GB plan and Ghost seems plenty responsive - __much__ quicker than WordPress was. You might consider selecting "Backups" under "Additional Options" too, especially if you find yourself mucking with the server configurations at all. For $2/month they take a complete snapshot of your entire server every week; restoring it is as easy as one button press.

If you don't currently have one, purchase a domain name and configure it to point at your droplet. [I've had great luck with Namecheap](https://affiliate.namecheap.com/?affId=113268). If you do already have one, don't switch it yet because your visitors will hit a bunch of 404 pages until your content is uploaded.

When you [SSH into the server](https://www.digitalocean.com/community/tutorials/how-to-connect-to-your-droplet-with-ssh), it'll tell you a few things that have been setup by default:

- The "ufw" firewall is enabled, blocking all ports except 80 and 443 (http and https) and 22 (SSH and SFTP). That's good.
- "Let's Encrypt" has been pre-installed. More on that later, but you'll want to set that up and enable HTTPS.
- Ghost is configured to use MySQL. You can run `mysql_secure_installation` to ready your server for production use. I won't cover that here, but it's something to note.

## Step 3: Clean up the exported file

In the [instructions for migrating posts](https://help.ghost.org/hc/en-us/articles/225093168-Migrating-From-WordPress-to-Ghost), they warn you that the image directory in Ghost is similar but not identical. Open the JSON file you downloaded (in an editor like Atom or Notepad++), and take note of the various images paths in your posts. The date portion of the path should be fine, but `/wp-uploads/` needs to be replaced with `/content/images/`.

You might also want to do a search for your previous host's IP address. You can leave the domain name alone, but if the IP address is in the JSON file anywhere you'll want to replace it with the IP address of your new site __(which you'll eventually replace with the domain name, but not yet)__.

## Step 4: First time setup

Before I could access the admin area of Ghost, I had to open `http://<your-ip-address>/ghost/signup` and create a new account. From there, I opened the "Labs" section and attempted to import the file, but it kept failing. It took awhile to realize that the list of messages was __not__ of successfully imported files but exactly the opposite - a list of failures and the reason(s) why. Several of my draft posts were missing titles and one had a missing date.

![](https://grantwinney.com/content/images/2017/03/ghost-import-failed-missing-date.png)

Here are [common import problems and solutions](https://help.ghost.org/hc/en-us/articles/226670028). You might find it easier to run the contents of the JSON file through a JSON formatter first, like [this one](https://atom.io/packages/pretty-json) for Atom. It makes it easier to read, and it'll still import into Ghost just fine. Eventually, everything should import without errors.

Next, upload the images to the `/content/images/` directory [using the sftp command](https://www.digitalocean.com/community/tutorials/how-to-use-sftp-to-securely-transfer-files-with-a-remote-server), and change the owner to `ghost`. Something like this should work nicely:

```bash
sftp -r root@<your-ip-address>
cd /var/www/ghost/content/images
put /local/path/to/your/images/uploads/*
chown -R ghost:ghost /var/www/ghost/content/images
```

If the posts and images have been correctly uploaded, you should be able to open a few posts and verify that they look okay and the images load correctly (you may have to change image links to your new site's IP address if you haven't assigned a domain name yet.)*

## Step 5: Assign the domain name

If you're migrating from your old blog, it's time to point your domain name to the Ghost installation. Usually, this means going into your domain name provider's settings and changing the Type A record to point at your new site's IP address. Now your site is accessible via HTTP, but I'd highly recommend enabling (and enforcing) HTTPS - you'll be able to login securely, and [Google is HTTPS](https://motherboard.vice.com/en_us/article/google-will-soon-shame-all-websites-that-are-unencrypted-chrome-https) both as a ranking factor in their search engine and eventually as the default for Chrome.

## Step 6: Setting up HTTPS

It used to cost money, but thanks to [Let's Encrypt](https://letsencrypt.org/) it's now free for everyone. It's sponsored by some major players in the tech industry including Mozilla, Google and the [EFF](https://www.eff.org/). I found the article "[Initial Server Setup with Ubuntu 16.04](https://www.digitalocean.com/community/tutorials/initial-server-setup-with-ubuntu-16-04)" helpful in configuring my server and setting up proper access - at least the first four steps.

DigitalOcean has an article on [how to secure Apache with Let's Encrypt](https://www.digitalocean.com/community/tutorials/how-to-secure-apache-with-let-s-encrypt-on-ubuntu-16-04), but I found a really nice writeup by Robert Nealan: "[Setting up SSL for Ghost on DigitalOcean with Let's Encrypt](https://www.robertnealan.com/setting-up-ssl-for-ghost-on-digitalocean-with-lets-encrypt/)". It worked great, and even showed how to setup CRON to automatically renew the certificate. __(A downside of Let's Encrypt is that the license must be renewed every 90 days, which makes sense but is still a pain.)__

_****Note:****_ If you follow the tutorial by Robert Nealan, when you get to the step where you're uncommenting out the new nginx config lines (currently step 8 in his instructions), leave the following lines commented out (or just remove them). They'll break Ghost's ability to serve up the `sitemap.xml` and `robots.txt` files, both of which are useful.

```none
#        location ~ ^/(sitemap.xml|robots.txt) {
#                root /var/www/ghost/public;
#        }
```

## Step 7: Second clean up and re-importing posts

If you've gotten this far, you may want to re-import your posts. Open that JSON file again and replace any occurrences of your site's IP address to your domain name. And if the domain name was previously using the insecure `http://`, you'll want to update all of those to `https://`. The previous instructions showed you how to redirect all `http` requests to `https`, so technically things should work just fine as-is, but those redirects cause a small amount of overhead and get you dinged on sites that analyze your website.

There are a couple ways you could approach this. One way is to do what I did. Go to the "Labs" panel and press "DELETE" to delete all your posts. It'll leave your images and other settings alone. After you've updated the JSON file you got from the WordPress plugin, just re-import it and everything should be back and good to go.

![](https://grantwinney.com/content/images/2017/03/labs-settings-ghost.png)

The other option, which I didn't try but you may want to if you've made changes to your posts after you uploaded them into Ghost the first time, is to click the "EXPORT" button on that same screen to export your posts from Ghost. Update __that__ file and then delete all your content and re-import the updated file.

Either way, verify you can still login and your posts are accessible and look okay. Assuming you had WordPress setup to use __only__ your post title as the URL, which is also the default in Ghost, then everything should remain accessible in search engines and no one will be the wiser. Otherwise, you may have to navigate to the "General" panel and select "Include the date in your post URLs" under "Dated Permalinks". I'm not sure what that format looks like, but if it's still not the same as your old site you'll need to look at how to configure your server to direct requests for old paths to the new path. That's called doing a 301 permanent redirect, and will let search engines know to update their references to your posts. You don't want to lose traffic! I won't go into all that here, but [this article explains more](https://www.digitalocean.com/community/tutorials/how-to-create-temporary-and-permanent-redirects-with-apache-and-nginx).

## Step 8: A little more cleanup (maybe)

If that works, it's time to check each of your posts for formatting issues. The Ghost plugin for exporting your WordPress posts has a few issues. For one thing it messes with some formatting, especially italics. It converted `<em>one</em> two` to `*one *two` all over, which wreaked havoc with the formatting of my posts.

Also, if you use a plugin that renders extra html output with css styling and such, like [Table of Contents Plus](https://wordpress.org/plugins/table-of-contents-plus/), it tries to convert the rendered output of that plugin but it doesn't do a great job. There may be some other oddities to clean up too, so you might as well take care of those now.

## Step 9: Configuring Disqus

We're getting into the optional stuff now.

Most people want to encourage discussion on their blogs. There's no built-in commenting engine like WordPress has, but Disqus is popular and it integrates quickly and cleanly. Signup for an account and navigate to the install page for Ghost to get the embed code.

`https://<your-user-name>.disqus.com/admin/install/platforms/ghost/`

Although Disqus has instructions on where to place the code, [Ghost has their own instructions on enabling comments](https://www.ghostforbeginners.com/how-to-enable-comments-on-a-ghost-blog/) that differ so I followed those.

There are a couple of variables in the code they provide (commented out by default) that apparently should be changed __(__[__read more here__](https://help.disqus.com/customer/en/portal/articles/2158629)__),__ but they only tell you to replace `PAGE_IDENTIFIER` with `{{post.id}}`. I think the `PAGE_URL` placeholder should be replaced with the unique url for the post, but since I'm not sure which variable holds that I just left them both commented out for now. It seems to work fine, and correctly loads existing comments from my old blog.

After you modify the file, run `service ghost restart` from the command line (your SSH session) and wait a few seconds. Check out a post and make sure the Disqus commenting system loads at the bottom.

## Step 10: Syntax Highlighting

If you're frequently posting code snippets on your blog you'll want something that can format it nicely. You can always enclose code in three backticks (` ``` `) but Ghost doesn't have syntax highlighting built-in by default. Which is good, because many bloggers won't need it and it'd be unnecessary bloat.

There's a couple of javascript libraries you can use - I chose [Prism.js](http://prismjs.com). It's awesome in that you choose exactly which languages (such as c# or perl) and plugins (such as showing line numbers) you want to have, and it provides the minimal amount of javascript and css for you to copy into your site.

Once you've downloaded the `prism.js` and `prism.css` files, use SFTP to upload them to your server in the `themes/casper/assets` directory. Modify the `default.hbs` file in the Casper theme to reference both files in the section (near the top) that indicates it's for scripts and styles. Follow the existing format of using `{{assets}}` in the links... I think it helps with caching resources or something.

```none
{{!-- Styles'n'Scripts --}}
<link rel="stylesheet" type="text/css" href="{{asset "mine/prism.css"}}" />
<script type="text/javascript" src="{{asset "mine/prism.js"}}"></script>
```

After including the links, restart Ghost again (`service ghost restart`) for the changes to take effect. If you need to support additional languages, both files have a custom link on the first line that includes all your selected languages and plugins. Using that link will preselect your current selections, so you can add what you want and then overwrite the files. Here's a sample URL:

> [http://prismjs.com/download.html?themes=prism-coy&languages=csharp+ruby&plugins=line-numbers](http://prismjs.com/download.html?themes=prism-coy&languages=csharp+ruby&plugins=line-numbers)

Another nice-looking library is [highlight.js](https://highlightjs.org/). It has the added benefit of providing a single link hosted out on a CDN that provides something like 22 languages out of the box, so there's absolutely nothing to install except referencing that link. If enough other blogs are using that same CDN resource, your visitors may have already downloaded and cached it which makes your site load faster. If the languages you need are in those 22, that's the way to go.

## Step 11: Google analytics

Some people like to track traffic, and Google analytics can help you do that. You'll [find instructions here](https://support.google.com/analytics/answer/1008080?hl=en) on how to get the analytics script, which you can then copy into the footer of your blog thanks to the "Code Injection" section of the admin panel.

Bing has its own set of [Webmaster Tools](http://www.bing.com/toolbox/webmaster/) as well. You can get a link to include in the header of your blog via the same "Code Injection" section.

## Further Reading

This post has gone on longer than I intended. Here are some other links you may find interesting too...

- [Using MailChimp with Ghost](http://academy.ghost.org/how-to-use-mailchimp-with-your-ghost-blog/)
- [Ghost vs WordPress](https://ghost.org/vs/wordpress/)
- [SSL Setup with Cloudflare](https://help.ghost.org/hc/en-us/articles/223238128-SSL-Setup-with-CloudFlare)

Nothing left to do now but to get blogging!