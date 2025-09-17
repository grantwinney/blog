---
categories:
  - Build
date: 2018-01-22T03:41:27Z
description: ""
draft: false
cover:
  image:
slug: ghost-blog-autolink-images-to-full-resolution
tags:
  - ghost-blog
title: How to Autolink All Images in Ghost to the Full Resolution
---
## The Problem

When you insert an image into a post using the Ghost platform, it might be displayed as much smaller, depending on your theme. Viewing the full version means either opening the image in a new tab or linking the image to itself so that clicking on it opens it up outside of the context of the page. Either way, someone's doing manual work, so let's automate it!

## The Solution

Here's a snippet of JavaScript code you can insert under "Blog Header" in the "Code injection" section of your Ghost blog, or in a separate file if you're self-hosting and have access to where the themes are uploaded.

```javascript
<script type="text/javascript">
document.addEventListener("DOMContentLoaded", function(event) {
    document.querySelectorAll('.post-content img').forEach(function(image) {
        image.style.cursor = "zoom-in";
        if (image.title === '') {
            image.title += "(click to zoom)"
        } else {
            image.title += " (click to zoom)"
        }
        image.onclick = function(event) {
            window.location.href = image.src;
        };
    });
});
</script>
```

## What's it do?

This selects all image elements under the element that has the `.post-content` class applied to it, which is the main body of your post's content, and makes them clickable to display the full image. You probably don't want to apply this script to _every_ image on your site, but you can change change the selector as needed. Read more about [querySelectorAll](https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelectorAll) and [selectors](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors) at MDN.

It does this when the [DOMContentLoaded](https://developer.mozilla.org/en-US/docs/Web/Events/DOMContentLoaded) event fires, which occurs after the HTML is rendered but before any resources (images, stylesheets, etc) are downloaded, this code. That way, we can be sure the image placeholders are present, even if the images themselves are not yet.

It loops through each image, doing three things:

- The cursor is changed to "zoom-in", signaling that an action can be taken.
- The title is changed, again to signal that an action can be taken.
- A click event is assigned, that navigates to the image's source.