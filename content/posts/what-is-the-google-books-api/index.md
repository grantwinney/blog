---
categories:
- API
- Google
date: "2018-01-31T00:07:00Z"
description: ""
draft: false
cover:
  image: photo-1517770413964-df8ca61194a6.jpg
slug: what-is-the-google-books-api
summary: The Google Books API provides access to Google Books, which lets you search
  for any book and, at a bare minimium, see meta data about it. Depending on copyright
  status, you might also be able to see sample pages or read the entire book. You
  can also buy books.
tags:
- API
- Google
title: Manage your books with the Google Books API
---


There's a staggering amount of data out there - and a lot of it free - but accessing it isn't always easy. A good API hides the complexities of accessing that data, and can save you a ton of development time too. After writing about 15 APIs in 15 days over the holidays, I've decided to find a different API to write about every Monday (okay, so I'm a day late this week...).

Two things before you get started:

 * If you're unfamiliar with APIs, you might want to read this first to familiarize yourself with them.
 * You may want to install Postman, which allows you to access API endpoints without having to write an app, as well as save the calls you make and sync them online between your computers.


What does it provide?

The Google Books API provides access to.. well... Google Books. Google Books lets you search for any book and, at a bare minimium, see meta data about it. Depending on copyright status or permission from the author, you might also be able to see sample pages or read the entire book. You can also buy books through their service.

Google Books has been around for over 15 years. Although the progress has apparently slowed down a lot in the last few years, amid legal battles and newer goals, it's still available... for now. Here's an interesting article about its history: How Google Book Search Got Lost


Taking it out for a spin

The easiest way to try it out is to use the APIs Explorer tool they built on top of their own API. In order to access someone's private data, you'll need to have your app request access to their account. Similarly, you're the user of the APIs Explorer, so it'll request access to your account.

But first, a few others things you'll need to do to get ready first.


Create an account

Sign in to your existing Google account, or create a new test account if you like to keep things separate. I'm using my normal Google account and it's working fine. YMMV.


Find your user id

It's easy to find your ID, even if it's not that obvious. According to the documentation on IDs:

The only way retrieve the user ID is to extract it from the selfLink in a Bookshelf resource retrieved with an authenticated request. Users can also obtain their own user ID from the Books site.

Go to the Google Books site and click on "My Library". Look in the address bar. You should see a URL like the following. Just note the ID.

https://books.google.com/books?uid=12341234123412341234



Add a few books

You should still be in your library. If you already have books available in your library, awesome. Otherwise, search for something free like Alice in Wonderland or Twenty Thousand Leagues Under the Sea, and click the "Add to my library" button at the top of the page.


Create a bookshelf

In your library, look for the red "NEW SHELF" button on the left. Click that and create a new shelf. Name it whatever you want, then select it from the list to open it up. I chose to make it private, but whatever.




Note the URL. The bookshelf has an ID too, for example 9999 in the following example, so make a note of it too for later.

https://books.google.com/books?uid=12341234123412341234&as_coll=9999&source=gbs_lp_bookshelf_list


Look for the "Search My Library" box on the left. Search for those titles you added earlier, one at a time. Hover over the "Add to my library" button in the search results and select your new shelf to add them to it.


Authorize the APIs Explorer app to access your account

Like I said earlier, you need to grant the app authorization to access your data, just like you would anything else. Go back to the API endpoints page.

Click the "OFF" button next to "Authorize requests using OAuth 2.0" in the upper-right corner of the page. The APIs Explorer needs access to manage your books (obviously), so select the checkbox and press "Authorize". You should get a popup - select the Google account where you've got your books saved, and then click "Allow".

You should be back where you started on the APIs Explorer page, but now the gray "OFF" toggle button should be a blue "ON" button. If you wait too long, you'll need to go through this process again. The "authorization" you grant only seems to last about a day, maybe less.


FINALLY! Let's ask for metadata about our new shelf

Go to books.bookshelves.get, and fill in the userId and shelf fields using the two IDs you grabbed earlier. This endpoint lists metadata about the shelf you created - in my case, I had only added one book when I called it.

Note that they provide you with the REST endpoint call, with your values in it. You could test things out in the APIs Explorer, then copy the call into your app. Convenient.


Now let's ask for metadata about ALL our shelves

Click on books.bookshelves.list, and fill in your userId again. This endpoint lists metadata about all of your shelves, including:

 * Everything listed in your library on the left-hand side (the names vary slightly)
 * "My Google eBooks", which seems to correspond to "My Books on Google Play"
 * "Purchased", "Reviewed", and "Recently viewed", which don't show in the list at all

Since it includes IDs for everything, you could display a list like this to a user, let them select one, then use the ID for whatever you need. If you just created an account, or haven't used Google Books before, you may not have some of those shelves.


One more... detailed info on all volumes (books) in our shelf

Click on books.bookshelves.volumes.list, and fill in your userId and shelf IDs again. This endpoint lists detailed information about the volumes (books) in your shelf.

Note that the results include the ID for the book, like "Y7sOAAAAIAAJ" in the above results. You can use that ID with endpoints like books.volumes.get that expect a volumeId value.


Next Steps

When you're ready to starting creating an app that uses the API, you'll need to create an "application" in Google's system that represents the app you're building. Then you'll use that to request authorization to your users' accounts. Based on the sensitivity of the data you're requesting, you may need an OAuth 2.0 token (more secure) or an API key.

I don't intend to get into all the intricacies of that though - this took longer to setup and try out than I thought it would. Google has a decent Getting Started guide you should check out.

I did play with the API key a bit (I'll try OAuth 2.0 at some point). It requires you to:

 1. Create a bookshelf like I explained above (and note the bookshelf id, aka "as_coll")
 2. Go to https://books.google.com/books if you're not there already, find the bookshelf, click the gear and "edit properties", then set the visibility to "public".
 3. Go to https://console.developers.google.com/apis/library/books.googleapis.com and select a project at the top (or create a new one from the same screen), and enable the API.
 4. Go to https://console.developers.google.com/projectselector/apis/credentials, select your project from the drop-down, then "create credentials" and "api key" to create an API key for the request.
 5. Open Postman and do a GET https://www.googleapis.com/books/v1/users/<your-user-id>/bookshelves/<as-coll-id>?key=<api-key>

If you want to read about a real-life implementation of this API, here's an article I came across recently: Build a Best Sellers List with New York Times and Google Books API


Observations

Not too much else to say, except one thing I noticed in the terms of service. The terms dictate that you can't charge a fee for any app that uses their API service.

You may not charge users any fee for the use of your application, unless you have entered into a separate agreement with Google or obtained Google's written permission.

This seems silly to me, as (1) it's not like Google's motives are altruistic anyway - they sell books through the service, and (2) an app that uses their API ostensibly provides end-users with more features than just the API, and you should be free to charge for your time and service. Requiring you to publicize that you use the Google Books API would make more sense to me, as it's free advertising for them.

Then again, Google can't even keep its own store free of junk apps that most likely violate their own TOS, so the odds of them randomly catching your application seems pretty slim...
