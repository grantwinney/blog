---
categories:
- Google
date: "2020-02-28T03:42:32Z"
description: ""
draft: false
cover:
  image: photo-1506784365847-bbad939e9335.jpg
  relative: true
slug: how-to-find-the-ical-address-for-a-public-google-calendar
summary: Every Google calendar URL has an iCal file you can use... here's how to find
  it.
tags:
- Google
title: How to find the iCal address for a public Google calendar
---
If you already know why you're here, then visit the CodePen link below, plug-in the public URL (or the calendar ID) from the calendar settings page, and click the appropriate button to get the iCal link.

[Find the iCal address for a public Google calendar](https://codepen.io/astrangegame/pen/azvazpJ)

For everyone else, scroll past the text boxes for a brief explanation...

## How's that work?

Impressive, you didn't just run off. This won't take too long, I promise.

When someone creates a public Google calendar for the whole world to use, you'll see a little "+ Google Calendar" button in the lower-right corner. Click on that, and you can import the calendar into your own Google account.

![](https://grantwinney.com/content/images/2020/02/google-calendar-html.png)

![](https://grantwinney.com/content/images/2020/02/google-calendar-add-prompt.png)

![](https://grantwinney.com/content/images/2020/02/google-calendar-calendar-added.png)

Unless you've replaced it with another service, like I did. 😐

If you don't _want_ to import a Google calendar into a Google account, you might see the "Public URL" and try to import that into another client. That's reasonable, but it won't work.

![](https://grantwinney.com/content/images/2020/02/google-calendar-settings.png)

![](https://grantwinney.com/content/images/2020/02/import-calendar-error.png)

The reason it can't be imported is that the Public URL from Google is really just a link to an HTML page, and other clients don't know what to do with it. You need something that's standardized, that all clients can easily consume and do something with, and that's an iCalendar file, which (if you open it up in a text editor) looks something like this:

```none
BEGIN:VCALENDAR
PRODID:-//Google Inc//Google Calendar 70.9054//EN
VERSION:2.0
CALSCALE:GREGORIAN
METHOD:PUBLISH
X-WR-TIMEZONE:UTC

BEGIN:VEVENT
DTSTART;VALUE=DATE:20210402
DTEND;VALUE=DATE:20210403
DTSTAMP:20200227T163343Z
UID:20210402_60o30dr5coo30c1g60o30dr56k@google.com
CLASS:PUBLIC
CREATED:20190517T221201Z
DESCRIPTION:Holiday or observance in: Connecticut\, Hawaii\, Delaware\, Ind
 iana\, Kentucky\, Louisiana\, New Jersey\, North Carolina\, North Dakota\, 
 Tennessee\, Texas
LAST-MODIFIED:20190517T221201Z
SEQUENCE:0
STATUS:CONFIRMED
SUMMARY:Good Friday (regional holiday)
TRANSP:TRANSPARENT
END:VEVENT

BEGIN:VEVENT
DTSTART;VALUE=DATE:20200410
DTEND;VALUE=DATE:20200411
DTSTAMP:20200227T163343Z
UID:20200410_60o30dr5coo30c1g60o30dr56g@google.com
CLASS:PUBLIC
CREATED:20190517T221201Z
DESCRIPTION:Holiday or observance in: Connecticut\, Hawaii\, Delaware\, Ind
 iana\, Kentucky\, Louisiana\, New Jersey\, North Carolina\, North Dakota\, 
 Tennessee\, Texas
LAST-MODIFIED:20190517T221201Z
SEQUENCE:0
STATUS:CONFIRMED
SUMMARY:Good Friday (regional holiday)
TRANSP:TRANSPARENT
END:VEVENT

...
...

BEGIN:VEVENT
DTSTART;VALUE=DATE:20191224
DTEND;VALUE=DATE:20191225
DTSTAMP:20200227T163343Z
UID:20191224_60o30dr56ko30c1g60o30dr56c@google.com
CLASS:PUBLIC
CREATED:20140108T163258Z
DESCRIPTION:
LAST-MODIFIED:20140108T163258Z
SEQUENCE:0
STATUS:CONFIRMED
SUMMARY:Christmas Eve
TRANSP:TRANSPARENT
END:VEVENT

BEGIN:VEVENT
DTSTART;VALUE=DATE:20191102
DTEND;VALUE=DATE:20191103
DTSTAMP:20200227T163343Z
UID:20191102_60o30c9g6ko30c1g60o30dr56c@google.com
CLASS:PUBLIC
CREATED:20140108T163258Z
DESCRIPTION:
LAST-MODIFIED:20140108T163258Z
SEQUENCE:0
STATUS:CONFIRMED
SUMMARY:All Souls' Day
TRANSP:TRANSPARENT
END:VEVENT

END:VCALENDAR
```

As luck would have it, you can easily extract the Calendar ID from any Google calendar URL (or just use the calendar ID directly if you know it), and replace `{CALENDAR_ID}` in the following URL. Or use the script I wrote at the top of this post.

```none
https://calendar.google.com/calendar/ical/{CALENDAR_ID}/public/basic.ics
```

That's it!
