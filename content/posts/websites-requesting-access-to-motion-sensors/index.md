+++
categories = ["Privacy", "Bots", "Code Review"]
date = 2019-12-30T17:49:59Z
description = ""
draft = false
image = "https://images.unsplash.com/photo-1487893667092-772cdd6fe0ec?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=2000&fit=max&ixid=eyJhcHBfaWQiOjExNzczfQ"
slug = "websites-requesting-access-to-motion-sensors"
summary = "I was checking the status of a FedEx order when Brave warned me that \"this site has been blocked from accessing your motion sensors\". I'm struggling to understand why a website would need that access. Do I get a different experience if I drop my device? Tip my monitor over? Spin the mouse around?"
tags = ["Privacy", "Bots", "Code Review"]
title = "Why are websites requesting access to motion sensors... on my desktop?"

+++


I was checking the status of a FedEx order in Brave, when I noticed a notification in the address bar that I've never seen before. It was warning me that "this site has been blocked from accessing your motion sensors". Wut? It doesn't even need to be an order status - their home page kicks it up too.

I'm struggling to understand why a website would need access to a motion sensor on a mobile device, let alone the fact I was using a desktop. Do I get a different experience if I knock my PC off the desk? Tip my monitor on its side? Grab the mouse cord and spin it around my head really fast?

After a few cursory online searches, I'm coming up with little other than a few threads on Reddit and Brave that indicate people are also seeing this on Kayo Sports and Twitch, as well as Experian and Tutanota.

Guess it's time to dig a little deeper.


What are Web APIs?

Before zeroing in on sensors, let's backup a sec and talk about web design and Web APIs. Your browser has access to a lot of data via (and metadata regarding) the device you installed it on. As much as some of the websites you visit would looove to have access to all that data, any decent browser acts as a firewall, blocking that access by default and prompting you to allow it.


Geolocation API

One of the more common APIs is the one used to request your location, usually when you're using a websites's "store locator" to find the store nearest you.

The button below uses code (lightly modified) from MDN's Geolocation API docs. When you click it, the JavaScript code executes a call to navigator.geolocation.getCurrentPosition(), asking the browser for your location.



Show my location   







Your browser prompts you to allow access, which you can deny. Yay privacy.

If you don't see the prompt but you think you've allowed it, there are two different settings that control access - a global page with a list of "blocked" and "allowed" sites, and a per-site page where you can adjust all permissions for a single site. In Chrome, just replace brave:// with chrome:// in the address bar.


Notifications API

Another (unfortunately, very) popular API is the one used to display notifications to visitors. Using the Notifications API, you can request permission from a visitor with a call to Notification.requestPermission() and then just create a new Notification() to annoy them keep them up to date. (May not work in Brave due to a bug.)



Notify me!






Sensors API

There's a (maybe sorta?) new API for requesting access to sensors in Chromium-based browsers (Ghacks puts it at Chrome 75, around June 2019, but wikipedia suggests Chrome 67 around May 2018). It's not widely supported yet. According to MDN, the only major browsers that currently support it are Chrome and Opera, on desktop and mobile.

Check out the MDN docs, the W3C candidate recommendation, the ongoing conversation over at Chrome, and Intel's Sensor API playground for examples.

The following links execute some JavaScript code to try starting up various sensors, which should trigger the sensor icon in the address bar. (If an error occurs, it'll display below the links.)



 * Try Accelerometer
 * Try AmbientLightSensor
 * Try Gyroscope
 * Try LinearAccelerationSensor
 * Try Magnetometer


 * Try AbsoluteOrientationSensor (accelerometer, gyroscope, magnetometer)
 * Try RelativeOrientationSensor (accelerometer, gyroscope)



Status Message: N/A





As with the geolocation and notification APIs, you can grant or deny access at the global or per-site level. What's kind of annoying is that all of the above sensors fall under a single "motion sensors" umbrella, so you can't easily tell which of those sensors a particular site is trying to access.


Why are sites requesting the Sensors API?

I've seen the sensor request on several sites, and others have reported more - FedEx, Lowes, Kayo Sports, Hotels.com, Anthem, Pizza Hut... to name a few. Why would sites as varied as those need access to a gyroscope or accelerometer? Like all modern development, websites are built upon layers and layers of libraries. Are they using the same one? Is some library several layers deep requesting access to an API it doesn't need?

I think I've figured it out, but if you find something to the contrary, do share. 🧐


An obfuscated file, from Akamai

All the pages I've checked out have a reference to an obfuscated file that, when removed, makes the motion sensor icon go away. The name is a 112 bit random value that offers no clues, but differs for each site, so it probably doubles as a unique identifier or account id.

 * Lowe's: c45ff2fedf18894428b6eae366abf1
 * FedEx: b6c65804238fde1fae4a597ae052
 * Anthem: c3ce05c96199f8c080a174ece11ff
 * ... and so on.

A look at the markup for the page shows it loads the script right before the end of the page, and it looks nearly identical in all cases.

Lowe's

<noscript>
    <img src="https://www.lowes.com/akam/11/pixel_49faa00b?a=dD1jMjczMGNkMmRlNmY0NDYwY2Q5MzQ2ZGVjNWI5YWIwZjEwZDM2Nzg0JmpzPW9mZg==" style="visibility: hidden; position: absolute; left: -999px; top: -999px;" />
</noscript>
<script type="text/javascript" >
    var _cf = _cf || []; 
    _cf.push(['_setFsp', true]);  
    _cf.push(['_setBm', true]); 
    _cf.push(['_setAu', '/resources/c45ff2fedf18894428b6eae366abf1']); 
</script>
<script type="text/javascript"  src="/resources/c45ff2fedf18894428b6eae366abf1"></script>

FedEx

<noscript>
    <img src="https://www.fedex.com/akam/11/pixel_190b4e7f?a=dD05YmZjNzQ1Njc1YTU3MDA5OWY0MDFiYjRmOWU3YTJhMzJjNjljNjdlJmpzPW9mZg==" style="visibility: hidden; position: absolute; left: -999px; top: -999px;" />
</noscript>
<script type="text/javascript" >
    var _cf = _cf || []; 
    _cf.push(['_setFsp', true]);  
    _cf.push(['_setBm', true]); 
    _cf.push(['_setAu', '/assets/b6c65804238fde1fae4a597ae052']); 
</script>
<script type="text/javascript"  src="/assets/b6c65804238fde1fae4a597ae052"></script>

Anthem

<noscript>
    <img src="https://www.anthem.com/akam/11/pixel_31f28831?a=dD04MTAwN2I4YzhlYmNjYjUzYTNjMzA2OTIyMjllNjYzOTRhYjRjNzFiJmpzPW9mZg==" style="visibility: hidden; position: absolute; left: -999px; top: -999px;" />
</noscript>
<script type="text/javascript" >
    var _cf = _cf || []; 
    _cf.push(['_setFsp', true]);  
    _cf.push(['_setBm', true]); 
    _cf.push(['_setAu', '/public/c3ce05c96199f8c080a174ece11ff']); 
</script>
<script type="text/javascript"  src="/public/c3ce05c96199f8c080a174ece11ff"></script>

Hotels.com

<script type="text/javascript" >
    var _cf = _cf || []; 
    _cf.push(['_setFsp', true]);  
    _cf.push(['_setBm', true]); 
    _cf.push(['_setAu', '/assets/0997d10d16655fda9826ab5d88ea']);
</script>
<script type="text/javascript"  src="/assets/0997d10d16655fda9826ab5d88ea"></script>

Kayo Sports

<noscript>
    <img src="https://kayosports.com.au/akam/11/pixel_52bf70fb?a=dD05ZWY4MzI1MTFjODFmMGFmYzhkZmUzNzhkZWNmM2RiZjVmYzc5ZWVhJmpzPW9mZg==" style="visibility: hidden; position: absolute; left: -999px; top: -999px;" />
</noscript>
<script type="text/javascript" >
    var _cf = _cf || []; 
    _cf.push(['_setFsp', true]);  
    _cf.push(['_setBm', true]); 
    _cf.push(['_setAu', '/assets/662c194b202d4b929be8d06c3195']); </script>
<script type="text/javascript"  src="/assets/662c194b202d4b929be8d06c3195"></script>

Since 4 of the 5 sites included a call to a URL with "akam/11/pixel" in it immediately prior, I assume it's related.. possibly some kind of tracking pixel (one of the reasons your email provider blocks images by default). A search of akam/11/pixel (short for Akamai?) turns up loads of other sites that all cause the sensor icon to display too.


Deobfuscating it leads to code that suggests Akamai

The randomly named js file is always the same, but obfuscated. I was able to deobfuscate it a bit with an online tool and then further by scripting a search and replace with that _ac array that has 711 elements in it, but that only gets a person so far. Figuring out what this does would be a huge challenge (the reason for obfuscating a file in the first place), but searching for bits and pieces of code turned up a couple threads suggesting it's the Akamai bot detection service.

The values in the _ac array might have some clues in it, and some of the entries sound suspicious. There's loads of references to various plugins, and no shortage of references to sensors (gyroscope, magnetometer, accelerometer and accelerationIncludingGravity, ambient-light-sensor, rotationRate, deviceorientation and DeviceOrientationEvent, DeviceMotionEvent, and sensor_data), and other odd stuff (startTracking and requestWakeLock).





Any requests to use those sensors, or even a check to see if a device supports them, would probably cause the sensor icon to show like it does, just like in the sensor examples I wrote up at the top of this post.

The next 30 lines after that one have some possibly-interesting stuff too. A version number, some counters, a URL for some kind of analytics, an api key, and something (maybe a flag?) called sensor_data that's set to 0.

var _cf = _cf || [],
    bmak = bmak || {
        ver: 1.54,
        ke_cnt_lmt: 150,
        mme_cnt_lmt: 100,
        mduce_cnt_lmt: 75,
        pme_cnt_lmt: 25,
        pduce_cnt_lmt: 25,
        tme_cnt_lmt: 25,
        tduce_cnt_lmt: 25,
        doe_cnt_lmt: 10,
        dme_cnt_lmt: 10,
        vc_cnt_lmt: 100,
        doa_throttle: 0,
        dma_throttle: 0,
        session_id: default_session,
        js_post: !1,
        loc: ,
        cf_url: (https: === document[location][protocol] ? https:// : http://) + apid.cformanalytics.com/api/v1/attempt,
        params_url: (https: === document[location][protocol] ? https:// : http://) + document[location][hostname] + /get_params,
        auth: ,
        api_public_key: afSbep8yjnZUjq3aL010jO15Sawj2VZfdYK8uY90uxq,
        aj_lmt_doact: 1,
        aj_lmt_dmact: 1,
        aj_lmt_tact: 1,
        ce_js_post: 0,
        init_time: 0,
        informinfo: ,
        prevfid: -1,
        fidcnt: 0,
        sensor_data: 0,

One function in the mess of code stood out. It seems to be hitting tons of permissions and sensors to see if the browser prompts you for each one, or simply grants or denies them without prompting. You can see from my example code near the top of this post that prompting a user for access is enough to show the icon, and that seems to be what this is doing.

np: function () {
    var a = [],
        t = [geolocation, notifications, push, midi, camera, microphone, speaker, device-info, background-sync, bluetooth, persistent-storage, ambient-light-sensor, accelerometer, gyroscope, magnetometer, clipboard, accessibility-events, clipboard-read, clipboard-write, payment-handler];
    try {
        if (!navigator[permissions]) return 6;
        var e = function (t, e) {
                return navigator[permissions][query]({
                    name: t
                })[then](function (t) {
                    switch (t[state]) {
                    case prompt:
                        a[e] = 1;
                        break;
                    case granted:
                        a[e] = 2;
                        break;
                    case denied:
                        a[e] = 0;
                        break;
                    default:
                        a[e] = 5
                    }
                })[catch](function (t) {
                    a[e] = -1 !== t[message][indexOf](is not a valid enum value of type PermissionName) ? 4 : 3
                })
            },
            n = t[map](function (a, t) {
                return e(a, t)
            });
        Promise[all](n)[then](function () {
            bmak[nav_perm] = a[join]()
        })
    } catch (a) {
        return 7
    }
},


A reference to cformanalytics.com, registered to Akamai

A quick whois on cformanalytics.com from the cf_url key above suggests it belongs to Akamai... they just keep coming up.

Domain Name: CFORMANALYTICS.COM
Registry Domain ID: 1897860898_DOMAIN_COM-VRSN
Registrar WHOIS Server: whois.akamai.com
Registrar URL: http://www.akamai.com
Updated Date: 2020-04-07T18:35:33Z
Creation Date: 2015-01-24T01:00:53Z
Registry Expiry Date: 2022-01-24T01:00:53Z
Registrar: Akamai Technologies, Inc.
Registrar IANA ID: 2480
Registrar Abuse Contact Email: registrar-abuse@akamai.com
Registrar Abuse Contact Phone: +1.6174443076


It seems safe to assume it's Akamai

At this point, I feel fairly confident it's Akamai's script, and it probably is some kind of bot detection service. I'm not sure why a bot detection service would need to check sensors, but maybe it's just one signal in a myriad of signals to detect if a requestor is a human or a bot? Or maybe it's being used as part of fingerprinting to track and individually identify visitors?

I spent a little time digging around the Akamai site, and while most of their documentation is locked behind having an actual account, I stumbled on this regarding their mobile device capabilities:

The Akamai Bot Manager Premier software development kit (BMP SDK) takes the fundamental technology of Akamai Bot Manager and applies it to native mobile apps. The BMP SDK collects behavioral data while the user is interacting with the application. This behavioral data, also known as sensor data, includes the device characteristics, device orientation, accelerometer data, touch events, etc. Akamai BMP SDK provides a simple API to detect bot activities and defend against malicious bot and account takeover.

It includes this graphic, which just seems to reinforce the above description, that their bot detection service(s) uses sensor data like accelerometer capabilities to determine whether a requestor is a bot or not.

I feel fairly confident that's the answer, but it leaves at least one more question...


But why do they expect desktops to have an accelerometer?

I think maybe it's an oversight, but I can't really prove it. Akamai has every incentive to keep their presence hidden. From how well they seem to have obfuscated their files, I think they'd agree. Since I can't prove anything, I'll try to make some educated guesses.


Guess 1: Their script doesn't handle responsive websites correctly

Most websites are responsive, meaning they use CSS to adjust dynamically for desktop devices with large screens, tablets with medium screens, and mobile devices with tiny screens. So unless Akamai detects the screen size and loads a different obfuscated file for each (and what about mobile users that choose the "view desktop site" option, or tablets with a very high resolution?), odds are the same script that runs on mobile devices is running on desktops.


Guess 2: They provide two scripts, but companies only implement one

Or perhaps their Akamai Bot Manager Premier SDK service that builds on the basic "Akamai Bot Manager" service is only meant to be loaded on mobile versions of sites, leaving the onus of implementation to individual sites. I can imagine most businesses, upon hearing that they have to implement two libraries, and then realizing that one is just a more enhanced version of the other, instructing the development team to just reference the "premier" SDK everywhere.

If anyone hears differently or knows more, please share in the comments. I've been updating this periodically over the last 6 months. I think there's a lot of inquiring minds who'd love to know more!