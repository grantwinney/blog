---
title: Bringing old JS games back to life, part 2
slug: old-js-games-part2
summary: After resurrecting a 25 year old JS maze game, I decided to run a few more through Copilot and see what could be learned in the process.
featureImageAttr: Photo by <a href="https://unsplash.com/@aerialpicbymax?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Max</a> on <a href="https://unsplash.com/photos/a-large-military-ship-in-the-middle-of-the-ocean-CzAEShfOCoU?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Unsplash</a>
draft: false
date: 2026-06-04T14:14:00
lastmod:
categories:
  - Code Review
topics:
  - JavaScript
  - Retro
  - Old JS Games
  - MS Copilot
aliases:
---
A couple weeks ago, I stumbled on an [old JS game](/old-js-maze-game) that lets you navigate a 3D maze created in ASCII characters, but being that it was over 25 years old and hosted on a long gone site, it didn't work very well, lol. With the help of Copilot, I brought it back to life.

I had some fun doing that, so I decided to check out [a few more archived games](https://web.archive.org/web/20000815211242/http://javascript.internet.com/games/) to see what Copilot could make of them again. It did surprisingly well...

## Battleship

First up, a [battleship game](https://web.archive.org/web/20000815214940/http://javascript.internet.com/games/battleship.html) courtesy of Jason Hotchkiss, who shared it on Dec 15, 1999. It works as-is in chromium-based browsers, which is kind of amazing. It wasn't using anything that modern browsers prohibit, but there's plenty that's been deprecated.

<div class="game">
  <div class="boards">
    <div class="board">
      <div class="heading">COMPUTER'S FLEET</div>
      <div id="computer-grid" class="grid"></div>
    </div>
    <div class="board">
      <div class="heading">PLAYER'S FLEET</div>
      <div id="player-grid" class="grid"></div>
    </div>
  </div>
  <div id="status"></div>
  <div id="message"></div>
  <div><button id="newgame">New Game</button></div>
</div>

### Document.write

The biggest issue was the use of `document.write` to update the DOM, which almost all of these games did, and which [MDN strongly discourages](https://developer.mozilla.org/en-US/docs/Web/API/Document/write) nowadays. Like using `eval` or unparameterized SQL queries, it's a potential security hole. Imagine a commenting system that uses `document.write`, and then someone posts a comment containing javascript code that's naively stored as-is and then loaded (unseen) for every future visitor to the page. 😬

The way the game used it was pretty harmless since it didn't involve user input. It created an `img` for each cell in a 16x16 grid, made them clickable by nesting each in an `a` element, and then wrote them to the DOM. Copilot replaced it with calls to `document.createElement` and `appendChild`, and used `addEventListener` instead of hyperlinks.

### Classes and modules

Copilot did some nice cleanup too, moving arrays of data representing ships, ship types, etc into constants. It also moved most of the code into a "Battleship" class, moving variable initialization into the constructor.

It's nice to see things can be encapsulated this way, so every variable isn't globally defined. Plus the layout looks a lot more familiar to this C# dev.

### BR tag vs CSS grids

The legacy code created each "board" (for the player and computer) by document.writing 16 `img` elements, then writing a `<BR>` tag, rinse and repeat for 16 rows. Copilot refactored it to create and add 256 `img` elements to each grid, using CSS to wrap them into the correct shape. Nice.

```css
grid-template-columns: repeat(16, 16px);
grid-auto-rows: 16px;
```

Check out the code here: [LegacyJSBattleship · grantwinney/BlogCodeSamples](https://github.com/grantwinney/BlogCodeSamples/tree/master/Languages/JavaScript/LegacyGames/LegacyJSBattleship)

---
## Breakout

Next on our tour of ancient JS games, we have [Breakout](https://web.archive.org/web/20000815214951/http://javascript.internet.com/games/break-out.html), uploaded by Nick Young in April 2000. Breakout is a much simpler predecessor of Arkanoid that [came out 50 years ago](https://www.youtube.com/watch?v=AMUv8KvVt08). No powerups or anything special here.. it's just you, a ball, and 40 blocks. Ah, those were simpler times.

<canvas id="breakoutgame" width="440" height="320" style="border:1px solid"></canvas>

Unlike the Battleship one that worked well without any changes, this one didn't work at all. After a little playing around, it's because it used proprietary IE elements. Replacing all the `.posTop` and `.posLeft` properties with `.top` and `.left` gets things moving. Oddly, in some places he used the non-proprietary names as well. Ah, those were complicated times.

So let's see what Copilot did with this one.

### Canvas

The original script had 50 lines using `document.write` to position *40 separate tables* all using `position:absolute`, along with some messages and buttons and various text fields. Yikes.

Copilot replaced it with the [Canvas API](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API), so it could loop through the same code 40 times to draw the rectangles, along with the ball, paddle, timer, and whatever else it needed. The canvas has a *lot* of flexibility, so Copilot moved everything inside it, eliminating the need for buttons and text fields that lived outside the playing area.

### Elapsed Time

The original code used `setTimeout` to make the main loop of the program call itself every 100 ms, or 10x times a second. On each call, it incremented a timer variable, so that it also went up in value 10 per second. Then some other logic divided the timer value by 10 and displayed it. The end result was that the onscreen gameplay time looked like it was just a clock, increasing once per second, but it was a little more complicated and less reliable.

Copilot's new code uses Canvas and calls [requestAnimationFrame](https://developer.mozilla.org/en-US/docs/Web/API/DedicatedWorkerGlobalScope/requestAnimationFrame), which means the main loop is now executing 60x a second, the variable increments 60 a second, and the gameplay timer increases 6 every second. In other words, way too fast. To be fair, the original code had a "fast" mode that reduced the 100ms call to 10ms, and in that case the timer would increase 10 every second, so it wasn't great either.

After some nudging, Copilot fixed it by using the timestamp that `requestAnimationFrame` sends to whatever function you pass it. It saved the first timestamp as the "game start" time, computing a delta for each subsequent call to the main loop, adding that delta to the original time and doing some quick math to get the number of seconds. Neat. And completely overengineered.

My solution was to store `Date.now()` when a new game starts, and then just compare it to the current time each time the canvas updates, replacing its 30 line suggestion with just 2 lines:

```js
this.startTime = Date.now();

`Time: ${Math.floor((Date.now() - this.startTime) / 1000)}`
```

It's a good reminder that although these LLM tools are good at finding *a* solution, it's not always the *best* solution!

Check out the code here: [LegacyJSBreakout · grantwinney/BlogCodeSamples](https://github.com/grantwinney/BlogCodeSamples/tree/master/Languages/JavaScript/LegacyGames/LegacyJSBreakout)

---

## Concentration

Let's check out one more, a simple matching game called [Concentration](https://web.archive.org/web/20000711003541/http://javascript.internet.com/games/concentration.html), uploaded by Brian Gosselin in June 2000.

<div id="concroot"><div id="board"></div><button type="button" value="" id="conctimer" >START</button><div id="boardmsg"></div></div>

Nothing all that new from Copilot. It replaced all the `document.write` calls again, and replaced some of the same stuff as the other games, so I'll focus on something else interesting I noticed.

### Accessing elements directly by name??

The original code assigned a `name` to a few elements, like the `form` and button, [which is still valid](https://www.w3docs.com/snippets/html/what-is-the-difference-between-the-id-and-name-attributes.html#the-name-attribute), but also to `img` elements it created, which is *not* valid. That's not surprising, it's old code right? I'm not great with today's JS, let alone what passed for valid 25 years ago.

```js
<form name="f">

document.write('<img src="image0.gif" name="img'+((6*r)+c)+'" border="0">');

<input type="button" value="         " name="b" onClick="init()">
```

But there's also code that accesses the elements directly by their name, which I had no idea was ever possible. Interesting. Copilot replaced them with `document.getElementById`.

```js
document.f.b.value = ""; // clearing the timer text from the button
document.f.b.value = min + ":" + sec; // setting the timer text on the button
document.f[('img' + i)].src = "image0.gif"; // accessing the images in the form
```

Trying to find info on the above code sent me down a rabbit hole, where I learned that you can actually *still* access elements directly by their ID. Um, what? All I've ever heard about is using `document.getElementByID`, but doing something like this totally works:

```js
<div id ="myDiv">initial value</div>

someButton.addEventListener("click", () => { myDiv.innerHTML = 'new value'; });
```

According to [this thread](https://stackoverflow.com/questions/25325221/why-dont-we-just-use-element-ids-as-identifiers-in-javascript), it's not only allowed but actually standards compliant. Yuck. It seems it's only in there for backwards compatibility though, and generally discouraged.

Enough of that.. here's code for the last one: [LegacyJSConcentration · grantwinney/BlogCodeSamples](https://github.com/grantwinney/BlogCodeSamples/tree/master/Languages/JavaScript/LegacyGames/LegacyJSConcentration)
