---
categories:
- Raspberry PI
- Coding
- Python
date: "2016-04-03T09:03:25Z"
description: ""
draft: false
cover:
  image: generate-morse-code-banner.png
slug: raspberry-pi-morse-code-transmitter
summary: Making the Pi blink an LED a few times is thrilling, but what about building
  something.. more? Let's build a morse code transmitter!
tags:
- Raspberry PI
- Coding
- Python
title: Building a Morse Code Transmitter on a Raspberry Pi
---
Last week, [I made the Raspberry Pi blink an LED a few times](https://grantwinney.com/hello-world-for-the-raspberry-pi-making-an-led-blink/). As thrilling as that was, I almost immediately wanted something more. Building a morse code transmitter seemed like a nice little challenge.

## Goals

- Setup a simple circuit (LED and resistor) using a breadboard
- Learn about Morse Code in order to correctly translate a sentence
- Manipulate the GPIO pins on the Raspberry Pi to send signals at intervals
- Get familiar with basic Python constructs, like dictionaries, functions and loops

## Setup

To do this, a few things are necessary:

- Install Raspbian on the Pi _(comes with Python 3 preinstalled)_
- Get a kit with a breadboard, LED and resistor _(cobbler is optional, but helpful)_
- [Setup a breadboard with an LED and resistor](https://grantwinney.com/hello-world-for-the-raspberry-pi-making-an-led-blink/)

## What is Morse Code?

The first thing I did was head over to wikipedia to read all about [morse code](https://en.wikipedia.org/wiki/Morse_code), and to decide how much I wanted to implement for this project.

- Morse code is a method of transmitting text as a series of on-off tones, lights, or clicks.
- One variant is called International Morse Code (IMC).
- IMC consists of the Latin alphabet (A-Z), Arabic numerals (0-9), some punctuation, and procedural signals (prosigns).
- Each IMC symbol is represented by a unique sequence of short and long signals called “dots” and “dashes”.
- Prosigns are sequences of letters which have special meaning, such as AS for “Wait”, SN for “Understood” or SOS for “Distress”.

Here are the rules regarding timing, for relaying a message in morse code.

- A dot duration is the basic unit of time measurement in code transmission (.) : 1
- A dash duration is 3x the dot duration (–) : 111
- Each dot or dash is followed by a short silence, equal to the dot duration : 0
- Letters in a word are separated by a gap equal to 3 dots : 000
- Words are separated by a gap equal to 7 dots : 0000000

The wikipedia page also includes a [morse code reference chart](https://en.wikipedia.org/wiki/File:International_Morse_Code.svg) we can use for translating.

![](International_Morse_Code.png)

That should be enough information to get started.

## Coding the Translator

Here are a few notable notes about the approach I took.

### The Circuit

A closeup of the circuit I created. _Pin 21 » LED » resistor » ground_

![breadboard single led circuit](breadboard-single-led-circuit.jpg)

### Mapping Characters to Morse Code

The first thing I decided to do was get the above chart into a dictionary. Now I can look up any character and get its morse code equivalent.

In Python, the dictionary looks something like this (trimmed down):

```python
Symbols = {
    'A': '.-',
    'B': '-...',
    'C': '-.-.',
    # more letters
 
    '1': '.----',
    '2': '..---',
    '3': '...--',
    # more numbers
 
    '.': '.-.-.-',
    ',': '--..--',
    '?': '..--..',
    # more punctuation
}
```

### Mocking Out the GPIO Module

The GPIO module is a library of methods that make manipulating the GPIO pins easy. As easy as specifying which pin to use, if it’ll receive a signal (in) or send one (out), and whether it’s on (high) or off (low).

I found it easier to develop on a laptop with PyCharm installed, instead of on the Pi itself, so I didn’t have access to the RPi.GPIO module.

As a workaround, I created a mock file with the same functions I’d be accessing from the RPi.GPIO library, each of which just printed a message to the screen. It prints “Sending High signal” when it should’ve turned the LED on, “Sending Low signal” when it should’ve turned the LED off, and so on.

```python
HIGH = 1
LOW = 0
 
BCM = 'BroadCom Numbering'
BOARD = 'Board Numbering'
 
IN = '"In" Direction for Pin'
OUT = '"Out" Direction for Pin'
 
def output(pin, signal):
    print("Sending {} signal to GPIO pin {}".format(_convert_signal_to_text(signal), pin))
 
# omitted other functions my program mocks out,
#  but you can find them in the github repo
 
def _convert_signal_to_text(signal):
    if signal == HIGH:
        "High"
    else:
        "Low"
```

Whether importing the real library or my mock, I made sure to give them an alias of “GPIO”. Running the program on the Pi was then as simple as replacing `import GPIOmock as GPIO` with `import RPi.GPIO as GPIO`, and all other lines of code referencing `GPIO.whatever()` could be left as-is.

### Distinguishing Dots from Dashes from Letters from…

The user can input a full sentence, but that has to be broken down into smaller components for transmission.

Python’s `split()` command breaks up the initial sentence into words nicely, and from there it’s loops all the way down. Do _something_ for each symbol, in each letter, in each word.

And since all intervals between the dots and dashes are simply a multiple of the base unit time, defined as the time for a single “dot”, I created a variable called `UNIT_TIME` that represents seconds. Everything else – dots, dashes, space between letters and words, is based off of that, per the rules above. We can change the value in `UNIT_TIME` to transmit messages more quickly or more slowly.

## Trying it Out

The kids have been having fun with it. Said they were sending messages to Australia. Hey, anything’s possible, but you’d have to have a pretty decent pair of binoculars and maybe a few mirrors to see our LED from the other side of the world. :)

[Here it is working](https://res.cloudinary.com/dxm4riq52/video/upload/q_auto/v1583296394/Raspberry%20Pi/Morse_Code_via_LED_on_the_Raspberry_Pi_2_lmqsvf.mp4).. if you try this out yourself, let me know how it goes! If you improve on it, or find a flaw in my approach, I’d like to hear about that too – share in the comments below.

## Final Thoughts

Other random things I learned…

### Circuit Design Tools

The circuit I created for this was as simple as it gets. But I’ve seen some that use most of the breadboard and have wires crossing everywhere, and in those cases a tool that helps map out the design of the circuit may be useful. I haven’t tried these yet, so I can’t say much else.

- [Fritzing](http://fritzing.org/home/), _electronics made easy_
- [123D Circuits](https://123d.circuits.io/), _electronics from beginner to pro_
- [EasyEDA](https://easyeda.com/),* circuit simulation, PCB design, electronic circuit design online*

### Tutorials

- [How to Use a Breadboard and Build a LED Circuit](http://computers.tutsplus.com/tutorials/how-to-use-a-breadboard-and-build-a-led-circuit--mac-54746)
- Introduction to [Using a Breadboard](https://www.cl.cam.ac.uk/projects/raspberrypi/tutorials/robot/breadboard/)

### gpiocrust (mocking RPi.GPIO)

I mocked a few functions for this short program, but when I find myself doing something more complicated that uses more of the GPIO library, I’ll check into [gpiocrust](https://github.com/zourtney/gpiocrust).

> A concise, pythonic wrapper around the Raspberry Pi’s RPi.GPIO library. An encrusting, if you will. With (almost silent) fallback to mock objects, you can prototype pin I/O locally on your favorite computer, even when your Pi is on the other side of town (see Mock API for more details). gpiocrust is fully compatible with Python 2 and Python 3.

### RPi.GPIO Module

The [RPi.GPIO](https://pypi.python.org/pypi/RPi.GPIO) module makes it easier to access the GPIO pins. I used it a little bit here. If you’re interested, here’s some [documentation](https://sourceforge.net/p/raspberry-gpio-python/wiki/Home/).
