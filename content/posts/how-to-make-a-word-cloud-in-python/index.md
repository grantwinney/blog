---
categories:
- Coding
- Python
date: "2020-06-04T21:43:00Z"
description: ""
draft: false
cover:
  image: 2020-06-02-15_53_53-Create-word-clouds---WordItOut---Brave.png
slug: how-to-make-a-word-cloud-in-python
summary: We've all seen word clouds, like in the sidebars of blogs, but let's see
  how we might create our own with a little bit of code!
tags:
- Coding
- Python
title: Creating a basic Word Cloud in Python
---


You've most likely seen word clouds before, like in the sidebars of blogs. It's a fun, easy way to visualize which words in a group are more significant in some way. While you can create your own online, there's no reason you can't write your own in the language of your choice too.

Here's a quick example I threw together using Python and the built-in Tkinter module for drawing to the screen. In a very rough way, the smaller the percentage gets, the more faded I make the color, and the farther I move it from the center.

import random
from tkinter import *     # use Tkinter for Python2
window = Tk()
window.title("Welcome to LikeGeeks app")
window.geometry('800x480')

mid_h = 320
mid_v = 240

results = {
    'Back-end Dev': 55.2,
    'Full-stack Dev': 54.9,
    'Front-end Dev': 37.1,
    'Desktop / Enterprise': 23.9,
    'Mobile Dev': 19.2,
    'DevOps': 12.1,
    'DB Admin': 11.6,
    'Designer': 10.8,
    'System Admin': 10.6,
    'Embedded Apps': 9.6,
    'Data / Business Analyst': 8.2,
    'Data Scientist / Machine Learning': 8.1,
    'QA / Tester': 8,
    'Data Engineer': 7.6,
    'Academic Researcher': 7.2,
    'Educator': 5.9,
    'Gaming / Graphics': 5.6,
    'Engineering Manager': 5.5,
    'Product Manager': 5.1,
    'Scientist': 4.2
}

def rnd_pos():
    return 1 if random.random() < 0.5 else -1

for dev_type, percentage in results.items():
    start_pos = int(165-(percentage*3))
    color = '#%02x%02x%02x' % (255, 50+start_pos, 50+start_pos)
    lbl = Label(window, text=dev_type, font=("Arial", int(percentage)), fg=color)
    lbl.place(x=mid_h-(start_pos*rnd_pos()), y=mid_v-(start_pos*rnd_pos()))

window.mainloop()

It ain't the prettiest, but I think it's passable for a non-pythonista in under an hour. In the right setting, it's a good way to steer someone's focus on whatever's most important first... however you happen to define that!

