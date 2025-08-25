+++
categories = ["Erlang", "Coding"]
date = 2018-06-06T18:18:06Z
description = ""
draft = false
image = "__GHOST_URL__/content/images/2018/06/antique-clock.jpg"
slug = "getting-rid-of-unused-function-errors-when-using-timers-in-erlang"
summary = "Have you ever tried to execute a function at some future time in Erlang? You can, with a timer, but the compiler may complain that the function you're calling via the timer is unused. Why is that and what can you do?"
tags = ["Erlang", "Coding"]
title = "Getting rid of unused function errors when using timers in Erlang"

+++


The Problem

Have you ever tried to execute a function at some future time in Erlang? You can, with the timer:apply_after (and related) functions, but you're likely to run into an error when compiling. Let's say you have a module with two functions - one is exported, and the other is simply used to print your age.

-module(test).
-export([main/0]).

print_age(Age) ->
  io:format("Your age: ~p~n", [Age]).

main() ->
  timer:apply_after(2000, test, print_age, [20]).


If you try compiling the above module, you'll get an error like this:

> c(test).
test.erl:4: Warning: function print_age/1 is unused
{ok,test}

Or if you have Dialyzer configured, you'll see a very similar error:

test.erl:4: function print_age/1 is unused

But, but... we know this function will be used! How do we get rid of the error?


Solution #1 (meh - suppressing the warning)

You can suppress warnings such as this one with a compiler option. The following will make the compiler silent about the unused function. And in certain cases, that's what you need to do... but not in this case.

-compile({nowarn_unused_function, {print_age,1}}).


Oddly, the following seems like it should work similar to the above, since I'm using Dialyzer and Dialyzer has its own options, but it had no effect. 🤷‍♂

-dialyzer({no_unused, [print_age/1]}).


If you compile the file again and then call main() it'll wait 2 seconds and print an error message instead of the age:

=ERROR REPORT==== 6-Jun-2018::13:37:14 ===
Error in process <0.89.0> with exit value:
{undef,[{test,print_age,[20],[]}]}


Solution #2 (better - exporting the function)

There's nothing in the timer docs saying the function you're calling has to be exported, but it does, so export the function that you want the timer to call:

-module(test).
-export([main/0,print_age/1]).

print_age(Age) ->
  io:format("Your age: ~p~n", [Age]).

main() ->
  timer:apply_after(2000, test, some_func, [20]).

Now when you compile and run it, you'll see the age printed:

Your age: 20


What Happened?

Why does this happen? I'm no expert, but here's a post that suggests the timer is running in a gen_server in a separate process.

The timer module is a standard gen_server running in a separate process. All the function in the timer module are public interfaces that execute a hidden gen_server:call or gen_server:cast to the timer server. This is a common usage to hide the internal of a server and allow further evolutions without impact on existing applications.

You can read more about the gen_server here, but think of it this way. After passing a function to the timer module, two things need to be able to happen:

 * The rest of your codebase has to continue running, so it can't wait at the timer.
 * Your function call has to be stored somewhere until it's ready to execute (after the delay you specify).

And where it's stored is in a separate process with its own modules and functions, out of the way of the current process that needs to keep running. But in order for the new process to access the original function you specified, that function must be exported. One module can't access a function in another module unless it's exported.

Unfortunately, that's a little messy since you may not want other modules to be able to call that function (which is possible once it's exported), but the only way I see around that is to leave good documentation on your code.