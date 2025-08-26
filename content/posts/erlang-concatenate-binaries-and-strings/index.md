---
categories:
- Erlang
- Coding
date: "2017-09-26T16:09:00Z"
description: ""
draft: false
slug: erlang-concatenate-binaries-and-strings
summary: Concatenating strings and binaries in Erlang can get ugly quick. Let's make
  it easier.
tags:
- Erlang
- Coding
title: Concatenate Binaries and Strings in Erlang
---


The Problem

There have been a number of times when using Erlang that I've found myself concatenating a list of binaries and strings. I usually resort to manual conversions one way or the other... and I think you'll agree they're both pretty ugly.

<< <<"One">>/binary, (list_to_binary(" Two "))/binary, <<"Three">>/binary, (list_to_binary(" Four!"))/binary >>.
% <<"One Two Three Four!">>

binary_to_list(<<"One">>) ++ " Two " ++ binary_to_list(<<"Three">>) ++ " Four!".
% "One Two Three Four!"

There's a helpful lists:concat function that quickly converts a list of elements to a single string. Unfortunately, the list of allowed types doesn't include binaries.

lists:concat(["one",5,asdf,1.25]).
% "one5asdf1.25000000000000000000e+00"


The Solution

Here's a small snippet with some eunit tests that'll convert binaries to strings, run them through the lists:concat function, then return the binary or string you asked for.



You can also find the code in a gist on GitHub.



-module(utils).

-export([concat/2]).

%% EXTERNAL

concat(Words, string) ->
    internal_concat(Words);
concat(Words, binary) ->
    list_to_binary(internal_concat(Words)).

%% INTERNAL

internal_concat(Elements) ->
    NonBinaryElements = [case Element of _ when is_binary(Element) -> binary_to_list(Element); _ -> Element end || Element <- Elements],
    lists:concat(NonBinaryElements).

%% EUNIT TESTS

-include_lib("eunit/include/eunit.hrl").

concat_conversion_test_() ->
    [
        {"list of strings to string", ?_assertEqual("This and that.", utils:concat(["This", " and", " that."], string))},
        {"list of strings to binary", ?_assertEqual(<<"This and that.">>, utils:concat(["This", " and", " that."], binary))},
        {"list of binaries to string", ?_assertEqual("This and that.", utils:concat([<<"This">>, <<" and">>, <<" that.">>], string))},
        {"list of binaries to binary", ?_assertEqual(<<"This and that.">>, utils:concat([<<"This">>, <<" and">>, <<" that.">>], binary))},
        {"mix of values to string", ?_assertEqual("This and that 5asdf hi.", utils:concat([<<"This">>, " and", <<" that ">>, 5, asdf, " hi."], string))},
        {"mix of values to binary", ?_assertEqual(<<"This and that 5asdf hi.">>, utils:concat([<<"This">>, " and", <<" that ">>, 5, asdf, " hi."], binary))}
    ].


Usage

To use it, just pass a list of any element types that lists:concat would normally allow, as well as binaries.

utils:concat([<<"One">>, " two ", three, 5, 1.25], binary).
% <<"One two three51.25000000000000000000e+00">>

utils:concat([<<"One">>, " two ", three, 5, 1.25], string).
% "One two three51.25000000000000000000e+00"

If you find a better way to do this, let me know!
