---
categories:
  - Coding
date: 2019-05-16T11:22:49Z
description: ""
draft: false
slug: how-to-modify-a-config-file-in-erlang
summary: Modifying an Erlang config file at runtime wasn't as easy (or obvious) as I'd thought it'd be. So I wrote a script to hopefully make it easier.
tags:
  - erlang
title: Modify a config file in Erlang
---
I found myself recently needing to write an [escript](http://erlang.org/doc/man/escript.html) to modify a [config file](https://www.erlang.org/docs/19/man/config). All I needed was to read it in, make a couple updates, and write it back out. Should be easy, right? Please make it easy Erlang. No? Okay... 😢

> The code in this post is available on <a href="https://github.com/grantwinney/BlogCodeSamples/tree/master/Languages/Erlang/ConfigFileModifier">GitHub</a>, for you to use, expand upon, or just follow along while you read... and hopefully discover something new!

Here's a sample of what the config file looks like, without resembling actual production code of course. The point is, it's nothing special - just a list of configuration parameters for a system, laid out in a nested `proplist` format.

```erlang
[{application_1,[{log_options,[{log_path,"C:/Program Files/Acme/Logs"}]}]},
 {application_2,[{log_options,[{log_path,"C:/Program Files/Acme/Logs"}]},
                 {app_options,[{max_attempts,4},{attempt_delay_ms,5000}]},
                 {dependencies,[[{name,writer},
                                 {exe,"C:/Program Files/Acme/Writer.exe"}],
                                [{name,logger},
                                 {exe,"C:/Program Files/Acme/Logger.exe"}],
                                [{name,server},
                                 {exe,"C:/Program Files/Acme/Server.exe"}]]}]},
 {application_3,[{app_options,[{allowed_groups,[admin,manager]}]}]}].
```

## Reading in terms from a file

My first thought was to just open the file and use the [proplists](http://erlang.org/doc/man/proplists.html) module to parse it, but whenever I opened it I got a binary string with the contents of the file. Was I reading it wrong? I started looking at the [file](http://erlang.org/doc/man/file.html) module for different ways to read a file, aaaand..... I had skipped right over the function I needed - [file:consult/1](http://erlang.org/doc/man/file.html#consult-1). If your file has nothing but legit Erlang code in it, then `file:consult()` can read it into memory.

In my defense, the name, description, and example are all awful... _"Reads Erlang terms, separated by '.'"_ That's all we get, but then the Erlang documentation leaves _much_ to be desired. And the name!! What does consulting a file have to do with reading in Erlang terms? And of course, there's no opposite unconsult or deconsult. Why can't we have a module that makes parsing and modifying these config files easier? 😖

## Modifying a config file

And so, I present my own, more appropriately-named module called `config_parser`. You can grab it below or [find it on GitHub](https://github.com/grantwinney/BlogCodeSamples/tree/master/Languages/Erlang/ConfigFileModifier), and modify it to your heart's content. It reads and writes _([thank you](https://zxq9.com/archives/1021))_ config files, and can also get and set nested terms so you can more easily modify them.

```erlang
% Author: Grant Winney
% License: MIT

-module(config_parser).

-export([read_terms/1, get_nested_terms/2, set_nested_terms/3, write_terms/2]).

read_terms(FileName) ->
    case file:consult(FileName) of
        {ok, [Terms]} ->
            {ok, Terms};
        {error, {_Line, _Mod, _Term} = Reason} ->
            {error, file:format_error(Reason)};
        {error, Reason} ->
            {error, error_message(Reason, FileName)}
    end.

write_terms(FileName, Terms) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    file:write_file(FileName, lists:map(Format, [Terms])).

get_nested_terms(Keys, Terms) ->
    lists:foldl(fun(Key, InnerTerms) -> proplists:get_value(Key, InnerTerms) end, Terms, Keys).

set_nested_terms([Key], ReplacementTerms, Terms) ->
    lists:keyreplace(Key, 1, Terms, {Key, ReplacementTerms});
set_nested_terms([Key|NestedKeys], ReplacementTerms, Terms) ->
    InnerValue = set_nested_terms(NestedKeys, ReplacementTerms, proplists:get_value(Key, Terms)),
    lists:keyreplace(Key, 1, Terms, {Key, InnerValue}).


error_message(enoent, FileName) ->
    io_lib:format("The file does not exist: ~p", [FileName]);
error_message(eaccess, FileName) ->
    io_lib:format("Missing permission for reading the file, or for searching one of the parent directories: ~p", [FileName]);
error_message(eisdir, FileName) ->
    io_lib:format("The named file is a directory: ~p", [FileName]);
error_message(enotdir, FileName) ->
    io_lib:format("A component of the filename is not a directory: ~p", [FileName]);
error_message(enomem, _FileName) ->
    io_lib:format("There is not enough memory for the contents of the file.");
error_message(Error, FileName) ->
    io_lib:format("~p error: ~p", [Error, FileName]).
```

## Usage

There's a couple other files in the [repo](https://github.com/grantwinney/BlogCodeSamples/tree/master/Languages/Erlang/ConfigFileModifier) so you can try it out. Just leave them in the same directory, compile the Erlang module, and run the two functions to see how it updates the config file. You should see a new dependency added to application_2, and a new group added to application_3.

```erlang
[{application_1,[{log_options,[{log_path,"C:/Program Files/Acme/Logs"}]}]},
 {application_2,[{log_options,[{log_path,"C:/Program Files/Acme/Logs"}]},
                 {app_options,[{max_attempts,4},{attempt_delay_ms,5000}]},
                 {dependencies,[[{name,writer},
                                 {exe,"C:/Program Files/Acme/Writer.exe"}],
                                [{name,logger},
                                 {exe,"C:/Program Files/Acme/Logger.exe"}],
                                [{name,server},
                                 {exe,"C:/Program Files/Acme/Server.exe"}],
                                [{name,consumer},
                                 {exe,"C:/Program Files/Acme/Consumer.exe"}]]}]},
 {application_3,[{app_options,[{allowed_groups,[admin,manager,owner]}]}]}].
```

## Issues

If you have a fix or problem, feel free to [submit a PR](https://github.com/grantwinney/BlogCodeSamples/pulls) or [open an issue](https://github.com/grantwinney/BlogCodeSamples/issues/new?title=Issue%20regarding%20Erlang%20config%20file%20script). Also, I haven't added any specs or EUnit tests around this, but if you do and you'd like to share them, I'd like to include them!
