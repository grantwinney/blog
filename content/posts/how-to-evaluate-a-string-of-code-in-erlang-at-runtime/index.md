---
categories:
- Erlang
- Coding
date: "2017-03-05T18:56:30Z"
description: ""
draft: false
cover:
  image: photo-1560523371-b28d9e070c21.jpg
slug: how-to-evaluate-a-string-of-code-in-erlang-at-runtime
tags:
- erlang
- coding
title: Evaluating a string of code in Erlang at runtime
---
Did you know that Erlang has the ability to read in a string representing a line of code to execute at runtime? It can parse it out, evaluate it and return the value.

Let's see how...

## Evaluating Simple Expressions

At its most basic, we can just read any expression passed in and execute it.

```erlang
-module(parser).
 
-export([
    evaluate_expression/1
]).
 
-spec evaluate_expression(string) -> any().
evaluate_expression(Expression) ->
    {ok, Tokens, _} = erl_scan:string(Expression),    % scan the code into tokens
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),     % parse the tokens into an abstract form
    {value, Result, _} = erl_eval:exprs(Parsed, []),  % evaluate the expression, return the value
    Result.
```

Let's try passing in some simple arithmetic expressions, remembering that statements end in commas and functions with a period, so our strings need to include those punctuations:

```erlang
> c(parser).
{ok,parser}
 
> parser:evaluate_expression("4 > 2.").
true
> parser:evaluate_expression("4+2.").
6
> parser:evaluate_expression("A=7+2,A-4.").
5
```

## Security Considerations

As might be expected though, there are some serious security pitfalls if you allow just anyone to execute an arbitrary line of code though.

### A brief review of SQL injection attacks

Let's switch gears for a minute and talk about SQL injection attacks.

We've just created a UI where a user can just type in their username to see information about themselves. Behind the scenes, we simply take the username as they entered it, and plug it into a query that looks like this:

```sql
"select * from user_table where user_name = " + username
```

When it’s evaluated, it looks something like this, and it returns the record for the user to the page:

```sql
"select * from user_table where user_name = " + "gwinney"
```

As long as the user plays nicely, everything okay. But what if they enter their name as `gwinney; delete * from user_table`? Now the query that’s run ends up looking like this:

```sql
select * from user_table where user_name = gwinney; delete * from user_table
```

The solution to this is to sanitize all input, aka parameterize the query. I don’t want to dive too deeply into it here, but if we’ve done things the _right_ way then the query looks more like this, which will of course fail because that crazy username doesn’t exist.

`select * from user_table where user_name = 'gwinney; delete * from user_table'`

### What’s this have to do with Erlang?

Similarly, we can run into security issues with our expression code.

We’re allowed to include a call to _any_ function – local functions as well as BIFs (Erlang’s built-in functions) and exported functions in other modules we’ve created – and it’ll parse and attempt to execute them.

If we make the above function accessible to the outside world, even indirectly, and the input isn’t sanitized, then we’ve handed over the ability for someone to directly call all kinds of functions they have no business calling. Oops.

So how do we prevent that?

## Intercepting Local Function Calls

We can supply a function to `erl_eval:exprs` through which all calls to local functions will be passed, and that’s where we can take additional actions.

Local functions are those in the same module, which can be called without specifying the module name. Though some BIFs don’t _require_ a module name, like `list_to_binary`, that's only because they’re auto-imported by the system – they’re still considered non-local.

There’s some new stuff in the code below – a function called `handle_local_function` and a local function called `get_random_number` _(thanks_ [_xkcd_](https://xkcd.com/221/)_)_. The handler function outputs an informational message and then handles the passed-in function name.

```erlang
-module(parser).
 
-export([
    evaluate_expression/1
]).
 
-spec evaluate_expression(string) -> any().
evaluate_expression(Expression) ->
    {ok, Tokens, _} = erl_scan:string(Expression),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Parsed, [],
                                        {value, fun handle_local_function/2}),
    Result.
 
-spec handle_local_function(atom(), list()) -> any().
handle_local_function(FunctionName, Arguments) ->
    io:format("Local call to ~p with ~p~n", [FunctionName, Arguments]),
    case FunctionName of
        get_random_number -> get_random_number();
        what_time_is_it -> calendar:universal_time();
        are_we_there_yet -> "no";
        _ -> "uh uh uh. you didn't say the magic word!"
    end.
 
-spec get_random_number() -> integer().
get_random_number() ->
    4.  % chosen by fair dice roll; guaranteed to be random
```

Let's run the module again and pass in some new expressions.

We can intercept local functions (which may not really exist, but the expression evaluator doesn’t know that) and redirect them as we please… or just spit out a message if the user tries to do something invalid:

```erlang
> c(parser).
{ok,parser}
 
> parser:evaluate_expression("get_random_number().").
Local call to get_random_number with []
4
 
> parser:evaluate_expression("what_time_is_it().").
Local call to what_time_is_it with []
{{2017,3,5},{15,21,53}}
 
> parser:evaluate_expression("are_we_there_yet().").
Local call to are_we_there_yet with []
"no"
 
parser:evaluate_expression("break_the_system().").
Local call to break_the_system with []
"uh uh uh. you didn't say the magic word!"
```

## Intercepting Non-Local Function Calls

Similarly, we can supply a function to `erl_eval:exprs` through which all calls to _**non-**_local functions will be passed _(anything outside of the current module, including BIFs and even the operators used in comparisons)._

Here’s the code again, extended to handle non-local functions:

```erlang
-module(parser).
 
-export([
    evaluate_expression/1
]).
 
-spec evaluate_expression(string) -> any().
evaluate_expression(Expression) ->
    {ok, Tokens, _} = erl_scan:string(Expression),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Parsed, [],
                                        {value, fun handle_local_function/2},
                                        {value, fun handle_non_local_function/2}),
    Result.
 
-spec handle_local_function(atom(), list()) -> any().
handle_local_function(FunctionName, Arguments) ->
    io:format("Local call to ~p with ~p~n", [FunctionName, Arguments]),
    case FunctionName of
        get_random_number -> get_random_number();
        what_time_is_it -> calendar:universal_time();
        are_we_there_yet -> "no";
        _ -> "uh uh uh. you didn't say the magic word!"
    end.
 
-spec handle_non_local_function(atom(), list()) -> any().
handle_non_local_function({ModuleName,FunctionName}, Arguments) ->
    io:format("Non-local call to ~p with ~p~n", [FunctionName, Arguments]),
    case ModuleName of
        erlang ->
            case FunctionName of
                '>' -> apply(ModuleName, FunctionName, Arguments);
                '<' -> apply(ModuleName, FunctionName, Arguments);
                list_to_binary -> apply(ModuleName, FunctionName, Arguments);
                _ -> "nope"
            end;
        calendar ->
            case FunctionName of
                universal_time -> calendar:universal_time();
                lets_pretend_this_returns_four -> 4;
                something_ridiculous -> "what calendar are you using??";
                _ -> "notgonnahappen"
            end;
        _ -> "don't think about it"
    end.
 
-spec get_random_number() -> integer().
get_random_number() ->
    4.  % chosen by fair dice roll; guaranteed to be random
```

Note how we explicitly handle the `>` and `<` comparison operators that are part of the erlang module, how we can redirect non-existent functions to existing ones, and how we can display a message if a function is unsupported.

Greater than and less than comparisons are allowed, but not equality… because. Some functions are allowed, some aren’t, and some are redirected. In the last example below, an evil user tries to enact their nefarious plan to take part of the system down, but is foiled. :p

```erlang
> c(parser).
{ok,parser}
 
> parser:evaluate_expression("4 < 2.").
Non-local call to '<' with [4,2]
false
 
> parser:evaluate_expression("4 > 2.").
Non-local call to '>' with [4,2]
true
 
> parser:evaluate_expression("4 == 2.").
Non-local call to '==' with [4,2]
"nope"
 
> parser:evaluate_expression("list_to_binary(\"hi\").").
Non-local call to list_to_binary with ["hi"]
<<"hi">>
 
> parser:evaluate_expression("binary_to_list(<<\"hi\">>).").
Non-local call to binary_to_list with [<<"hi">>]
"nope"
 
> parser:evaluate_expression("calendar:universal_time().").
Non-local call to universal_time with []
{{2017,3,5},{21,4,42}}
 
> parser:evaluate_expression("calendar:local_time().").
Non-local call to local_time with []
"notgonnahappen"
 
> parser:evaluate_expression("calendar:lets_pretend_this_returns_four().").
Non-local call to lets_pretend_this_returns_four with []
4
 
> parser:evaluate_expression("calendar:something_ridiculous().").
Non-local call to something_ridiculous with []
"what calendar are you using??"
 
> parser:evaluate_expression("sys:terminate(some_process, \"buahaha\").").
Non-local call to terminate with [some_process,"buahaha"]
"don't think about it"
```

## What's Next?

Good examples in Erlang can be hard to come by, and what you see here was a fair amount of trial and error. If you find yourself trying to parse code and execute it at runtime, maybe this’ll help.

Other resources to check out:

- Official docs: [erl_scan](http://erlang.org/doc/man/erl_scan.html), [erl_parse](http://erlang.org/doc/man/erl_parse.html), [erl_eval](http://erlang.org/doc/man/erl_eval.html) _(have a pot of coffee ready)_
- [Domain Specific Languages in Erlang](http://people.apache.org/~dennisbyrne/infoq/DSLs_in_Erlang.ppt) _(powerpoint presentation)_
- [Can parameterized statement stop all SQL injection?](http://stackoverflow.com/q/6786034) _(a thread with more details)_