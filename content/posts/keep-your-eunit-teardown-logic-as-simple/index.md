+++
categories = ["Erlang", "Testing", "Coding"]
date = 2019-03-14T11:03:00Z
description = ""
draft = false
image = "https://images.unsplash.com/photo-1487700160041-babef9c3cb55?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1080&fit=max&ixid=eyJhcHBfaWQiOjExNzczfQ"
slug = "keep-your-eunit-teardown-logic-as-simple"
summary = "Ever had an EUnit test fixture fail with meck reporting it was \"already_started\"? Well I did, and here's why..."
tags = ["Erlang", "Testing", "Coding"]
title = "Keep your EUnit teardown logic as simple as possible!"

+++


When you use test fixtures in EUnit, you'll likely define a setup and a teardown function, for doing initialization and cleanup work before and after each test. If you're familiar with try/catch/finally blocks in other languages, the teardown function is similar to a finally block; that is, it should always run even when a test throws an exception. But like a finally block, you want to be careful about what you're doing in your cleanup.

I ran into an issue recently where EUnit tests that were part of a test fixture were failing with an error I hadn't seen before. The error seemed to be coming from the meck mocking suite itself, and was reporting that it was "already_started"... and the tests would fail to run.

Here's a small program we can use to see the problem. All it does is accept a name, and print out a short greeting with the current time. (The code below is trimmed down, but the full code is available on GitHub if you'd like to run it. You'll want to have Rebar3 installed, and it'd help to be familiar with Meck.)

-module(salutations_app).
-export([greeting_time/1]).

greeting_time(Name) ->
    format("Hi ~s, it's ~s!", [Name, current_time()]).

%% INTERNAL

current_time() ->
    binary_to_list(iso8601:format(calendar:universal_time())).

format(Template, Params) ->
    lists:flatten(io_lib:fwrite(Template, Params)).


Teardown succeeds, even when a test throws an exception

Here's the first example. Two of these tests intentionally throw exceptions - dividing by zero and sorting a non-list - but the teardown function should run regardless of whether individual tests throw an exception.

-module(exceptions_in_tests).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-import(salutations_app, [greeting_time/1]).

setup() ->
    Modules = [iso8601],
    meck:new(Modules),
    meck:expect(iso8601, format, fun(_) -> <<"2019-02-16T01:06:48Z">> end),
    Modules.

teardown(Modules) ->
    ?debugFmt("Do we ALWAYS get into teardown? (yes)", []),
    meck:unload(Modules).

greeting_time_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [
      {"greet bob", fun bob_gets_expected_greeting/0},
      {"greet tim", fun tim_gets_expected_greeting/0},
      {"greet sue", fun sue_gets_expected_greeting/0}
     ]
    }.

bob_gets_expected_greeting() ->
    1/0,  % <- no good can come of this!
    ?assertEqual("Hi Bob, it's 2019-02-16T01:06:48Z!", salutations_app:greeting_time("Bob")).

tim_gets_expected_greeting() ->
    ?assertEqual("Hi Tim, it's 2019-02-16T01:06:48Z!", salutations_app:greeting_time("Tim")).

sue_gets_expected_greeting() ->
    ?debugFmt("Do we start the test? (yes)", []),
    lists:sort(this_aint_no_list),
    ?debugFmt("Do we finish the test? (no way)", []),
    ?assertEqual("Hi Sue, it's 2019-02-16T01:06:48Z!", salutations_app:greeting_time("Sue")).

-endif.

From the output below, we can see where sue_gets_expected_greeting printed the first debug statement, but not the second after the exception is thrown. Both exceptions are printed to the console. But the teardown function ran all three times, even for the tests that fail. 👍

> rebar3 eunit --module exceptions_in_tests
===> Verifying dependencies...
===> Compiling salutations
===> Performing EUnit tests...

<0.107.0>: Do we ALWAYS get into teardown? (yes)
<0.107.0>: Do we ALWAYS get into teardown? (yes)
<0.131.0>: Do we start the test? (yes)
<0.107.0>: Do we ALWAYS get into teardown? (yes)

Failures:

  1) exceptions_in_tests:greeting_time_test_/0: greet bob
     Failure/Error: {error,badarith,
                        [{exceptions_in_tests,bob_gets_expected_greeting,0,
                             [{file,
                                  "c:/.../exceptions_in_tests.erl"},
                              {line,29}]}]}

  2) exceptions_in_tests:greeting_time_test_/0: greet sue
     Failure/Error: {error,function_clause,
                        [{lists,sort,
                             [this_aint_no_list],
                             [{file,"lists.erl"},{line,478}]},
                         {exceptions_in_tests,sue_gets_expected_greeting,0,
                             [{file,
                                  "c:/.../exceptions_in_tests.erl"},
                              {line,37}]}]}

Finished in 0.343 seconds
3 tests, 2 failures
===> Error running tests


Teardown fails, when the teardown itself throws an exception

Here's the second example. Now the tests should pass, but the teardown function itself will throw an exception. The question is, what happens when it throws before the meck:unload runs?

-module(exceptions_in_teardown).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-import(salutations_app, [greeting_time/1]).

setup() ->
    Modules = [iso8601],
    meck:new(Modules),
    meck:expect(iso8601, format, fun(_) -> <<"2019-02-16T01:06:48Z">> end),
    Modules.

teardown(Modules) ->
    ?debugFmt("Do we ALWAYS get into teardown? (well, the first time...)", []),
    _ = 1/0,
    meck:unload(Modules).

greeting_time_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [
      {"greet bob", fun bob_gets_expected_greeting/0},
      {"greet tim", fun tim_gets_expected_greeting/0},
      {"greet sue", fun sue_gets_expected_greeting/0}
     ]
    }.

bob_gets_expected_greeting() ->
    ?assertEqual("Hi Bob, it's 2019-02-16T01:06:48Z!", salutations_app:greeting_time("Bob")).

tim_gets_expected_greeting() ->
    ?assertEqual("Hi Tim, it's 2019-02-16T01:06:48Z!", salutations_app:greeting_time("Tim")).

sue_gets_expected_greeting() ->
    ?assertEqual("Hi Sue, it's 2019-02-16T01:06:48Z!", salutations_app:greeting_time("Sue")).

-endif.

Nothing good, as it turns out! This is why I was seeing the "already_started" error - a previous meck:unload fails to run and the next test causes meck:new to run again. This example is silly, but what if you had some tests creating a file (yeah, yeah, against unit test philosophy but whatever) and wanted to delete it each time? What if one of those deletes failed and threw an exception? Every test after it fails too. 😭

> rebar3 eunit --module exceptions_in_teardown
===> Verifying dependencies...
===> Compiling salutations
===> Performing EUnit tests...

<0.218.0>: Do we ALWAYS get into teardown? (well, the first time...)

Pending:
  undefined
    %% Unknown error: {abort,
                   {cleanup_failed,
                       {error,badarith,
                           [{exceptions_in_teardown,teardown,1,
                                [{file,
                                     "c:/.../exceptions_in_teardown.erl"},
                                 {line,17}]}]}}}
  undefined
    %% Unknown error: {abort,
                   {setup_failed,
                       {error,
                           {already_started,<0.219.0>},
                           [{meck_proc,start,
                                [iso8601,[]],
                                [{file,
                                     "c:/.../_build/test/lib/meck/src/meck_proc.erl"},
                                 {line,93}]},
                            {lists,foreach,2,[{file,"lists.erl"},{line,1336}]},
                            {meck,new,1,
                                [{file,
                                     "c:/.../_build/test/lib/meck/src/meck.erl"},
                                 {line,141}]},
                            {exceptions_in_teardown,setup,0,
                                [{file,
                                     "c:/.../exceptions_in_teardown.erl"},
                                 {line,11}]}]}}}
  undefined
    %% Unknown error: {abort,
                   {setup_failed,
                       {error,
                           {already_started,<0.219.0>},
                           [{meck_proc,start,
                                [iso8601,[]],
                                [{file,
                                     "c:/.../_build/test/lib/meck/src/meck_proc.erl"},
                                 {line,93}]},
                            {lists,foreach,2,[{file,"lists.erl"},{line,1336}]},
                            {meck,new,1,
                                [{file,
                                     "c:/.../_build/test/lib/meck/src/meck.erl"},
                                 {line,141}]},
                            {exceptions_in_teardown,setup,0,
                                [{file,
                                     "c:/.../exceptions_in_teardown.erl"},
                                 {line,11}]}]}}}

Finished in 0.235 seconds
3 tests, 0 failures, 3 cancelled
===> Error running tests


Teardown succeeds, as long as it handles exceptions

Oooookay, first let me say you should really refactor your teardown function to do as little as possible and make it simple. But if that's not possible, then at the very least surround anything that could potentially fail in a try/catch/after block. Here's one final example that catches exceptions and guarantees that the meck:unload will run.

-module(exceptions_in_teardown_handled).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-import(salutations_app, [greeting_time/1]).

setup() ->
    Modules = [iso8601],
    meck:new(Modules),
    meck:expect(iso8601, format, fun(_) -> <<"2019-02-16T01:06:48Z">> end),
    Modules.

teardown(Modules) ->
    try
        ?debugFmt("Do we ALWAYS get into teardown? (hopefully!)", []),
        _ = 1/0
    of _ -> ok
    catch
        C:R -> ?debugFmt("Teardown failed!!! ~p : ~p", [C,R])
    after
        ?debugFmt("Do we ALWAYS get into after block?", []),
        meck:unload(Modules)
    end.

greeting_time_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [
      {"greet bob", fun bob_gets_expected_greeting/0},
      {"greet tim", fun tim_gets_expected_greeting/0},
      {"greet sue", fun sue_gets_expected_greeting/0}
     ]
    }.

bob_gets_expected_greeting() ->
    ?assertEqual("Hi Bob, it's 2019-02-16T01:06:48Z!", salutations_app:greeting_time("Bob")).

tim_gets_expected_greeting() ->
    ?assertEqual("Hi Tim, it's 2019-02-16T01:06:48Z!", salutations_app:greeting_time("Tim")).

sue_gets_expected_greeting() ->
    ?assertEqual("Hi Sue, it's 2019-02-16T01:06:48Z!", salutations_app:greeting_time("Sue")).

-endif.

So now we have a teardown that should always run and finish, thanks to an after block that runs meck:unload if all hell breaks loose. Granted, if it did throw when it failed to delete a file, you might run into other issues... but one disaster at a time. 😎

> rebar3 eunit --module exceptions_in_teardown_handled
===> Verifying dependencies...
===> Compiling salutations
===> Performing EUnit tests...

<0.107.0>: Do we ALWAYS get into teardown? (hopefully!)
<0.107.0>: Teardown failed!!! error : badarith
<0.107.0>: Do we ALWAYS get into after block?
    
<0.107.0>: Do we ALWAYS get into teardown? (hopefully!)
<0.107.0>: Teardown failed!!! error : badarith
<0.107.0>: Do we ALWAYS get into after block?
    
<0.107.0>: Do we ALWAYS get into teardown? (hopefully!)
<0.107.0>: Teardown failed!!! error : badarith
<0.107.0>: Do we ALWAYS get into after block?

Finished in 0.438 seconds
3 tests, 0 failures