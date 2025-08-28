---
categories:
- Erlang
date: "2019-02-09T21:10:27Z"
description: ""
draft: false
cover:
  image: photo-1456406644174-8ddd4cd52a06.jpg
slug: common-dialyzer-errors-and-solutions-in-erlang
summary: When dealing with a dynamically typed language, any effort to tame the beast
  can pay off. For Erlang, that means Dialyzer specs. The more specs added, the more
  helpful the tool becomes - but getting to that point can be painful. Here are some
  warnings I've seen, what they mean and how I solved them.
tags:
- Erlang
title: Common dialyzer errors and solutions in Erlang
---
When you're dealing with a dynamically typed language like Erlang, any effort to [tame the beast](https://grantwinney.com/5-ways-to-tame-the-erlang-beast/#dialyzer) can pay off in spades. I'm currently focused on an Erlang app that has zero [Dialyzer specs](https://learnyousomeerlang.com/dialyzer) in it, so adding them is the hill I'm currently dying on. If you're new to it, check out [Learn You Some Erlang](https://learnyousomeerlang.com/dialyzer).

> Dialyzer begins each analysis optimistically assuming that all functions are good. It will see them as always succeeding, accepting anything, and possibly returning anything. No matter how an unknown is used, it's a good way to use it. This is why warnings about unknown functions are not a big deal when generating PLTs. It's all good anyway; Dialyzer is a natural optimist when it comes to type inference. As the analysis goes, Dialyzer gets to know your functions better and better.

The more specs you add, the more helpful and complete the tool becomes, but getting to that point can be painful at first. Trust me, it's worth it. Once implemented, Dialyzer can save you runtime exceptions, make you aware of dead code, and more. It's prevented me from making some stupid mistakes.

Here are some of the errors I'm encountering as I'm struggling to add specs, along with what they mean and how to solve them. It'll never be all-inclusive, but I'll just keep adding to it over time and hopefully it'll save someone else a headache.

---

## Overlapping Domains

When you've got several functions with the same arity that expect different types of parameters, it's normal to specify several Dialyzer specs too... but sometimes they accidentally overlap and need to be crunched down to a single definition.

> Overloaded contract for module:function/2 has overlapping domains; such contracts are currently unsupported and are simply ignored

### Subsets / Supersets

It's possible one set of specs is a direct subset of another. Here's two clauses with the same arity, one that accepts positive integers and the other that accepts all integers. I'm aware this could be reworked to pass, but humor me. Assuming you couldn't change the code, the first spec should just be removed in favor of the second.

```erlang
-spec add(pos_integer(), pos_integer()) -> pos_integer();
         (integer(), integer()) -> integer();
         (string(), string()) -> string().
add(X, Y) when is_integer(X), is_integer(Y), X > 0, Y > 0 ->
    X + Y;
add(X, Y) when is_integer(X), is_integer(Y) ->
    abs(X + Y);
add(X, Y) when is_list(X), is_list(Y) ->
    X ++ Y.
```

### Redundancy

It's also possible that two specs cover more cases than intended. Here's two clauses where the first accepts undefined and anything to return anything, and the second accepts anything with a string to return anything. These overlap, since ultimately the function can accept anything in either position to return anything. IMO it'd actually be better to allow them separately to better indicate what the developer's intentions were, but it's not supported so that's that.

```erlang
-spec get_value(undefined, any()) -> any();
               (any(), string()) -> any().
get_value(undefined, Default) ->
    Default;
get_value(Value, Default) ->
    lager:debug("Default would've been: ~s", [Default]),
    Value.
```

---

## Invalid Type Specification

As the name suggests, this one occurs any time there's an invalid type spec... and there could be lots of reasons your specification is invalid.

> Invalid type specification for function module:function/1. The success typing is (boolean()) -> atom()

### Missing Parentheses

It could be as simple as accidentally omitting the parentheses after the type name. For example, typing `boolean` instead of `boolean()`.

```erlang
-spec get_status(boolean) -> atom().
```

### Wrong Type

Or it could just be that, true to the name, the type is wrong. Here's a function that only ever returns an atom, but the spec claims it's a boolean.

```erlang
-spec get_status(boolean()) -> boolean().
get_status(true) ->
    ready;
get_status(false) ->
    not_ready.
```

### Inconsistent Types

Check for inconsistent specs between related functions. Here someone saw the comparison in `test_system_status` and mistook the return value for `boolean()`, when the boolean value is really being sent to `get_status()` and the ultimate return value is an `atom()`.

```erlang
-spec test_system_status(#a_record{}) -> boolean().
test_system_status(Record) ->
    get_status(Record#a_record.field_one =:= "foo" or
               Record#a_record.field_two =:= "bar").

-spec get_status(boolean()) -> atom().
get_status(true) ->
    ready;
get_status(false) ->
    not_ready.
```

Another possibility is an inconsistency between how a field is being treated in several places within a function. Here, Dialyzer will complain that the "success typing" for `Age` is actually a string. It's being accepted as an integer, treated as and integer, then stored in a string field. Yeah I know, it'd be goofy to define the record like that, but it makes a point. :)

```erlang
-record(person, {
    name :: string(),
    age :: string(),
    comment :: string()
}).

-spec create_person(string(), integer()) -> #person{}.
create_person(Name, Age) ->
    case Age > 100 of
        true ->
            Comment = "Woah, you're old.";
        _ ->
            Comment = "Get back to work."
    end,
    #person{
        name = Name,
        age = Age,
        comment = Comment
    }.
```

---

## Function Will Never Be Called

This one's nice. Dialyzer can tell you pretty easily if a function is dead code. If a function isn't exported or referenced in its module - in other words, it'll never be called - it'll warn you.

> Function function/2 will never be called

Look for functions that are not exported or used anywhere in the module. You'll usually get a warning when compiling, even without Dialyzer. This might seem obvious, but if you're dealing with source files that are a couple thousand lines with a hundred functions calling one another, dead code can hide pretty well. And once you remove one dead function, it may turn out other code that _it_ was calling is dead too.

```erlang
-module(test).

-export([]).

-spec create_greeting(string(), string()) -> string().
create_greeting(Name, Greeting) ->
    Greeting ++ ", " ++ Name ++ "!".
```

---

## The Pattern Can Never Match

As you add more Dialyzer specs, it's capable of analyzing your code to determine where certain clauses couldn't _possibly_ be hit. This is great for removing unused code, including unnecessary "catch-all" clauses.

> The pattern _some_pattern_ can never match since previous clauses completely covered the type _some_type_

> The variable some_variable can never match since previous clauses completely covered the type some_type

Take the [application:get_env](http://erlang.org/doc/apps/kernel/application.html#get_env-1) function for example. It either finds the value and returns `{ok, Value}`, or doesn't and returns `undefined`. That's it.. it won't return any other value. Let's say someone writes a catch-all (the fourth clause) to handle unexpected input... Dialyzer will complain that it could never match, and it's right.

```erlang
-spec get_username() -> any().
get_username() ->
    get_value(application:get_env(username)).

-spec get_value(tuple() | undefined) -> {ok, any()}.
get_value(undefined) ->
    undefined;
get_value({ok, "none"}) ->
    undefined;
get_value({ok, Value}) ->
    Value;
get_value({_, _}) ->  % The pattern {_, _} can never match since previous clauses
    undefined.        %   completely covered the type 'undefined' | {'ok',_}
```

In the above example, the "catch-all" _might_ be valid, except that all possible values for the type were covered in previous clauses. Remove one of those clauses though, and the "catch-all" could be valuable.

Sometimes though, there's a clause that's just plain wrong. It attempts to handle a value that could never be passed to it.

> The pattern some_pattern can never match the type some_type

Take this function, for instance. It passes the result of `lists:any`, which can only produce a boolean value, to a function with a "catch-all". Under no circumstances will that last clause ever be hit, and Dialyzer knows it.

```erlang
-spec any_waldos([string()]) -> string().
any_waldos(Names) ->
    create_message(lists:any(fun (Name) -> Name =:= "Waldo" end, Names)).

-spec create_message(boolean() | undefined) -> string().
create_message(true) ->
    "Found Waldo!";
create_message(false) ->
    "Where's Waldo?";
create_message(undefined) ->
    "Error".
```

---

## Matching of Pattern Tagged with a Record Name Violates Declared Type

If you've added type specs to a record, and then attempt to pattern match fields of that record in ways that are inconsistent with the specs, Dialyzer can warn you that you've violated the rules you set for the record.

> Matching of pattern {'person', _, 'whatever'} tagged with a record name violates the declared type of #person{name::'undefined' | string(), age::'adult' | 'kid' | 'undefined'}

Assume you've got a `person` record, where `age` can only be two values - `kid` or `adult`. If you write a function that tries to pattern match for a value of `age` that's not one of those two - in this case `whatever` - Dialyzer will warn you of the violation.

```erlang
-record(person, {
    name :: string(),
    age :: kid | adult
}).

-spec get_age(#person{}) -> atom().
get_age(#person{age = whatever}) ->
    none;
get_age(#person{age = Age}) ->
    Age.
```

---

## Function Has No Local Return

This one can be tricky, and sometimes shows up as (or with) different errors _(such as the next one, record construction xyz violates the declared type of field)_.

> Function some_function/2 has no local return

Here's a block of code to demonstrate the problem. The `sec_level` field is a number, but the `get_level` function returns a string. Knowing what the problem and solution is, I've gotta say this particular error message is, well... pretty much crap. I'm sure there's some sense to it, but it seems like there's gotta be a better way to say it.

```erlang
-record(employee, {
    title :: string(),
    sec_level :: non_neg_integer()
}).

-spec create_employee(string(), non_neg_integer()) -> #employee{}.
create_employee(Title, SecurityLevel) ->
    #employee{title = Title, sec_level = get_level(Title, SecurityLevel)}.

-spec get_level(string(), string()) -> string().
get_level("CEO", _) ->
    "10";
get_level("Minion", _) ->
    "0";
get_level(_Title, Security) ->
    Security.
```

---

## Record Construction Violates the Declared Type of Field

You might see this Dialyzer error if you try to store a value type in a field that's not the type you said it should be, like storing a number in a string or vice-versa.

> Record construction #employee{title::string(), sec_level::string()} violates the declared type of field sec_level::'undefined' | non_neg_integer()

This is the same example used above, because this snippet will produce two warnings. Where "function has no local return" is just about useless, this error says exactly what the problem is.

Always check to make sure you're storing the right value types... in this case, storing a string in a field marked as a non-negative integer isn't gonna work, and Dialyzer knows it.

```erlang
-record(employee, {
    title :: string(),
    sec_level :: non_neg_integer()
}).

-spec create_employee(string(), non_neg_integer()) -> #employee{}.
create_employee(Title, SecurityLevel) ->
    #employee{title = Title, sec_level = get_level(Title, SecurityLevel)}.

-spec get_level(string(), string()) -> string().
get_level("CEO", _) ->
    "10";
get_level("Minion", _) ->
    "0";
get_level(_Title, Security) ->
    Security.
```

---

## The Call Breaks the Contract

I'm not exactly sure when this particular warning shows up, but I got it when trying to call a function in a third party library called [MimeMail](https://github.com/Vagabond/gen_smtp/blob/master/src/mimemail.erl).

> The call mimemail:encode({[101 | 116 | 120,...],[104 | 108 | 109 | 116,...],[{<<_:16,_:_*8>>,binary()},...],[],_}) breaks the contract (MimeMail::mimetuple()) -> binary()

If you're lucky, the third party libraries you use took the time to do specs too. You get even more protection against runtime errors, but sometimes it means you'll have to dig into the source code of other libraries to figure out what Dialyzer is complaining about.

I didn't save the code that caused this one, so I couldn't recreate an example, but I'm sure I'll come across it again sooner or later...

---

## The call module:function will never return since it differs from the success typing arguments

These warning messages are starting to all sound somewhat similar and to blend together. Once again, here's a warning message that's not particularly helpful, but it led to finding a problem that absolutely would've thrown an exception at runtime.

> The call lists:map(fun((_) -> nonempty_maybe_improper_list()), Groups::'undefined') will never return since it differs in the 2nd argument from the success typing arguments: (fun((_) -> any()), [any()])

Make sure you're not iterating over a variable as if it'll definitely be a list, unless you're positive it absolutely will be... or can handle it accordingly.

Here's a short code snippet to demonstrate the problem, although the one I found in a production system was buried in a half-dozen or so nested functions and took a full day to find. The `modify_groups` function loops over the groups to create a new collection, but the `create_employee` function pattern matches on `undefined`... which means that `lists:map` is guaranteed to try iterating over an undefined value and will throw an exception. After adding specs to a half-dozen modules and records, Dialyzer caught two of these situations, which made me happy.

```erlang
-record(employee, {
    title :: string(),
    groups :: [string()] | undefined
}).

-spec create_employee(#employee{}) -> #employee{}.
create_employee(#employee{title = Title, groups = undefined} = E) ->
    #employee{groups = modify_groups(Title, E#employee.groups)}.

-spec modify_groups(string(), [string()]) -> [string()].
modify_groups(Title, Groups) ->
    lists:map(fun(Group) -> Title ++ " " ++ Group end, Groups).
```