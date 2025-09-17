---
categories:
  - Exploring
date: 2018-05-22T16:55:18Z
description: ""
draft: false
slug: are-property-accessors-possible-in-erlang
summary: I ran into a problem in Erlang yesterday that made me think... is there anyway to implement a property accessor on a record?
tags:
  - erlang
title: Are property accessors possible in Erlang records?
---
I was tackling a new requirement the other day, which needed a new record. One of the fields is a list of items, while another happens to represent a count of those items - not that the consumer of the record would necessarily be aware of that relationship.

It would've been convenient to be able to define a record like this, where as soon as a `class` was created and a list of `person` records assigned to it, the `number_of_students` was somehow automatically set to the length of the `students` list.

```erlang
-record(person,
        {
            name :: string(),
            grade :: string()
        }).
-record(class,
        {
            subject :: string(),
            professor :: string(),
            students :: [#person{}],
            number_of_students = length(students) :: integer()  % won't work
        }).
```

## What's a property accessor look like?

If you're unfamiliar with the concept of property accessors - maybe because Erlang is your first language - let's take a look at a couple examples.

C# has [property accessors](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/properties), like this one where the `Greeting` property concatenates a person's first and last names and prepends "Hello", to form a greeting. Anytime a name is modified, the property will return the updated name the next time it's called.

```csharp
using System;
					
public class Program
{
	public static void Main()
	{
		var p = new Person { FirstName = "Jane", LastName = "Doe" };
        
		Console.WriteLine(p.Greeting);  // "Hello, Jane Doe!"
	}
}

public class Person
{
	public string FirstName { get; set; }
	public string LastName { get; set; }
	
	public string Greeting
	{
		get { return $"Hello, {FirstName} {LastName}!"; }
	}
}
```

And here's [similar functionality in Ruby](https://www.sitepoint.com/properties-and-methods-in-ruby-from-a-net-pov/). Again, if either the first or last name is modified, calling `Greeting` will return a string with the updated names in it.

```ruby
class Person
  attr_accessor :first_name
  attr_accessor :last_name
 
  def initialize(first_name, last_name)
    @first_name = first_name
    @last_name = last_name
  end
  
  def Greeting
    "Hello, #{@first_name} #{@last_name}!"
  end
end

person = Person.new("Jane","Doe")

puts person.Greeting  # "Hello, Jane Doe!"
```

## Is it even necessary in Erlang?

The strength of the property accessor, in my mind, is that it abstracts away some detail that someone else using it will not then have to worry about. There's a string that happens to return a greeting, but who cares what it's doing behind the scenes? If I update the person's name, the greeting changes - yay!

Abstracting away details when possible, in any language, is usually a good thing.

But in Erlang, everything is immutable. You can't change a field after it's been set - you can only return a whole new record. If someone didn't know that fact applied to records, I could understand why. Erlang happens to make it _appear_ that updating a field is possible.

Let's say we have a function in an Erlang module that returns a `class` for us. We call the function to get a `class` record, "modify" a field (but not really), and lastly inspect the original reference again. I put modify in quotes because you're really just creating a new instance, and the original is still referenced by `ClassOne`.

```erlang
c(sample).
rr(sample).

% Get a class record

ClassOne = sample:get_class().
% #class{subject = "forensics", professor = "dr moriarty",
%        students = [#person{name = "joe", grade = "A"},
%                    #person{name = "suzy", grade = "B"}]}

% Update one of the fields, which really just creates a new class record

ClassOne#class {subject = "math"}.
% #class{subject = "math", professor = "dr moriarty",
%        students = [#person{name = "joe", grade = "A"},
%                    #person{name = "suzy", grade = "B"}]}

ClassOne.
% #class{subject = "forensics", professor = "dr moriarty",
%        students = [#person{name = "joe", grade = "A"},
%                    #person{name = "suzy", grade = "B"}]}
```

The Erlang syntax makes it _look_ as if you can update a single field in an existing record - something that would work as expected in C#, Ruby, or other languages - but it's actually creating a new record.

The nice thing about property accessors in other languages is that they appear to update automatically, returning a new value as a result of other fields being modified. But since fields in an Erlang record don't get updated, maybe there isn't a point.

Still... abstraction is a useful thing, so what _can_ we do?

## Can we cobble something together?

A record is really just [syntactic sugar for a tuple](http://learnyousomeerlang.com/a-short-visit-to-common-data-structures#records). Here are two representations of the same data. The first is formatted as a record, but under the covers it's a tuple with the name of the record first, followed by any data it contains.

```erlang
#class{subject = "forensics", professor = "dr moriarty",
       students = [#person{name = "joe", grade = "A"},
                   #person{name = "suzy", grade = "B"}]}

{class, "forensics", "dr moriarty", [{person, "joe", "A"}, {person, "suzy", "B"}]}
```

So we're somewhat limited. The only idea I could come up with was stuffing a function in one of the fields, like this. It accepts an instance of the record, and returns the length of the `students` list. I used `element` because you can't reference a record from within the record itself.

```erlang
-record(class,
        {
            subject :: string(),
            professor :: string(),
            students :: [#person{}],
            number_of_students = fun(Class) -> length(element(4, Class)) end :: integer()
        }).
```

And that produces this beauty. 🤢

```erlang
ClassOne = sample:get_class().
(ClassOne#class.number_of_students)(ClassOne).
% returns 2
```

## What's the _right_ thing to do?

The only reasonable thing you can really do is create some "helper" functions that accept the class you're interested in, and return the information you're looking for.

```erlang
-module(sample).

-export([get_class/0,
         get_classroom_size/1, get_student_names/1]).

-record(person,
        {
            name :: string(),
            grade :: string()
        }).
-record(class,
        {
            subject :: string(),
            professor :: string(),
            students :: [#person{}]
        }).

-spec get_class() -> #class{}.
get_class() ->
    #class {subject = "science",
            professor = "ms frizzle",
            students = [#person { name = "dorothy", grade = "A" },
                        #person { name = "arnold", grade = "B" }]}.

-spec get_classroom_size(#class{}) -> integer().
get_classroom_size(Class) ->
    length(Class#class.students).

-spec get_student_names(#class{}) -> string().
get_student_names(#class{students=Students}) ->
    string:join([S#person.name || S <- Students], ", ").
```

Now you can pass an instance of your record into a helper function, and it'll extract the data you're interested in, and format it the way you'd like. Similar to the property accessors, no one has to worry about what's going on inside the function.

```erlang
c(sample).
rr(sample).

ClassOne = sample:get_class().
% #class{subject = "science", professor = "ms frizzle",
%        students = [#person{name = "dorothy", grade = "A"},
%                    #person{name = "arnold", grade = "B"}]}

sample:get_classroom_size(ClassOne).
% 2

sample:get_student_names(ClassOne).
% "dorothy, arnold"
```

Alternatively, you could add fields to the record to hold the values that are based on other fields, and use helper function to set them along with all the other fields when the record is instantiated.

```erlang
-module(sample).

-export([create_classroom/3]).

-record(person,
        {
            name :: string(),
            grade :: string()
        }).
-record(class,
        {
            subject :: string(),
            professor :: string(),
            students :: [#person{}],
            size :: integer(),
            names :: string()
        }).

-spec create_classroom(string(), string(), [#person{}]) -> #class{}.
create_classroom(Subject, Professor, Students) ->
    #class { subject = Subject,
             professor = Professor,
             students = Students,
             size = length(Students),
             names = string:join([S#person.name || S <- Students], ", ") }.
```

Now you can create the record and populate it with the other fields at the same time.

```erlang
sample:create_classroom("science", "ms frizzle", [#person{name="dorothy",grade="A"}, 
                                                  #person{name="arnold",grade="B"}]).

% #class{subject = "science", professor = "ms frizzle",
%        students = [#person{name = "dorothy", grade = "A"},
%                    #person{name = "arnold", grade = "B"}],
%        size = 2,
%        names = "dorothy, arnold"}
```

## What can we learn from this exercise?

At the end of the day, property accessors are kind of pointless in Erlang because the fields they would provide access to cannot be modified _ever_. Not to mention, there's no concept of a "getter" without a "setter". In other words, someone could create a new instance of the record and overwrite the default value (the function) I assigned it in the record. Perhaps something would've been possible if records weren't a tacked-on afterthought and simply tuples in disguise. 🤔

If what you really want is a way to populate a field during creation of a record, create a helper function like the last example in the previous section. Let the function manipulate the data and populate the fields with the values you'd like.

If what you really want is a way to manipulate a field before getting its value back, create a helper function like the first example in the previous section. Let the function manipulate the data and return the values you're interested in.
