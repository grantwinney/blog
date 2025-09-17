---
categories:
- Builds
date: "2015-01-15T00:45:22Z"
description: ""
draft: false
slug: csharp-extension-method-to-pass-column-name-to-getfieldvalue
summary: Let's combine the SqlDataReader's GetFieldValue and GetOrdinal methods into an extension method that lets us pass a column name and get back a specific type.
tags:
- csharp
- coding
title: An Extension Method to Pass a Column Name to SqlDataReader.GetFieldValue
---
The [SqlDataReader.GetFieldValue](http://msdn.microsoft.com/en-us/library/hh485652\(v=vs.110\).aspx) method uses generics to return the value of a column as the requested data type, which is nice, but it also requires us to know and pass the column index instead of just using its name, which is less nice.

Let's see if we can do better with a simple extension method.

> If you'd like to follow along while you read, the code in this article is available on <a href="https://github.com/grantwinney/BlogCodeSamples/tree/master/Languages/CSharp/SqlDataReaderGetFieldValueByName">GitHub</a>.

## GetFieldValue Only Accepts a Column Index

The `GetFieldValue<T>()` method requires us to know and pass the index of the column we're interested in:

```csharp
using var conn = new SqlConnection("yourConnectionString");
using var cmd = new SqlCommand("SELECT name, age FROM students", conn);
conn.Open();

using var dr = cmd.ExecuteReader();
var name = dr.GetFieldValue<string>(0);
var age = dr.GetFieldValue<int>(1);
```

What happens if the query changes? What if an additional column is added to (or removed from) the beginning, or the existing columns are swapped? It's not hard to imagine causing an `InvalidCastException` or `IndexOutOfRangeException`.

On the up side, at least it casts the object to the type we specify before returning it.

## The Indexer Accepts a Column Name

We can also access the column value using an indexer, passing either the index of the column or its name:

```csharp
using (var conn = new SqlConnection("yourConnectionString"));
using (var cmd = new SqlCommand("SELECT name, age FROM students", conn));
conn.Open();

using (var dr = cmd.ExecuteReader());
var name = dr["name"].ToString();
var age = Convert.ToInt32(dr["age"]);
```

Internally, this runs pretty much the same code as the above method, except it doesn’t cast to a particular data type, so all you get back is an object that you have to convert yourself.

## An Extension Method to Combine Both

Let's combine both of these methods into a single extension method, so we can both specify the return data type _and_ reference the column name instead of the index:

```csharp
public static class SqlReaderExtensions
{
    /// <summary>
    /// Gets the value of the specified column as a type, given the column name.
    /// </summary>
    /// <typeparam name="T">The expected type of the column being retrieved.</typeparam>
    /// <param name="reader">The reader from which to retrieve the column.</param>
    /// <param name="columnName">The name of the column to be retrieved.</param>
    /// <returns>The returned type object.</returns>
    public static T GetFieldValue<T>(this SqlDataReader reader, string columnName)
    {
        return reader.GetFieldValue<T>(reader.GetOrdinal(columnName));
    }
}
```

And here's how to use it:

```csharp
using var conn = new SqlConnection("yourConnectionString");
using var cmd = new SqlCommand("SELECT name, age FROM students", conn);
conn.Open();

using var dr = cmd.ExecuteReader();
var name = dr.GetFieldValue<string>("name");
var age = dr.GetFieldValue<int>("age");
```
