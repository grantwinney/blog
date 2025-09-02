---
categories:
  - Questions
date: 2020-12-09T13:19:00Z
description: ""
draft: false
cover:
  image:
slug: what-is-mocking-a-dependency
summary: When you're writing tests, you generally don't want to write to the database, email customers, and hit third-party API's. That's why we need to know how to mock dependencies!
tags:
  - testing
title: What is mocking a dependency?
---
Ever taken your car for an e-check and had it placed on those rollers for testing? It's part of a gadget called a dynamometer, and it's used to test your car at high speeds without having to move it an inch.

Imagine.. you have a car with a problem. It makes a horrible noise and shakes when you hit 60 mph. Being an intrepid individual, you decide to diagnose it yourself. Being a _smart_ individual, you decide not to do it while careening down the highway. You need a way to make the car seem to be going really fast without it actually moving. Hm, those rollers might help!

![](image.png)

That's not a perfect analogy, but in essence that's mocking a dependency.

1. You identify a part of the system to test - something you _can_ control.
2. You identify other parts of the system that it touches that you _can't_ control.
3. You replace those parts, in such a way that the system you're testing never knows.

You don't want your car moving while you diagnose it. And you don't want your app writing records to a database, kicking up prompts for input, or connecting to third-party APIs, while you're testing it. You don’t want your test failing because a network drive is unavailable, a location on disk can’t be written to, or an SMTP server is down.

## Let's get practical

I don't know about you, but examples always help me... so let's take a closer look at a couple.

### Don't write to disk

When you're writing a large app, one of your best friends is the logger. He's not an exciting friend but he's a great listener. He's taking notes of everything you do and say... and can throw them back in your face at a moment's notice. .... ....... 🤨

Anyyyyway... in the .NET world there's a popular logging library called NLog. Let's check out a short example that validates a username and logs some debug info to a file. I removed the configuration part for the logger, but [you can see it all here](https://github.com/grantwinney/BlogCodeSamples/tree/master/Languages/CSharp/MockingDependencies).

```csharp
public class UsernameValidation
{
    private readonly Logger logger;

    public UsernameValidation()
    {
        // ... configure nlog

        // Create new instance of logger
        logger = LogManager.GetCurrentClassLogger();
    }

    public bool IsUsernameAlphaOnly(string username)
    {
        try
        {
            logger.Debug($"{nameof(IsUsernameAlphaOnly)}: Testing whether {username} is valid.");
            var isMatch = Regex.IsMatch(username, "^[A-Za-z]+$");
            logger.Debug($"{nameof(IsUsernameAlphaOnly)}: {username} is {(isMatch ? "a valid" : "an invalid")} username.");
            return isMatch;
        }
        catch (Exception ex)
        {
            logger.Error(ex, $"{nameof(IsUsernameAlphaOnly)}: Guess {username} wasn't valid. :/");
            return false;
        }
    }
}
```

If we run a test against that method, it'll run the logger code too, writing out a log file with debug statements in it. What if the logger can't write to the disk? We're inadvertently testing the NLog library too, and the test could randomly fail for a reason that's out of our control.

```csharp
[TestCase("Bob", Description = "should be valid")]
[TestCase("JDoe1", Description = "should be invalid")]
public void Test1(string username)
{
    var l = new UsernameValidation();

    Assert.True(l.IsUsernameAlphaOnly(username));
}
```

_Test the method with a couple different inputs..._

```none
2020-12-09 12:31:27.2294|DEBUG|MockingDependencies.MockLogger.UsernameValidation|IsUsernameAlphaOnly: Testing whether Bob is valid.
2020-12-09 12:31:27.2583|DEBUG|MockingDependencies.MockLogger.UsernameValidation|IsUsernameAlphaOnly: Bob is a valid username.
2020-12-09 12:31:27.2767|DEBUG|MockingDependencies.MockLogger.UsernameValidation|IsUsernameAlphaOnly: Testing whether JDoe1 is valid.
2020-12-09 12:31:27.2767|DEBUG|MockingDependencies.MockLogger.UsernameValidation|IsUsernameAlphaOnly: JDoe1 is an invalid username.
```

_A log file is written to disk - probably not what we want!_

**Mock out the dependency**

Different languages and frameworks have different ways of mocking out dependencies. In .NET, it usually means mocking out an interface. That's a whole separate topic, but the short version is that an interface is a contract, and you can change the terms of that contract depending on who's running the code, like your user in production or your test framework. The way your test _changes_ those terms is via a mocking framework like [moq](https://github.com/Moq/moq4), [JustMock](https://www.telerik.com/products/mocking.aspx), [TypeMock](http://www.typemock.com/), [RhinoMocks](https://hibernatingrhinos.com/oss/rhino-mocks), etc... lots of options.

The NLog library happens to implement an interface called `ILogger`, and by adding a new constructor and changing a couple lines, we can pass that around our little method instead of the concrete Logger class.

```csharp
public class UsernameValidation_MockLogger
{
    private readonly ILogger logger;

    public UsernameValidation_MockLogger()
    {
        // ... configure nlog

        // Create new instance of logger
        logger = LogManager.GetCurrentClassLogger();
    }

    public UsernameValidation_MockLogger(ILogger logger)
    {
        this.logger = logger;
    }

    public bool IsUsernameAlphaOnly(string username)
    {
        try
        {
            logger.Debug($"{nameof(IsUsernameAlphaOnly)}: Testing whether {username} is valid.");
            var isMatch = Regex.IsMatch(username, "^[A-Za-z]+$");
            logger.Debug($"{nameof(IsUsernameAlphaOnly)}: {username} is {(isMatch ? "a valid" : "an invalid")} username.");
            return isMatch;
        }
        catch (Exception ex)
        {
            logger.Error(ex, $"{nameof(IsUsernameAlphaOnly)}: Guess {username} wasn't valid. :/");
            return false;
        }
    }
}
```

Then we modify the tests to create a "fake" (mock) logger, and use that in the method instead. Mocking frameworks are really powerful, but I'm not touching on any of that here. This is enough to cause the log statements to "succeed" as far as the other class is concerned, even though it's actually doing nothing. No log file is written to disk!

```csharp
[TestCase("Bob", Description = "should be valid")]
[TestCase("JDoe1", Description = "should be invalid")]
public void Test1(string username)
{
    var mock = new Mock<ILogger>();

    var l = new UsernameValidation_MockLogger(mock.Object);

    Assert.True(l.IsUsernameAlphaOnly(username));
}
```

### Don't read from disk

Something else that's pretty common in programming is reading from a file - maybe a configuration file or an ini (initialization) file. So here's another example, that reads an XML file from disk to find the price of a book _(_[_thanks Microsoft_](https://docs.microsoft.com/en-us/previous-versions/windows/desktop/ms762271(v=vs.85))_)._

```csharp
public class Books
{
    private readonly XDocument xDoc;

    public Books()
    {
        xDoc = XDocument.Load(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "MockXDocument", "books.xml"));
    }

    public decimal? GetPrice(string bookId)
    {
        var book = xDoc.Descendants("book")
                        .Where(x => x.Attribute("id").Value == bookId)
                        .SingleOrDefault();

        return decimal.TryParse(book?.Element("price")?.Value, out decimal price) ? price : (decimal?)null;
    }
}
```

If we run it with the following test, it'll read the file from disk. That's risky when you're testing. What if the file isn't present? What if an antivirus scanner gobbles it up? What if it's on a network drive, and someone moves it? All that stuff is outside your control, and all you really wanted to do was make sure you can return the correct price.

```csharp
public class Books_Tests
{
    Books books;

    [SetUp]
    public void Setup()
    {
        books = new Books();
    }

    [TestCase("bk102", 5.95)]
    [TestCase("bk111", 36.95)]
    [TestCase("bk999", null, Description = "not a book")]
    public void Test1(string bookId, decimal? bookPrice)
    {
        Assert.AreEqual(bookPrice, books.GetPrice(bookId));
    }
}
```

**Mock out the dependency**

This time, the `XDocument` doesn't implement an interface that we can easily take advantage of, so we'll have to do something else instead. One option would be to wrap the class we want to mock in a new class, and create the interface for our new class to implement, like this:

```csharp
public interface IXDocument
{
    IXDocument Load(string fileName);

    IEnumerable<System.Xml.Linq.XElement> Descendants(System.Xml.Linq.XName name);
}

public class XDocument : IXDocument
{
    private System.Xml.Linq.XDocument XDoc;

    public IXDocument Load(string uri)
    {
        XDoc = System.Xml.Linq.XDocument.Load(uri);
        return this;
    }

    public static IXDocument LoadEx(string fileName)
    {
        return new XDocument().Load(fileName);
    }

    public IEnumerable<System.Xml.Linq.XElement> Descendants(System.Xml.Linq.XName name)
    {
        return XDoc.Descendants(name);
    }
}
```

I won't go into too many details about the above, except to say that we create our own `XDocument` class that wraps the .NET class of the same name, and implements an interface that we can use with the moq mocking library. Just like the first example, all it takes is a new constructor that accepts the interface and a couple other extra lines.

```csharp
private readonly IXDocument xDoc;

public Books_MockXDocument()
{
    xDoc = XDocument.LoadEx(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "MockXDocument", "books.xml"));
}

public Books_MockXDocument(IXDocument xDoc)
{
    this.xDoc = xDoc;
}

public decimal? GetPrice(string bookId)
{
    var book = xDoc.Descendants("book")
                    .Where(x => x.Attribute("id").Value == bookId)
                    .SingleOrDefault();

    return decimal.TryParse(book?.Element("price")?.Value, out decimal price) ? price : (decimal?)null;
}
```

Here's the test again, now feeding the exact XML to our app that we want. What could've failed randomly before is now completely in our control!

```csharp
Books_MockXDocument books;
Mock<IXDocument> mockDoc;

[SetUp]
public void Setup()
{
    mockDoc = new Mock<IXDocument>();
    books = new Books_MockXDocument(mockDoc.Object);
}

[TestCase("bk102", 5.95)]
[TestCase("bk111", 36.95)]
[TestCase("bk999", null)]
public void Test1(string bookId, decimal? bookPrice)
{
    if (bookPrice.HasValue)
    {
        var testBook = $@"<book id=""{bookId}""><price>{bookPrice}</price></book>";

        mockDoc.Setup(x => x.Descendants("book"))
            .Returns(new List<XElement> { System.Xml.Linq.XDocument.Parse(testBook).Root });
    }

    Assert.AreEqual(bookPrice, books.GetPrice(bookId));
}
```

## Be mindful of what you control.. and what you don't!

I could come up with more examples - there's lots out there to consider - but hopefully these ones drive the point home well enough. When it comes to testing:

- Don't rely on hardware, like physical drives, emails servers, etc.
- Don't rely on software outside your control, like third-party APIs.
- Tests should be repeatable, reliable, _and completely in your control_. When they fail, you should know exactly why, and it should happen consistently.
- When you're dependant on something that's outside your control, look for a way to mock it out. You may have to get creative, but it'll almost certainly make your tests more reliable, which is a huge peace of mind!
