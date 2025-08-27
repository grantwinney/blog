---
categories:
- Software Design
- Questions
date: "2020-05-02T03:59:22Z"
description: ""
draft: false
cover:
  image: private-20115_1920.jpg
slug: the-law-of-demeter-a-practical-example
tags:
- Software Design
- Questions
title: What is the Law of Demeter?
---
At work, we're running through [The Pragmatic Programmer](https://amzn.to/2KNdr4i) - the original, not the 2nd edition published last year. If anyone is reading that, I'd love to know if it really updates things for modern programming and whether it seems necessary. The original seems pretty timeless.

Yesterday was my turn to present, listing out highlights from chapter 5, sharing some thoughts, and hopefully spurring some conversation. The authors start by talking about the Law of Demeter, but they don't explain it very well, nor do they call it by its much more self-explanatory name, the Principle of Least Knowledge.

## The Principle of Least Knowledge

I like this name a lot more. I've no idea who Demeter was, or why he was making laws. And while there seems to be a set of rules to follow, I think the spirit of the thing leaves it open to interpretation based on individual circumstances.

![](https://grantwinney.com/content/images/2020/05/pirates.jpg)

Basically, it's about classes (modules, libraries, whatever) not exposing more about themselves than needed for other classes to use them. The term the authors use is writing "shy" code, in that a piece of code shouldn't interact more than it has to, and shouldn't let other bits of code see more than they have to.

I found a great example in an article written by [David Bock](https://www.linkedin.com/in/davidbock/), called [The Paperboy, the Wallet, and the Law of Demeter](https://www2.ccs.neu.edu/research/demeter/demeter-method/LawOfDemeter/paper-boy/demeter.pdf). He presents a fictional story, where a paperboy needs to collect money from one of his customers, and he represents the process in code with something kinda like this, which I think is something most of us have seen.

```csharp
var paid = new Customer().GetWallet().GetPayment();
```

The problem is that, in real life, would the paperboy really grab the customer's wallet and get money from it? Why doesn't the customer just hand the money over, which in code might mean the `Customer` class has a `GetPayment` method that hides the fact that internally there's a wallet at all. Later on, if the wallet is replaced with piggy bank, the paperboy doesn't know or care... he still gets paid!

## Celebrate good times, come on!

All that stuff I said above was a generalization of David's, so I highly recommend reading his article if you want to learn more. Since you're here anyway, I'll throw my own example into the ring. Sometimes we need to see something from several slightly different angles before it clicks.

Imagine you're at a company, working on an application that all the employees use. It's got financial tools built in, and sales tools too; you can administer users and permissions, and run all manner of reports. This is far more common than you might realize, at least for companies with a few hundred employees.

A new request comes in - employees feel underappreciated, so management wants a report of anniversaries for the upcoming week, and new features that let them order a cake. And send an email. Maybe at the same time. Hey, they care but they've got other stuff to do too.

The first thing you do is dig up the `Employee` class, because it's got to exist __somewhere__ in the codebase... ah, there it is, with the usual fields attributed to an employee...

```csharp
public class Employee
{
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public string SSN { get; set; }
    public decimal Salary { get; set; }
    public DateTime HireDate { get; set; }
    public DateTime? TerminationDate { get; set; }
    public bool IsActive { get { return !TerminationDate.HasValue; } }
}
```

Now we need a `Celebration` class, to store logic for properly celeberating them employees' anniversaries and whatnot.

```csharp
public class Celebration
{
    public Employee Employee { get; private set; }
	
    public Celebration(int empId)
    {
        // somehow we load the employee's data
        Employee = dbContext.Students.Get(empId);
    }

    public void PurchaseCake()
    {
        // order a costco cake
    }
    public void PurchaseDeluxeCake()
    {
        // order a dairyqueen icecream cake
    }
    public void SendCard()
    {
        // fire off a hallmark card
    }
}
```

Since we've made the instance of `Employee` public, it'll make things really easy for us to test certain conditions when it comes time to purchase those treats and send all that paper.

```csharp
var celebrate = new Celebration(1234);

if (celebrate.Employee.IsActive
	&& celebrate.Employee.HireDate.Date == DateTime.Now.Date)
{
	if (celebrate.Employee.HireDate.Year + 9 < DateTime.Now.Year)
		celebrate.PurchaseDeluxeCake();  // 10 years gets you the big cake
	else if (celebrate.Employee.HireDate.Year < DateTime.Now.Year)
		celebrate.PurchaseCake();
	else
		return;

	celebrate.SendCard();
}
```

But wait a sec. Why does whatever piece of code that's checking for celebrations need to have access to the `Employee` class? Say this weren't code, and a manager needed to check an employee's record manually to view their hire date. Say someday all this gets offloaded to someone __other__ than a manager, like the newly-formed party-planning committee. Is it reasonable that, just to send you a card and order your cake, a person would need access to your entire employee record including salary and social security number? Noooo. No it is not.

The Principle of Least Knowledge challenges us to rethink how much access Class A has, through Class B, to Classes C, D, and E. In other words, we should hide any details about the `Employee` class that don't need to be exposed - even hide the fact that there's an instance of `Employee` in there at all.

If we make the `Employee` class private inside `Celebration`, it forces us to refactor the rest of the class so that it never makes the rest of the employee's data available. The class instantiating `Celebration` could presumably access employee data anyway, but the programmer behind it would have to deliberately instantiate it.

```csharp
public class Celebration
{
    private Employee employee;
	
    public Celebration(int empId)
    {
        // somehow we load the employee's data
        employee = dbContext.Students.Get(empId);
    }
	
    public bool IsEmployeeAnniversary()
    {
        return employee.IsActive
            && employee.HireDate.Date < DateTime.Now.Date;
    }
    
    public void PurchaseCake()
    {
        if (employee.HireDate.Year + 9 < DateTime.Now.Year)
            // send for a dairyqueen cake
        else
            // send for a costco cake
    }
    public void SendCard()
    {
        // fire off a hallmark card
    }
}
```

This has loads of positive effects on the codebase, including:

- Simplifying the logic in the caller, which no longer has to figure out how to decide if it's the employee's anniversary, just call methods and let another class do the work.
- If the same code as below is called anywhere else, then the above changes [DRY](https://dzone.com/articles/is-your-code-dry-or-wet) up the code base too, keeping the logic in one place.
- And if the logic inside the `Celebration` code changes - maybe some other criteria goes into determining an anniversary, or being at the company __20__ years gets you a Cheesecake Factory cake - then anything else in the codebase that happens to run code like the code below won't need to be touched. Nice!

```csharp
var celebrate = new Celebration(1234);

if (celebrate.IsEmployeeAnniversary())
{
    celebrate.PurchaseCake();
    celebrate.SendCard();
}
```

Thanks Demeter, for your useful laws. I owe you a cake and a card.