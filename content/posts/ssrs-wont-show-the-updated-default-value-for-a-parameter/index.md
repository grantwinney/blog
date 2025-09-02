---
categories:
  - Coding
date: 2024-01-31T02:40:31Z
description: ""
draft: false
cover:
  image: photo-1591696205602-2f950c417cb9.jpg
slug: ssrs-wont-show-the-updated-default-value-for-a-parameter
summary: Changed the default value for a report parameter, but it's not actually updating in SSRS? That's by design. Let's find a way around it.
tags:
  - ssrs
title: SSRS won't show the updated default value for a parameter
---
I recently had to figure out why a minor change to an SSRS report wasn't deploying correctly. It was just a minor change to the default value of a parameter on one report, nothing special.

One step of the deployment process involves uploading some [RDL](https://learn.microsoft.com/en-us/sql/reporting-services/reports/report-definition-language-ssrs) files via SSRS's [SOAP API](https://learn.microsoft.com/en-us/sql/reporting-services/report-server-web-service/accessing-the-soap-api). I expected the [CreateCatalogItem](https://learn.microsoft.com/en-us/dotnet/api/reportservice2010.reportingservice2010.createcatalogitem?view=sqlserver-2016#reportservice2010-reportingservice2010-createcatalogitem\(system-string-system-string-system-string-system-boolean-system-byte\(\)-reportservice2010-property\(\)-reportservice2010-warning\(\)@\)) endpoint to update the default parameter for the report in SSRS, like it would for other changes. But it didn't.

## Why won't the default value update?

Skip to the next section if you're not interested in the _"why",_ but I was. A few online searches turned up years-old threads with people complaining about this scenario_._ There's a number of suggested fixes, which I'll get to in a minute, but I was curious ... was this a bug or "by design"?

A [post](https://dba.stackexchange.com/a/149666) by a DBA in 2016 gave me the first hint that it might be the latter:

> This is a "known" issue/feature according to Microsoft. Descriptions, parameter defaults, subscriptions, etc. all fall under report "meta data" and are maintained separately on the Report Manager server. The only way to get these to change on the Report Manager is to either manipulate them manually or delete the report and upload anew.

A link in that thread led to a Microsoft Connect (discontinued and deleted.. thanks Microsoft) [post from 2007](https://web.archive.org/web/20130313052333/https://connect.microsoft.com/SQLServer/feedback/details/299372/report-parameter-defaults-not-updated-during-deployment), detailing the same issue I was seeing:

> Parameter defaults do not get updated when re-deploying existing reports. These either have to be updated manually or the reports deleted and re-deployed. The latter regenerates all report ID's (GUID's) and makes traking usage from the ExecutionLog more difficult.  
>   
> This is explained here as being by design however I can't envisage parameter defaults and prompts being maintaned by an administrator. An override mechanism similar to OverwriteDataSources should be added to Reporting Services projects to allow deploymnent from Visual Studio.

That led to [an even older post](https://web.archive.org/web/20100828132548/http://social.msdn.microsoft.com/forums/en-US/sqlreportingservices/thread/c6c5b75a-fcbd-48f4-a30d-852d443d0a74/) from a Microsoft Forums (also discontinued and deleted.. thanks _again_ Microsoft) thread in 2005, with an answer from a SQL Server program manager at MS, which is probably the closest we'll get to an authoritative answer: _(emphasis in Brian's answer is mine)_

> OP: For testing purposes, I had placed default values in my report parameters. I deployed the whole suite of reports once and tested them. Then I eliminated the default parameters and redeployed, but the server doesn't pick up the changes. Other changes are updated fine, but for some reason it's hanging on to my default parameter values? Why? Is there a way around this?  
>   
> Andrew Sears: Perhaps deleting the reports and recreating them would help?  
>   
> OP: Yes, that's what I'm doing. Seems stupid, though. Which table in the ReportServer database contains the default parameter info?  
>   
> Brian Welcker: Modifying the tables directly is not supported. However, you can see this information in the Parameter column in the Catalog table. **The original intent was to allow for an admin to change the defaults, prompts, etc. and not have it overwritten by the report developer. We should provide a way to override this behavior.**

I wish that override would've happened a long time ago, but I guess I've got my answer. Microsoft created separate logic for updating certain aspects of a report, and that logic seems to be preventing the API call from doing what one would expect it to do.

## Can we force the default value to update?

As for solutions, none of the ones I saw were particularly good. One common suggestion is to go in through the web interface for SSRS and change the default in there. If you're into automated deploys though, then manual steps like these are a no-go.

Another oft-suggested workaround is to drop the report and recreate it, but that's a no-go too. SSRS links a lot of things together, and deleting a report will almost certainly delete other records in other tables.. including any [subscriptions](https://learn.microsoft.com/en-us/sql/reporting-services/subscriptions/subscriptions-and-delivery-reporting-services) that have been created against the report.

Yet another workaround is to rename the parameter in the report, but that's still a problem for subscriptions. If one parameter disappears, and a new one materializes out of thin air, how could that _not_ break subscriptions? You and I may know it's a direct replacement, but SSRS won't.

Since the problem seems to be that SSRS ignores default values when a report is updated, it's worth asking the question – what if SSRS didn't have parameters to compare to? That seemed to be the thinking of user JonoB in [another thread](https://stackoverflow.com/a/76174818):

> Instead you can clear the parameters stored in the Database with the following sql, and then re-upload the report in place and it will regenerate the parameters.
> 
> `EXEC [ReportServer].[dbo].[SetParameters] '/MyReportPath/MyReport', NULL`

I tried it out, and as far as I can see, it has no adverse effects, despite Brian's admonition in 2005 that _"modifying the tables directly is not supported"._ There's a lot of built-in stored procs in SSRS, and this one just sets the `dbo.Catalog.Parameter` column for a given report. You can even, as JonoB suggests, pass in `NULL` to clear out the `Parameter` column.

After doing that, running the original API endpoint then repopulates `dbo.Catalog.Parameter` with the correct values.. including any changes to default values! It doesn't seem to touch anything else, other than making a call to the `dbo.FlushReportFromCache` stored proc that updates some cache in the [report server temp database](https://learn.microsoft.com/en-us/previous-versions/sql/sql-server-2008/ms156016(v=sql.100)#report-server-temporary-database).

You might be able to just update the column directly with a simple `UPDATE` statement, but if someone thought it was important to update the cache in the temp db after updating the `Parameter` column, it probably is.

## Final thoughts

I'm annoyed the answer isn't in the API docs, at least not that I found. Instead, the answer is buried in old (thankfully archived) posts on two separate doc sites that Microsoft killed off. I _really_ wish they'd leave this stuff around, or migrate it in a way that forwards the old url to whatever new site they decide to create each year, especially considering how many companies are running legacy software running on their old technologies.

Since it sounds like the differing logic is role-based, I wonder if it's possible to change the role or privileges of the user used to call the API? Or just call the API as an "admin", if possible? For now, calling the `SetParameters` proc before the `CreateCatalogItem` endpoint seems to be enough.

If you find better documentation, or a better solution, feel free to share it below.. I'd love to hear about it!
