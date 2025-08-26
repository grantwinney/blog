---
categories:
- SSRS
date: "2024-01-31T02:40:31Z"
description: ""
draft: false
cover:
  image: https://images.unsplash.com/photo-1591696205602-2f950c417cb9?crop=entropy&cs=tinysrgb&fit=max&fm=jpg&ixid=M3wxMTc3M3wwfDF8c2VhcmNofDMzfHxyZXBvcnRpbmd8ZW58MHx8fHwxNzA2NTg5MTAyfDA&ixlib=rb-4.0.3&q=80&w=2000
slug: ssrs-wont-show-the-updated-default-value-for-a-parameter
summary: Changed the default value for a report parameter, but it's not actually updating
  in SSRS? That's by design. Let's find a way around it.
tags:
- SSRS
title: SSRS won't show the updated default value for a parameter
---


I recently had to figure out why a minor change to an SSRS report wasn't deploying correctly. It was just a minor change to the default value of a parameter on one report, nothing special.

One step of the deployment process involves uploading some RDL files via SSRS's SOAP API. I expected the CreateCatalogItem endpoint to update the default parameter for the report in SSRS, like it would for other changes. But it didn't.


Why won't the default value update?

Skip to the next section if you're not interested in the "why", but I was. A few online searches turned up years-old threads with people complaining about this scenario. There's a number of suggested fixes, which I'll get to in a minute, but I was curious ... was this a bug or "by design"?

A post by a DBA in 2016 gave me the first hint that it might be the latter:

This is a "known" issue/feature according to Microsoft. Descriptions, parameter defaults, subscriptions, etc. all fall under report "meta data" and are maintained separately on the Report Manager server. The only way to get these to change on the Report Manager is to either manipulate them manually or delete the report and upload anew.

A link in that thread led to a Microsoft Connect (discontinued and deleted.. thanks Microsoft) post from 2007, detailing the same issue I was seeing:

Parameter defaults do not get updated when re-deploying existing reports. These either have to be updated manually or the reports deleted and re-deployed. The latter regenerates all report ID's (GUID's) and makes traking usage from the ExecutionLog more difficult.

This is explained here as being by design however I can't envisage parameter defaults and prompts being maintaned by an administrator. An override mechanism similar to OverwriteDataSources should be added to Reporting Services projects to allow deploymnent from Visual Studio.

That led to an even older post from a Microsoft Forums (also discontinued and deleted.. thanks again Microsoft) thread in 2005, with an answer from a SQL Server program manager at MS, which is probably the closest we'll get to an authoritative answer: (emphasis in Brian's answer is mine)

OP: For testing purposes, I had placed default values in my report parameters. I deployed the whole suite of reports once and tested them. Then I eliminated the default parameters and redeployed, but the server doesn't pick up the changes. Other changes are updated fine, but for some reason it's hanging on to my default parameter values? Why? Is there a way around this?

Andrew Sears: Perhaps deleting the reports and recreating them would help?

OP: Yes, that's what I'm doing. Seems stupid, though. Which table in the ReportServer database contains the default parameter info?

Brian Welcker: Modifying the tables directly is not supported. However, you can see this information in the Parameter column in the Catalog table. The original intent was to allow for an admin to change the defaults, prompts, etc. and not have it overwritten by the report developer. We should provide a way to override this behavior.

I wish that override would've happened a long time ago, but I guess I've got my answer. Microsoft created separate logic for updating certain aspects of a report, and that logic seems to be preventing the API call from doing what one would expect it to do.


Can we force the default value to update?

As for solutions, none of the ones I saw were particularly good. One common suggestion is to go in through the web interface for SSRS and change the default in there. If you're into automated deploys though, then manual steps like these are a no-go.

Another oft-suggested workaround is to drop the report and recreate it, but that's a no-go too. SSRS links a lot of things together, and deleting a report will almost certainly delete other records in other tables.. including any subscriptions that have been created against the report.

Yet another workaround is to rename the parameter in the report, but that's still a problem for subscriptions. If one parameter disappears, and a new one materializes out of thin air, how could that not break subscriptions? You and I may know it's a direct replacement, but SSRS won't.

Since the problem seems to be that SSRS ignores default values when a report is updated, it's worth asking the question – what if SSRS didn't have parameters to compare to? That seemed to be the thinking of user JonoB in another thread:




Instead you can clear the parameters stored in the Database with the following sql, and then re-upload the report in place and it will regenerate the parameters.



EXEC [ReportServer].[dbo].[SetParameters] '/MyReportPath/MyReport', NULL





I tried it out, and as far as I can see, it has no adverse effects, despite Brian's admonition in 2005 that "modifying the tables directly is not supported". There's a lot of built-in stored procs in SSRS, and this one just sets the dbo.Catalog.Parameter column for a given report. You can even, as JonoB suggests, pass in NULL to clear out the Parameter column.

After doing that, running the original API endpoint then repopulates dbo.Catalog.Parameter with the correct values.. including any changes to default values! It doesn't seem to touch anything else, other than making a call to the dbo.FlushReportFromCache stored proc that updates some cache in the report server temp database.

You might be able to just update the column directly with a simple UPDATE statement, but if someone thought it was important to update the cache in the temp db after updating the Parameter column, it probably is.


Final thoughts

I'm annoyed the answer isn't in the API docs, at least not that I found. Instead, the answer is buried in old (thankfully archived) posts on two separate doc sites that Microsoft killed off. I really wish they'd leave this stuff around, or migrate it in a way that forwards the old url to whatever new site they decide to create each year, especially considering how many companies are running legacy software running on their old technologies.

Since it sounds like the differing logic is role-based, I wonder if it's possible to change the role or privileges of the user used to call the API? Or just call the API as an "admin", if possible? For now, calling the SetParameters proc before the CreateCatalogItem endpoint seems to be enough.

If you find better documentation, or a better solution, feel free to share it below.. I'd love to hear about it!
