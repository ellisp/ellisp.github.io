---
layout: post
title: New Zealand Data & APIs on GitHub
date: 2015-08-01
---

## New data listing for New Zealand
Wellington's [PrototypeAlex](https://github.com/PrototypeAlex) has created a new [GitHub repository aiming to list data about New Zealand](https://github.com/PrototypeAlex/new-zealand-data).  My first reaction was "why?"  

After all, we already have [data.govt.nz](http://data.govt.nz) which is meant to be the definitive aggregator of government datasets; we have the [Figure.NZ](http://figure.nz) (recently rebranded from "Wiki New Zealand", as it isn't really a Wiki and has evolved into something else, still very useful, entirely) which aims to be: 

> "the place to play with New Zealand's data".  

And according to [PrototypeAlex on Twitter:](https://twitter.com/PrototypeAlex/status/626689156744265728) 

> "There's a heap of NZ data/api metalists floating around, so I made another one."

I think (and am pretty sure so does PrototypeAlex) that the difference is being on GitHub.  This means that anyone with a GitHub account can fork his repository, make and commit additions or corrections, and submit a pull request to him.  I've already done this twice for a bunch of small things I noticed, as half a dozen othbers, and he's pretty prompt at accepting those changes.  In effect, Alex can start crowd sourcing his list.  Maybe he was inspired by Wiki New Zealand dropping the "Wiki" from their name, because that's what he's doing.  It's simple but effective.

It's a good initiative and I hope it goes well, but there's a few things to work on.  The fact that it's difficult is shown by the problems others have had.  For example both data.govt.nz and opengovt.org.nz try to be catalogues of New Zealand government (national and local) datasets.  Both now show up as links from PrototypeAlex's list (and I think probably rightly - if not, I'd probably be obligated to submit a pull request to correct it, rather than blog about it myself).  Both have a number of out of date descriptions, broken links, and the like.

Reflecting on all this, here's what I think would make the best list possible:

1. The list itself should be machine-readable - in fact it should be a database itself, but a super simple one, say a [pipe delimited text file](https://answers.yahoo.com/question/index?qid=20081022042708AADr82V).  I don't think any of the options do this yet.
2. It should have one dataset per entry.  opengovt.org.nz and data.govt.nz aspire to this but don't always achieve it (mostly when the hosts of the data have an ambiguous setup about what is a dataset).
3. Each entry should have metadata - is it accessible by CSV, API, Excel.  Ideas of what constitutes a 'dataset' clearly vary.  Quite a few of the links I've tried from the various aggregation sites this morning go to web pages that present datasets in human-oriented summary tables or interactive web-apps, but don't have an obvious way of downloading the whole thing for re-use.  In my view being able to download the whole thing is the minimum requirement for a dataset to be said to be available.  Summary tables doesn't allow this, and in fact most APIs don't facilitate it either.
4. It should be correctable by the crowd.  The new GitHub version allows this but not others that I'm aware of.  Facilitating this is another reason why a text file is the ideal way to store this particular datasets of datasets; technology like Git makes it easy to see who is adding what, and keep track of versions, when it's all in text.

### Example: characteristics of recipients of main benefits
So, inspired by looking at these lists, I had a go at finding some data I could use.  I wanted something I knew nothing about, that was definitely not connected to my work, and I could do an end-to-end demo of using open data in pretty limited time.  After dealing with a few broken links and data-that-wasn't-really-data I chose more or less at random the [Ministry of Social Development's Five Year National level datatables](https://www.msd.govt.nz/about-msd-and-our-work/publications-resources/statistics/benefit/index.html#Datatables6).  Thanks to MSD for making this available


