---
layout: post
title: Why you need version control
date: 2016-09-16
tag: 
   - R
   - CodingStyle
   - WorkRelated
description: Using version control software is a defining sign of a professional approach to serious data analysis
image: /img/0055-ggplot2-commits.png
socialimage: http://ellisp.github.io/img/0055-ggplot2-commits.png
category: R
---

I recently had an email exchange with a seasoned, well respected analytical professional which included the following (from them, not me): "... my versioning is to have multiple versions of files and to use naming conventions... it works really well."

This is a very smart, competent researcher who has delivered great results, doing innovative and influential work albeit generally in very small teams of one or two people doing coding.  I'm sure the naming conventions *do* work for them, and by any reasonable definition they are highly successful with that approach.  But it wouldn't work in my world with my sort and scale of tasks.  I wouldn't run my blog that way, never mind a critical project in my day job.  In this blog post I want to explain why I think that analysts using naming conventions should move to version control software like [Git](https://git-scm.com/) or [Subversion](https://subversion.apache.org/).

## A classification of analysts

In my observation, it's possible to divide analysts (and analytics teams) into four categories:

1. Those for whom most work is done in Excel, particularly data management and presentation and visualisation of results.  Specialist tools like SAS, Stata, Matlab, R or E-views used only for statistical/econometric modelling tasks that are beyond Excel.

2. Those for whom most work, including data management and visualisation, is done in a coding language which is usually one of R, SAS or Python plus SQL, but version control is limited to copies of files and date-stamped naming conventions.

3. Like #2, but with version control software, a coding style guide, and thorough peer review processes.

4. Like #3, but with more good practice borrowed from the software development world - like unit testing, regression testing, validation checks, release cycles, modular code, high coverage documentation, and continuous integration.

Obviously this is a value-laden list for me, and while people in all four categories do important and useful work I think those analysts in categories 3 and 4 are much more valuable than those in 1 and 2.  In particular, managing a team of analysts who aim to be continuously in the best practice category, I can have confidence in efficiency, quality control, ability to scale up to very large and complex data-intensive problems, and institutional memory and sustainability.  

All the progressions between these categories are important (and involve a cultural shift with big changes in work practices needed), but I think a threshhold of professionalism is really crossed in the progression from category #2 to #3 - the adoption of version control and the introduction of some basic disciplines around coding style and review.  

The good news is that no-one is born in any of these categories and progression is actually fairly easy.  Most of my life I was in category #1 (and of course, I didn't think in those terms then - not knowing any better being one of the most noticeable features of that state).  

> "I only recruit people who are in category #2 or beyond..."

Most people with "analyst" in their job title - to my admittedly informal observation - are in the most basic of my four categories.  But I think that is changing.  The barriers to entry for learning R (in particular) are much less than ten years ago, with a wealth of good material online, much of it free; and much more powerful and user-friendly tools such as [Hadley Wickham's tidyverse](http://r4ds.had.co.nz/introduction.html) to make coding easier.   Now for my hands-on analytical roles, I only recruit people who are in category #2 or beyond when they come to us, and I expect them to be in the best practice category #4 within a month or two of joining the team.



## Why naming conventions don't cut it

Here's why that move from category #2 to #3, with the introduction of version control software, is so important.  Saving multiple versions of files with different names isn't good enough because...

### It doesn't scale up to projects with many files of code

A 3,000 line single script may work for a one-off project but is basically impossible to maintain, and will lead to many inefficiencies.  Plus it's difficult for multiple analysts to contribute to at once without stepping on toes.  So analytical projects of even modest complexity need multiple scripts.  You'll want to separate out project-enabling infrastructure (eg functions that are going to be used multiple times, and graphics themes); data prep; analysis; and presentation / communication.  All this should be held together with something like [make](https://en.wikipedia.org/wiki/Makefile) or rake, or (at minimum) a master script that calls the other scripts.

It's obvious how this will fall apart if you rely on re-naming your files for your version control.  At the very least it will make re-creating a past state a very difficult manual task; more likely the whole system (or your understanding of it) will collapse under it's own weight.

### It doesn't scale up to projects with multiple contributors

Version control lets you track who is making what additions and deletions to the code base.  If things go wrong, it makes it easier to find out who broke it, and easier to reverse what they did that broke it.  But more importantly, it provides a safe way for different analysts to be working simultaneously on their own versions, and then merge their changes together.

> "But if you don't have source control, you're going to stress out trying to get programmers to work together. Programmers have no way to know what other people did. Mistakes can't be rolled back easily. The other neat thing about source control systems is that the source code itself is checked out on every programmer's hard drive -- I've never heard of a project using source control that lost a lot of code."

*From Joel Sposky's [The Joel Test: 12 Steps to Better Code](http://www.joelonsoftware.com/articles/fog0000000043.html).  See also this [more recent reflection from Coriander Technologies](http://www.coriandertech.com/2011/11/05/the-joel-test-is-antiquated/) on which parts of The Joel Test re less relevant now, particularly since the rise of Agile.  Regardless, I often argue that the Joel Test applies to data dev teams as much as software developers.  Less than 12 / 12 and you're in a bit of trouble.  Less than 9 and you're in big trouble.*

Version control software is the only way for coding projects to scale up.  At least 10,000 developers, most of whom have never met eachother, work on [Linux development](http://www.zdnet.com/article/who-writes-linux-almost-10000-developers/), for example.  Try keeping track of that without version control.  Analytical projects don't get this big of course, but actually many of the problems with large numbers of contributors are already there once there are three of you.  And projects with three or more analysts on them are going to become much, much more common - the single academic coding by themselves is not the way of the future in data science.

If that's not reason enough, remember that you're always coding with at least two other collaborators: one or more iterations of past you, and one or more iterations of future you.  Just like you write documentation for those *alter egos*, you need version control to sort out who did what, when a change was introduced and why, and how to revert it when one of the iterations of past you turns out to have broken something like the idiot you know they were.

### It doesn't work with frequently maintained code bases

If you write a piece of analytical code and then pull it out every couple of years to update it for changed data, you might get away with relying on naming conventions.  But if you want to run it more frequently, add bits of functionality between runs, keep a production branch that works safely separate from a dev branch that doesn't, etc. etc. - you should use the tools designed to make it possible.

### It doesn't track small changes

Related to the previous point, if changes are big and chunky it might be ok to have "my-analysis-final-2016.R" but then what do you call the small enhancement that comes to you in a dream the next day?  Maybe you could get some naming system that works for all those small changes, but will you stick to it?  And why not use software that looks after that, and gives you a visual colour-coded comparison of the differences and the commit messages explaining them?

### The conventions break down in a review-development cycle

Have any of you seen documents with names like "my-analysis-1.1-final-post-comments-JANEsAdditions-v2-finaldraft.doc"?  Of course you have.  Don't let your analytical scripts end up like that.

### It doesn't support continuous integration

[Continuous integration](https://en.wikipedia.org/wiki/Continuous_integration) (or at least very frequent integration) is the twenty first century descendent of Joel's requirement that the team builds the whole project every day.  Good analytical teams borrow the concept to frequently re-build their project end to end to be sure it all works.  This applies whether you're building an package of functionality and data for others to use (see for example [the Travis CI build report for my nzelect R package](https://travis-ci.org/ellisp/nzelect)) or it's a piece of data grooming, munging and analysis with a Shiny app and PDF at the end, like [this example from my team at work](http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/international-tourism-forecasts).  There's software that helps do this, and it generally assumes you're using version control of the source code.

## Summing up

So, if you're coding in an analytical language like R or SAS but not using version control, give it a go.  It's easy to learn, and the few hours spent getting your head around it are justified by shifting you up a level of professional competence.  Your code will be more disciplined and powerful, and easier for you to maintain, adapt, and scale up; you'll be able to work with collaborators; you'll get way more efficient and quick in development; and you'll save yourself a lot of frustrations.  And of course, it's all open source and free.

> "The single academic coding by themselves is not the way of the future in data science."

I suspect most of my readers don't need the message.  The importance of version control is pretty widely recognised and is now one of the basic things taught in courses like the Johns Hopkins [The Data Scientist's Toolbox](https://www.coursera.org/learn/data-scientists-tools) on Coursera, and in [software carpentry](http://software-carpentry.org/) and [data carpentry](http://www.datacarpentry.org/) courses aimed at (previously) non-programming researchers.  But it's not yet reached all analysts and analytical teams, many of which are small and isolated from good practice.  There'll be a transition period I imagine with people like my original correspondent reluctant to embrace more collaborative approaches; but as analytics teams increase in size and expectations on them continue to grow, I think it won't be possible to operate in the field without version control as part of the toolkit.


*Note: the image associated with this post on the contents page and in social media "shares" is a screenshot from the [GitHub page of ggplot2](https://github.com/hadley/ggplot2).  I just needed an image, I'm not associated with that page in any way other than as one of the millions of avid users of ggplot2...*
