---
layout: post
title: A (not too talkative) twitterbot is born
date: 2015-12-29
tag: 
   - DataFromTheWeb
   - Bots
   - R
description: I've set up a twitter bot who (infrequently) retweets popular tweets about rstats with praise, repeats assorted R wisdom, and babbles about R.
image: /img/0027-cloud.png
socialimage: http://ellisp.github.io/img/0027-cloud.png
category: R
---
## Twitter bots
One of my holiday projects was to get my head around how Twitter bots work, and the best (only?) way of course is to make one.  Apparently [about one in seven Twitter accounts is a computer program](http://www.techtimes.com/articles/12840/20140812/twitter-acknowledges-14-percent-users-bots-5-percent-spam-bots.htm) rather than a human user (although of course there are humans behind, and responsible, for all the robots).  That's 23 million bots (and that figure is 16 months old).

Some of these bots are doing useful things (for a broad enough definition of useful - this is Twitter, after all), like [@deepselfie](https://twitter.com/deepselfie) by Andrej Karpathy which will [rate the quality of selfie photos](http://karpathy.github.io/2015/10/25/selfie/). Others are just annoying like [datascience_tn](https://twitter.com/datascience_tn), a presumably well meaning attempt to promote discussion on data science and set up a community in Tunisia, but which spams a whole bunch of hashtags with tweets and retweets at a rate approximating one post per second (I don't know for sure that @datascience_tn is a computer program, but if it's a human they've got some pretty fast typing and clicking abilities and indefatigable patience).

While some of those bots are useful, five percent of all Twitter users are spam bots, that aren't useful to anyone.

### Ethics
People are morally responsible for what is done by the things they create and unleash on the world, so it was with a bit of trepidation that I set out to do anything that would clutter an already cluttered twitterverse.  

My first idea had been to create a sort of "gossip-bot", to see how realistic I could make an automated follower of celebrity - retweeting trending posts, passing on stories, making them up... I quickly realised that could go horribly wrong.

Several ethical obligations seem to spring to mind:

* not to clutter up hash tag channels with excessive spam-like posts and re-tweets
* not to post anything untrue that could be mistaken by anyone reading it (ie anyone in the world) as true and libellous or mean
* not to accrue any personal gains through deception (eg by creating a bot to systematically retweet and favourite my own tweets and make me look more popular and influential than I am).

There's probably more.

### HappyRrobot
So, bearing that in mind, I gave myself a modest objective, enough to build the skills I wanted in this space and do something whimsical and amusing (at least for me) without doing any damage.  I chose the general theme of R and the #rstats hash tag as a starting point, hoping that at least more people in that space would appreciate the joke than get upset.  And so [HappyRrobot](https://twitter.com/HappyRrobot) was born.  

So far this is what it can do:

* Twice a day it searches the #rstats channel and retweets, with random praise, one of the more popular recent status updates with that hash tag
* Once a day it updates its own status with one of:
  - An [R `fortune` cookie](https://cran.r-project.org/web/packages/fortunes/index.html) (limited to those shorter than 140 characters)
  - A babbled, mixed up extract of the [R installation and administration](https://cran.r-project.org/doc/manuals/r-release/R-admin.html) guide
  - A babbled, mixed up extract of the [An Introduction to R](https://cran.r-project.org/doc/manuals/r-release/R-intro.html) guide
* Twice a month it scans the splash page of R-bloggers and tweets a wordcloud:  

![wordcloud](/img/0027-cloud.png)

### How it's done
It turned out to be outrageously easy to do this thanks to the [`twitteR` package by Jeff Gentry](https://cran.r-project.org/web/packages/twitteR/index.html).  A word of warning - the Twitter API has not been stable in recent years, so if you Google for how to speak to it from R or Python make sure you're using only recent links.

The basic steps are set out in [this post where Simon Munzert shows how to program a Twitter bot to send nagging messages to PhD students](http://www.r-datacollection.com/blog/Programming-a-Twitter-bot/).

For some people including me, the main barrier to entry is having access to a server that can run scripts on a regular basis.  In my case, I've now got an old physical machine running Ubuntu server that lets me do this and similar things; if I were taking it more seriously though (and hence worried about backups, continuous service, etc) I'd want to pay Amazon (not very much, they're cheap) to provide it through their EC2 service.

The full set of code that does the various robot tasks is spread over a number of files and folders and can be inspected (or forked...) from [GitHub](https://github.com/ellisp/Rrobot).  If I don't lose interest I might do some more ambitious things - I'd wanted to make it more interactive than it currently is, and now that I've worked out the basics of reading others' tweets, replying and retweeting I have only the small matter of getting something for it to say.  But that would be part of a bigger natural language processing project for me to learn about, some time in the future (and I would probably move to other subject matter).

Here are some examples of code from that repository.  

Here's the file that finds a fortune of less than 140 characters (after its author is pasted to the end) and puts it into the `tweettxt` object for later use.

{% highlight R lineanchors %}
library(fortunes)

f <- data.frame(
   quote = character(),
   len = numeric(),
   stringsAsFactors = FALSE
)

for(i in 1:360){
   f[i, 1] <- paste0("'", fortune(i)$quote, "' ", fortune(i)$author, ".")
   f[i, 2] <- stri_length(f[i, 1])
}

f2 <- f %>% filter(len < 140) 
tweettxt <- sample(f2[, 1], 1)
{% endhighlight %}

Here's the main daily tweeting file.  It calls a script that sets up the robot's credentials (which I haven't published on GitHub), and another that defines some functions including a simple `retweet()` function (I couldn't find one like this in twitteR but it was simple enough to make).  Then it decides whether to call the fortunes script (as above), or one of those that babbles in the style of a classic R manual.  It tweets the result, and saves its tweets to a local log.
{% highlight R lineanchors %}
#--------------set up---------------
library(twitteR)
library(stringi)
library(dplyr)
library(base64enc)

source("credentials_setup.R")
source("utils/functions.R")

#---------------main loop-------------
rnd <- runif(1)

if(rnd > 0.7){
   source("tweeting/fortunes.R")   
} else if(rnd > 0.3){
   source("tweeting/R-intro.R")
} else {
   source("tweeting/R-admin.R")
}

tweet(tweettxt)
line <- paste(as.character(Sys.time()), tweettxt, sep="\t")
write(line, file="tweets.log", append=TRUE)
{% endhighlight %}

Then I need a shell script that will run the relevant R file in batch mode:
{% highlight BASH lineanchors %}
#!/bin/bash
# Main tweeting loop - run once a day
cd ~/Rrobot

R CMD BATCH tweeting/main.R
{% endhighlight %}

And finally, in my `crontab` file, I have entries that run the various scripts when needed, at set times of the day (and days of the month, for the R-bloggers word cloud).
{% highlight BASH lineanchors %}
PATH=/home/ellisp/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games
37 8 *    * * /home/ellisp/Rrobot/cron_scripts/main_tweet
12 6 2,17 * * /home/ellisp/Rrobot/cron_scripts/rblog_cloud_tweet
18 18,11  *   * * /home/ellisp/Rrobot/cron_scripts/retweet
{% endhighlight %}
If you have a Windows server these last two steps would be different, but still straightforward; Simon Munzert's post shows how to do that.

### Acknowledgements
As well as `twitteR` this project depended heavily on the whimsical but pleasant `praise` plus a bunch of the usual suspects for me: `dplyr`, `tm`, `wordcloud`, `stringr`, `stringi`, `rvest`.