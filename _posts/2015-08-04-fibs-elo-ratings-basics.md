---
layout: post
title: Simulating backgammon players' Elo ratings
date: 2015-08-07
tag: 
   - R
   - Backgammon
   - Animations
description: I show how to convert from the Elo ratings of a backgammon player and their opponent and match length to the theorertical probability of winning.  I simulate a simple two player backgammon forum to show how Elo ratings vary at random around players "true" skill level.
image: /img/0002-EloProbs.gif
category: R
---
Backgammon clubs and on-line forums use a modified form of the [Elo rating system](https://en.wikipedia.org/wiki/Elo_rating_system) to keep track of how well individuals have played and draw inferences about their underlying strength.  The higher the rating, the stronger the player.  Players with higher ratings are inferred to be stronger than those with lower ratings and hence are expected to win; and the longer the match, the more likely the greater skill level will overcome the random chance of the dice.  The animated plot below shows the expected probabilities of two players in the [FIBS (First Internet Backgammon Server)](http://www.fibs.com/) internet forum, where players start at 1500 and the very best reach over 2000.

![Animated heatmap of player probabilities of winning](/img/0002-EloProbs.gif)