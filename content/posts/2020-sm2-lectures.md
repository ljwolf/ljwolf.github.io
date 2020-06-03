---
title: "Spatial Modelling 2 Lectures: Topics in Advanced Spatial Modelling"
date: 2020-03-09T12:02:47Z
---

For an intro & discussion of some of the fundamental ideas in spatial statistics, we'll cover a few main topics. 

## Logistics

Courses start at 11AM on the date listed. The "Join Remotely" links will work at 10AM for students to check their AV equipment. The remote sessions will be open from 11AM, through both practical sessions (1-3PM, 4-6PM).

## Plotting & spatial relationships in R
([notebook](https://ljwolf.org/teaching/2020-sm2-00.html))

This practical/component will cover some of the fundamental concepts in quantitative geography, such as:

- theoretical and empirical properties of relationships between geographic objects
- how to compute & use these relationships in R
- what kernel functions/distance decay is
- how to make focused maps & use spatial relationships to make better maps in R

## Testing Tobler's Law
([notebook](https://ljwolf.org/teaching/2020-sm2-01.html), [join remotely](https://eu.bbcollab.com/guest/7b2caddfd9f64dd09986e5272d83bdbf))

> Everything is related to everything else, but near things are more related than distant things. 

In this practical, we’ll be using the previous practical’s information about spatial relationships, functions, and modelling to understand the spatial structure of deprivation in Bristol. We’ll only be doing exploratory spatial data analysis this time, with an eye to doing some more advanced modelling in the weeks ahead.

Our main interest for this practical will be to examine a few questions 
1. Is deprivation clustered in Bristol? 
2. Where are the clusters of deprivation in Bristol? 
3. At what scale does deprivation cluster in Bristol (very local, ward-level, or regionally?)

###  Extra: Local Statistics: finding the uniqueness in all of us
([notebook](https://ljwolf.org/teaching/2020-sm2-02.html)) 

Today, we’re going to look at local spatial autocorrelation. Like a kind of outlier diagnostic, local spatial autocorrelation measures how the local structure of a spatial relationship around each site either conforms to what we expect or is different from what we expect. Together, local spatial statistics are a general branch of statistics tha aim to characterize the relationship between a single observation and the sites surrounding it.

Often, local spatial autocorrelation is contrasted with global spatial autocorrelation, which is the structural relationship between sites (in abstract) and their surroundings (again, in abstract), and this may have strongly different structure for many of the ways we think about the "surroundings" of each observation. Thus, local statistics are an attempt at measuring the geographical beahvior of a given social, physical, or behaviorial process around each observation.

## Spatial thinking in models
([notebook](https://ljwolf.org/teaching/2020-sm2-03.html), [join remotely](https://eu.bbcollab.com/guest/a28e74b3950e4118a496a5509e005cbe))

To this point in Spatial Modelling (I or II), we’ve really primarily dealt with regression models about spatial data. We have not really examined or understood how spatial structures or relationships themselves must be modeled. In the Regression module, we did talk a little bit about spatial fixed effects, which allow for there to be different mean/baselines in each area. In this component, we'll talk a little bit about spatial models, how they work, and why they are used. 
