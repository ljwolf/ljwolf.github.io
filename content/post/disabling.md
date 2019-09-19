---
title: "Disabling Technologies"
date: 2019-09-19T09:48:35+01:00
---

On “Routine” Computing at Scale as a Disabling Technology

Ever since I read Mark Gahegan's discussion and definition of [Geocomputation](http://www.geocomputation.org/what.html) in preparation to run the AAG 2019 Urban Data Science Panel, something he wrote about has really stuck in my mind. Namely, his discussion of *disabling technologies* is both very cogent and intellectually flexible:

## Disabling Technology (Gahegan, 1999)

> The late 1970's and early 1980's saw the rise of databases; large monolithic systems that standardised on interfaces, file structures and query languages. Like all generic solutions there was a cost: flexibility and performance were sacrificed for robustness and ease of application construction. Early GIS show evidence of these same engineering compromises. GIS saw to it that geographers became the slaves of the computer, having to adopt the impoverished representational and analysis capabilities that GIS provided, in exchange for ditching the Fortran, getting some sleep and producing much prettier output.

> GIS was, for some, a backwards step because the data models and analysis methods provided were simply not rich enough in geographical concepts and understanding to meet their needs. (It is entirely possible that computer scientists invented GIS out of spite, being fed up with all those quantitative geographers hogging their CPU cycles and clogging up the disks with kilobytes(!) of data.) Consequently, many of the geographical analysis problems that gave rise to the quantitative revolution in the first place could not be addressed in these systems. Quantitative geographers switched over to GIS or they went to the back of the research funding line.

> In the intervening time, GIS have improved somewhat and geography has become very much richer in digital information. The requirement to build complex applications and simulations has not receded, if anything it has become more urgent, with the need to plan for a changing climate, to feed an increasing population and to provide pinpoint marketing analysis for digital encyclopaedia salespeople.

This idea, that Geographic Information Systems adopted an abstraction at the price of expressiveness or flexibility *and* that this sacrifice permanently affected the trajectory of the field... well, I really think this is true.

## I think it's so true, it doesn't stop there

In the same sense, quantitative geography and scientific computing is faced (again) with a large collection of tradeoffs between "impovrished represntational and analysis capabilities" and a richer and more flexible future. But, I think this tradeoff is being made by the adoption of mainstream/large scale computing system. 

I'm beginning to believe in a notational [Sapir-Whorf](https://en.wikipedia.org/wiki/Linguistic_relativity) hypothesis (or maybe this is just an inversion of [Alan Kay's *Notation as a Tool of Thought*](http://www.eecg.toronto.edu/~jzhu/csc326/readings/iverson.pdf)). If your notation makes it hard to express a concept, you won't express that concept often. Unfortunately, this means that general purpose high-performance computing systems (like the rise of tensor frameworks) will make it fundamentally difficult to express "nonstandard" concepts, relationships, or algorithms. 

Fortunately, geography is becoming more and more "standard" as a computational target. But, the balance between whether or not this is drift in the kinds of questions geographers ask veruss whether this is due to maturation in the field is not really clear. 

Can quantitative geography be as enterprising as it was in the past? Or, now that it has to sit on top of restrictive and non-native abstractions in order to be performant, are we asking a narrower range of "feasible" questions, so that they can be answered with tools we know how to use? 