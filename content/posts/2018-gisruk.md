---
title: "GISRUK II: Spatially-Encouraged Spectral Clustering"
date: 2018-04-04T02:20:45+01:00
description: "A paper I'm presenting at GISRUK-2018"
tags: ['clustering','preprints','conference paper', 'paper','gisruk','2018']
---

This paper culminates a bit of work I've started on since seeing a talk by [Phil Chodrow](https://philchodrow.github.io/) on a paper that eventually became his quite interesting [NAS paper](https://philchodrow.github.io/publication/structure-segregation-pub/) paper on segregation and entropy surfaces. 

I was intrigued by the prospect of using spectral clustering for constrained clustering problems. Specifically, I'd known that affinity matrix clustering could be adapted to constrained contexts ever since reading about hierarchical ward clustering, but I hadn't seen a really convincing method that showed me how I could work this out for a general affinity-matrix clustering method.

In this paper, I present one, what I call "Spatially-Encouraged Spectral Clustering." It's a clustering method that allows you to mix the relevance of spatial and attribute information. However, unlike some of the other methods that do this combination, my method recognizes that clusterings on the combination of these two kernels (spatial and attribute) will be sensitive to the structure of the two kernels themselves. The kernels can be regularized, but it still does not imply that the resulting combination will be well-behaved as a function of either kernel bandwidth parameters. I show that in this paper. 

Thus, I recognize this technique is really a generalization/extension of another paper on constrained spectral clustering that I feel doesn't pay significant attention to the fact that both kernels matter. This makes it a "critical revision" of Yuan et al's work. 

The paper is hosted [here](https://osf.io/yzt2p/), as well as [code to conduct similar analyses and an example](papers/2018_gisruk.zip) or a [github repository](https://github.com/ljwolf/spenc) with installation directions & an issue tracker. The presentation I'm giving is [here on google slides](https://docs.google.com/presentation/d/1ckON-x9zyi6XYDr0R-lnEDONdPwv4A4lgQCfrqDUh-I/edit?usp=sharing) ([static](wolf_gisruk2018_spenc.pdf)), commenting is encouraged!
