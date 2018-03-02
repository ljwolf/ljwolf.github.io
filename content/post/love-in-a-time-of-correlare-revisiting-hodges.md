---
id: 171287250329
date: 2018-02-25T18:05:34Z
url: love-in-a-time-of-correlare-revisiting-hodges
title: <p>This is a quick exploration/exposition of the fact that estimated substantive effects (fixed effects in H&amp;R&rsquo;s terms) can and sometimes will change in models with spatially dependent error terms when (and only when) the dependence in the error term is collinear with one or more covariates in the design matrix.</p>
---

<p>Intuitively, this makes sense. In a thought experiment, estimate a regression. Then, introduce a covariate that&rsquo;s collinear with the other covariates. If there&rsquo;s no structure used in the model to keep track of which are the &ldquo;original&rdquo; effects and which are the newcomers (e.g. a <a href="https://en.wikipedia.org/wiki/Partial_regression_plot" target="_blank">partial regression</a> structure), then the effect estimates <em>must</em> change, since each is conditional on the other/they&rsquo;re made jointly over the covariates.</p>
love in a time of correlare: revisiting Hodges & Reich (2010) in SAR models
http://ljwolf.org/hodges_reich_sar

Tags: statistics, geography, data science, spatial models