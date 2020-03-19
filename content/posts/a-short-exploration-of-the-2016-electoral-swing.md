---
id: 153806452919
date: 2016-11-29T00:07:16Z
url: a-short-exploration-of-the-2016-electoral-swing
title: A short exploration of the 2016 electoral swing
tags: ["elections"," politics"," programming"," python"," gis"," geography", "imported"]
---
<p>I’m pretty skeptical of the generalized uniform partisan swing assumption in gerrymandering models. Part of this is due to some skepticism about how swing actually occurs in elections generally. If we don’t have an explicit “shock” model for our counterfactuals, they’re probably not going to replicate true experienced electoral swings well. </p><p><br/></p><p>I’ve <a href="https://gist.github.com/ljwolf/4fe787c821f954075850edc182c6efe6" target="_blank">put together a notebook where I go through and explore some modeling of the 2012-2016 electoral swing at the county level</a>. It’s neat, cause I get to use the package I put together, <a href="https://github.com/ljwolf/cenpy" target="_blank">cenpy</a>, to pull down some ACS data on the fly and merge it with the electoral data and geographies. I really dig doing that. Cenpy’s so fun to use. I really need to get something published on it :/</p><p><br/></p><p>Another thing I’m realizing I’d like to put together is a shim between PySAL spatial regression classes and statsmodels classes. I haven’t figured out yet if it’d be as easy as subclassing the statsmodels ResultsWrapper to wrap up the PySAL model, but I’ll be looking into it (especially since my dissertation work cloning &amp; extending JudgeIt in Python has focused primarily on using statsmodels). </p>



<small><i> imported from:</i> <a href='https://yetanothergeographer.tumblr.com/153806452919/a-short-exploration-of-the-2016-electoral-swing'<tt>yetanothergeographer</tt></a></small>