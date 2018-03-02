---
id: 171389427489
date: 2018-02-28T15:46:40Z
url: this-is-an-interesting-little-exploration-i-did
title: Reverse-PCA for making sense of the typical structure in multivariate models 
tags: ["statistics"," simulation"," machine learning"," geography"," brexit"," analysis", "imported", "image"]
---
<figure> <img src=//78.media.tumblr.com/190a56be50695e248630bc895ca7f3e5/tumblr_p4vodsgP6L1ts05oao1_r2_1280.png> </figure>

<p>I don&rsquo;t really have a good idea for what many places in the UK are like, nor for what the structure of some of this data is when considering its joint structure. So, while my model fits quite well and yields some interesting results, I&rsquo;m a bit limited because I don&rsquo;t really know what a place like <code>Barrow-in-Furness</code> is like, without looking into it.</p>

<p>In general, it&rsquo;s more difficult to get a sense of what the model&rsquo;s telling me from the conditional estimates because I don&rsquo;t really have a sense of the joint picture: I don&rsquo;t really intuit how they covary across places, like I might in US counties or states.</p>

<p>So, I found myself wanting a kind of <strong>&ldquo;joint&rdquo; marginal effect</strong>, something I could use to work out how my model predictions vary from &ldquo;places like A&rdquo; to &ldquo;places like B&rdquo; but define those generically, in terms of typical combinations of attributes in my sample.</p>

<p>I started by shifting things linearly along my data&rsquo;s midranges, but this doesn&rsquo;t account for the fact that some attributes may be negatively correlated with other attributes in my design matrix, and so I would expect it to be more typical in my data that <code>Xj</code> increases as <code>Xk</code> decreases, on average. This isn&rsquo;t just a linear shifting using each conditional effect&hellip; it&rsquo;s something else.</p>

<p>So, eigenspaces. I strung some code together to:</p>

<ol><li>Grab the <code>sklearn.decomposition.PCA</code> of my model design matrix.</li>
<li>Extract the most relevant dimension.</li>
<li>Sort my data by this dimension and grab the names of observations.</li>
<li>Plot the predicted Brexit % against these names.</li>
</ol><p>Above is the plot of my data&rsquo;s main dimension, the one that explains the most variance in my design matrix. The lines are the predicted % Brexit, observed % Brexit, and &ldquo;breakeven point,&rdquo; along with the names of places sorted by this dimension on the vertical axis.</p>

<p>Now, I can get a sense of how these types of places (sort of like area profiles) relate to one another in my data. This gives me an idea of what happens when I change from &ldquo;places like Kensington and Chelsea&rdquo; to &ldquo;places like Cornwall,&rdquo; without having to specify the precise covariance structure of my attribute data.</p>

<p>I can slice one dimension off the PCA decomposition, check how varying it changes my model, and see what covariates are related to that dimension.</p>

<p>In a way, gives me the &ldquo;joint&rdquo; marginal effect I want: what happens when you move your mean response along many different features, but in a way that reflects how these features covary in your source data.</p>



<small><i> imported from:</i> <a href='https://yetanothergeographer.tumblr.com/171389427489/this-is-an-interesting-little-exploration-i-did'<tt>yetanothergeographer</tt></a></small>
