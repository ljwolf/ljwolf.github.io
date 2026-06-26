---
title: "More On The Log Determinant Stuff I Think Ive"
date: 2016-06-19T20:37:12Z
tags: []
draft: false
---

<figure> <img src="/images/archive/tumblr_o8xt2hUN5T1ts05oao1_raw.png"> </figure>

<p>You see, the full likelihood of the model I&rsquo;m working with is something like:</p>

<p>$$|I - \rho W| \times \mathcal{N}(X\beta + \rho W y, \sigma^2) $$</p>

<p>And, since (W) and (I) are constants, I actually need the gradient of the log determinant <em>with respect to ( \rho ), not (A)</em>. Since the derivative of the log determinant <em>with respect to (A)</em>, the full laplacian matrix, is an (N \times N) matrix, I was getting a conformality error in PyMC3, which wanted the derivative with respect to $\rho$ and was expecting a scalar.</p>

<p>So, if I just declare (W) as a constant and proceed with the <code>Op</code> like a <code>Sparse_LapDet</code>, to denote that I&rsquo;m actually interested in computing the derivative of a <em>laplacian</em> log determinant, where the laplacian is defined in terms of scalar variable, this should work.</p>

<p>What a frustrating time troubleshooting this was, though. Theano definintly feels like a different frame of mind sometimes.</p>

*Originally posted on [yetanothergeographer.tumblr.com](https://yetanothergeographer.tumblr.com/146181184144/more-on-the-log-determinant-stuff-i-think-ive).*
