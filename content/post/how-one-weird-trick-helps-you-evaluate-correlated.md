---
id: 151491815214
date: 2016-10-07T20:09:36Z
url: how-one-weird-trick-helps-you-evaluate-correlated
title: How one weird trick helps you evaluate correlated Normal distributions quickly
tags: ["python"," statistics"," computation", "imported"]
---
<figure data-orig-width="679" data-orig-height="420" class="tmblr-full"><img src="http://78.media.tumblr.com/64a57a8fc5e43b8521e35d7b35c951f2/tumblr_inline_oepcbsZ5u91qeher0_540.png" alt="image" data-orig-width="679" data-orig-height="420"/></figure><p>Sorry, couldn’t resist the opportunity to <a href="https://en.wikipedia.org/wiki/One_weird_trick_advertisements" target="_blank">buzzfeed this research blog :)</a></p><p>I’ve been trying to get really efficient at writing samplers for various Bayesian spatial models. And, typically, this involves clever numerical tricks, trying to avoid computing either log determinants or matrix inverses by keeping around matrix factorizations or finding derived products that you can persist between iterations. Some of this has come to fruition in my sparse log determinant work, but I’m always looking for computation speed gains, especially as some of the targets I&rsquo;ve optimized are drying up.<br/></p><p>One I’ve just identified might be in sampling kernels that look like multivariate normal kernels. Naively, when you write the logp of a normal distribution, you’d expect to do the following kinds of computations. For respone <code>Y</code>, linear predictor <code>XBeta</code>, and a dense correlated covariance matrix <code>Sigma</code>, I&rsquo;d naively compute a normal kernel using the following code:</p><pre><code>e = Y - XBeta
Sigma_i = np.linalg.inv(Sigma)
kernel = -.5*np.linalg.multi_dot(e.T, Sigma_i, e.T)
</code></pre><p>But, there&rsquo;s actually a siginficant gain if you use the solve method instead of inverting <code>Sigma</code> by itself.</p><pre><code>e = Y - XBeta
kernel = np.linalg.solve(Sigma, e).T.dot(e)
</code></pre><p>Who knew?
How much faster? Timings are attached for covariance matrices from 5^2 to 20^2 in dimension. Left is the solve method, right is the inverse method. While I’m scaling these up for much larger spatially-correlated error models as I write, it looks to me like a promising speed gain!</p><p><i>edit: scaling quite nicely up to 50^2. I’ll have to see where the sparse math goes with this trick. </i></p>



<small><i> imported from:</i> <a href='https://yetanothergeographer.tumblr.com/151491815214/how-one-weird-trick-helps-you-evaluate-correlated'<tt>yetanothergeographer</tt></a></small>