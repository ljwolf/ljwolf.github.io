---
id: 145342859409
date: 2016-06-03T01:40:17Z
url: visual-display-of-complex-data-can-be-pretty
title: sampling distributions at each grid site
tags: ["geography"," statistics"," grad school"," python"," gis", "imported", "image"]
---
<figure> <img src=//78.media.tumblr.com/cf10dc435036ea04c78a86e736fd750e/tumblr_o86l35Jg0B1ts05oao1_1280.png> </figure>

<p>This is a visualization of one hierarchical parameter from a a <a href="http://link.springer.com/article/10.1007/s10109-006-0040-y" target="_blank">spatially-varying coefficient process model</a>, estimated on a simple 10x10 grid. The model itself is a pretty neat way to specify a spatially-varying process with an efficient formal structure. I won&rsquo;t go into the specification here, but my growing interest in MLM/HLM means I&rsquo;ll probably be posting about SVCP-style models more frequently.</p>

<p>The point of this, though, is that characterizing both the spatial distribution of hierarchical parameter means <strong>and</strong> the distributions of those parameters for each individual unit is hard. Essentially, you&rsquo;re making a map of distributions. And, for a regular lattice, this is somewhat more simple than you&rsquo;d expect: set the background of each unit to an indicator of centrality for the distribution in that unit, and then superimpose the distribution. Since each window is the same size, each distribution is on the same perceptive plane, so comparisons of both the pattern &amp; the distribution are relatively simple.</p>

<p>But, if the windows were differently sized, we&rsquo;d want a way to handle the classic cartographic problem: bigger elements on maps are seen as more important than smaller elements. Once the cells are differently-sized, the distributions would be harder to compare directly. We&rsquo;d have to figure out how to make the distributions comparable, even when they sit in frames that are not identically sized.</p>

<p>In reality, the reason why I&rsquo;m fretting about this is because I&rsquo;ve been immersed in the nitty-gritty of the statsistical theory behind spatial multilevel/hierarchical models &amp; am about to head deep into applying them for my RA &amp; my doctoral dissertation.</p>

<p>For the SVCP, I’m prepping this as a reference implementation to move to two enhancements.</p>

<ol><li>The original specification from <a href="http://www.people.vcu.edu/~dbandyop/pubh8472/Gelfand_SVC.pdf" target="_blank">Gelfand 2003</a> and that in the <a href="https://www.crcpress.com/Hierarchical-Modeling-and-Analysis-for-Spatial-Data-Second-Edition/Banerjee-Carlin-Gelfand/p/book/9781439819173" target="_blank">Bannerjee Spatial HLM book</a> use an Inverse Wishart prior on their coefficient process covariance matrix. This is <a href="http://andrewgelman.com/2012/08/22/the-scaled-inverse-wishart-prior-distribution-for-a-covariance-matrix-in-a-hierarchical-model/" target="_blank">less than ideal</a>, since IW covariance priors result in some <a href="https://dahtah.wordpress.com/2012/03/07/why-an-inverse-wishart-prior-may-not-be-such-a-good-idea/" target="_blank">pretty surprising statistical artifacts. </a>. Switching to an LKJ prior or a separation strategy prior for the project I need to use the SVCP on is a high priority.</li>
<li>I want to pivot the sampler to use PyMC3. I wrote the Gibbs sampler for the model myself; since all except for one parameter are analytically tractable draws from known builtin distributions, this is super simple to write. But, it&rsquo;d be nice to not have to use MH on a range-restricted, weakly-identified parameter. In fact, this particular model would be very amenable to an approximation straetgy, since the prior on the spatial decay parameter, $\phi$, is <em>very nearly</em> a normal-gamma distribution, and could probably be well-approximated by one. Plus, PyMC3 has an LKJ prior already implemented, and I know I&rsquo;ll be wanting to use it in other spatial HLMs.</li>
</ol>



<small><i> imported from:</i> <a href='https://yetanothergeographer.tumblr.com/145342859409/visual-display-of-complex-data-can-be-pretty'<tt>yetanothergeographer</tt></a></small>
