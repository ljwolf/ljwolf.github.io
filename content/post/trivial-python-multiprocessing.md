---
id: 139457868619
date: 2016-02-16T21:18:10Z
url: trivial-python-multiprocessing
title: Trivial Python Multiprocessing
tags: ["python"," multiprocessing"," gis"," geoprocessing"," geography"," tumblr markdown doesn't support codeblocks with returns?", "imported"]
---
<p>I just wrote up a notebook for a fellow PhD student on how I use python&rsquo;s builtin <code>multiprocessing</code> library to do embarassingly parallel computations much faster. Every time I think about it, I&rsquo;m floored at how simple using the builtin <code>multiprocessing</code> library is for certain operations.</p>

<p>There&rsquo;s a ton of uncertainty out there around the state of parallel computing in Python, and I&rsquo;m not an expert. But, I figure if it&rsquo;s good enough for the unicorn I worked for, it&rsquo;s good enough for a computational social scientist. Since you can prototype so fast, it&rsquo;s very simple to run tons more parallel simulations than you could ever expect to if you did it sequentially.</p>

<p>Since I use <code>multiprocessing</code> for easy stuff like Monte Carlo simulation and GIS processing, many of the operations are <em>embarassingly parallel</em>, meaning that no information is shared between each run of the procedure. Computations like this are your classic Monte Carlo simulations, where each simulation computes some statistic about the realization of a stochastic data generating process.</p>

<p>Many GIS operations and geoprocessing techniques can also be embarassingly parallel, like if you need to construct the minimum bounding circles for a set of polygons. You can do this easily, since each polygon&rsquo;s minimum bounding circle is independent of any other&rsquo;s minimum bounding circle.
So, if you can define your function to take one set of parameters and compute one result, then you can <code>map</code> that function over your simulation matrix.</p>

<p>For some experiment function, <code>experiment</code>, and a matrix of configurations, <code>data</code>,  <code>multiprocessing</code> in python is often as simple as adding this below your declaration of your function:</p>

<p><code>import multiprocessing as mp</code></p>

<p><code>pool = mp.Pool(mp.cpu_count())</code></p>

<p><code>results = pool.map(experiment, data)</code></p>

<p>So, say you&rsquo;re computing the Isoperimetric Quotient for a ton of shapes. You can just:</p>

<p><code>def ipq(polygon):</code></p>

<p><code>return (4 * PI * polygon.area) / (chain.perimeter**2)</code></p>

<p><code>import multiprocessing as mp</code></p>

<p><code>pool = mp.Pool(mp.cpu_count())</code></p>

<p><code>results = pool.map(ipq, polygons)</code></p>

<p>And then results contains the IPQ for each polygon.
This is super simple, and can save tons of time when you can&rsquo;t figure out how to vectorize a particular operation, or just plain need to do a ton of processing.</p>



<small><i> imported from:</i> <a href='https://yetanothergeographer.tumblr.com/139457868619/trivial-python-multiprocessing'<tt>yetanothergeographer</tt></a></small>