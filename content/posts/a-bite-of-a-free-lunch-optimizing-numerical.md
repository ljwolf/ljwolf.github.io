---
title: "A bite of a “free lunch” optimizing numerical routines in Python"
date: 2016-02-16T00:09:02Z
tags: ["tanstaafl", "optimization", "python", "gis", "foss", "learning", "studyblr"]
draft: false
---

<p><em>tl;dr: ask yourself <a href="#tldr" target="_blank">these</a> before you deadlift optimize code</em></p>

<p>I use Python pretty much daily, but I’m definitely not a Python wizard. Now that I’m done with  converting the Python Spatial Analytics Library to use Python 3 compatible idioms, I took a break from refactoring more generally.</p>

<p>But, in doing two small tests/stabs at refactoring for speed &amp; memory usage, I’m reminded why <strong><em>optimizing numerical code is so hard</em></strong>. I’ve heard &ldquo;there ain’t no such thing as a free lunch,” so I’m usually wary about getting into performance optimization stuff, but these two recent instances really drive this point home for me again.</p>

<p>First off, know I’m not referring to optimization found by using <a href="http://pypy.org/" target="_blank">pypy</a>, <a href="http://numba.pydata.org/" target="_blank">Numba</a>, <a href="http://nuitka.net/" target="_blank">Nuitka</a>, or <a href="http://cython.org/" target="_blank">Cython</a>. I’m not exactly sure where pypy currently sits on the scale of “optimizing” python, since it seems like the subset of numpy it covers is continuously fluctuating. But I know that when dealing with Numba and Cython, you’re typically looking at a significant amount of type-based refactoring, introducing special knowlege about objects to reduce the <a href="https://www.youtube.com/watch?v=wsczq6j3_bA" target="_blank">indirection and dispatch that make Python so slow</a>.</p>

<p>No, I’m referring to the type of knock-down-drag-out fights with the theory or implementation of an algorithm. I tried (and failed) to do this for one bit of our code and lent a small hand to vet another.
Since I’d talked to a few other grad students recently about how to write fast Python from the ground up, I figured a postmortem might help me remind myself to be more loath to conducting these kinds of deadlifting competitions.</p>

<h3><code>choice</code> and <code>shuffle</code></h3>

<p>The collaboration with <a href="https://cartodb.com/" target="_blank">CartoDB</a> yielded some pretty cool perspective, and I was very happy to see a <a href="https://github.com/pysal/pysal/pull/744" target="_blank">PR</a> from them after a few weeks back in Tempe.</p>

<p>Their PR contained two central optimizations, with one big realization. To compute Local Moran scores, we need to randomly reassign the neighbors of observations, and need to be sure to do this without replacement. That is, we can’t assign duplicate observations since, in theory, we’re using a random labling approach to estimate an empirical distribution for the test statistic.</p>

<p>Currently, we use <code>numpy.random.shuffle</code> and <code>numpy.random.permutation</code> to do this. In abstract, this never struck me as “slow” code, but I did recognize it as the bottleneck for the technique. To that end, I know a ton of attention has been paid by people <strong><em>tons</em></strong> smarter than me to make it fast (<a href="https://github.com/sjsrey" target="_blank">@sjsrey</a>, <a href="https://github.com/jlaura" target="_blank">@jlaura</a> <a href="https://github.com/lixun910" target="_blank">@lixun910</a> come immediately to mind). So, optimization from this was probably not going to come.</p>

<p>Hence why the PR was so surprising. Quickly, Xun noted the first speedboosting change wasn’t sound. It suggested using <code>numpy.random.randint</code> to generate random indexes, rather than <code>permutation</code> on the possible index. Since <code>randint</code> returns a vector of potentially-duplicate random draws from a discrete uniform distribution, using them as IDs for the procedure amounts to sampling <em>with</em> replacement, which is statistically unsound here.</p>

<p>With that down, we the potential optimization submission rested on the second change. I hoped that maybe using <code>np.random.choice</code> could be faster than using <code>np.random.shuffle</code>. My (and I’m sure the submitter’s) intuition was that <code>np.random.choice</code> might select random values the vector of indices without actually randomizing the entire vector. And, indeed, when you use <code>choice</code> <em>without replacement</em>, it’s two orders of magnitude faster than <code>shuffling</code> the entire vector and drawing the first <em>k</em> required observations. But, when <code>choice</code> <em>with</em> replacement is used, again what’s needed for soundness of the technique, this extreme gain evaporates.</p>

<h3>Computing extra inverses</h3>

<p>In a section of our code, we need to construct a symmetric matrix where each term requires us to construct two matrix inverses. The way we do this currently is to construct all of the inverses on demand for a matrix of size <em>t</em>, which means we’re constructing <em>O(t^2)</em> inverses. It runs slowly, but it’s faster than the <code>R</code> implementation the original authors were aiming to beat, so it’s not too concerning.</p>

<p>But, when the author was describing the code to me, it seemed pretty clear to me that we could construct only <em>O(t)</em> inverses by constructing all inverse matrices ahead of time and then looking them up, rather than computing an extra <em>t</em> on demand. Since the matrix wasn’t particularly large, this seemed like a very reasonable target for performance optimization.</p>

<p>So, I set off comparing the currently-implemented on-demand method against two alternatives. Both computed <em>t</em> fewer inversions and stored them. But, even if you leverage some rather speedy <a href="https://docs.scipy.org/doc/scipy-0.14.0/reference/generated/scipy.sparse.linalg.SuperLU.html" target="_blank">LU factorization from SciPy</a> for the inversion, I was only seeing a <strong><em>1.13X speedup</em></strong>. Not quite what I’d hoped.</p>

<p>So, why didn’t the <em>O(t)</em> algorithm outperform the <em>O(t^2)</em> algorithm more? <strong><em>because t is small</em></strong>. More importantly, <em>t</em> is <em>frequently</em> small, so the extra inversions are almost unnoticed. Profiling the code more thoroughly wouldn’t really catch this, since each inversion <em>is</em> still a savings, it’s just that the savings for our typical use cases is very small. While I was happy to have an implementation that used as much memory and provided a modest speedup, it wasn’t really worth it to go through the merge when the speedup didn’t make much of a dent in the computation time.</p>

<h3>No secs for a byte</h3>

<p>In both these cases, there’s this idea that, using some kind of trick, it may be possible to trade memory usage for speed. In the randomization case, it was this idea that it might be more efficient to only sample from a vector, rather than shuffling it entirely. In the estimation case, it was the thought that storing extra data would prevent having to construct more of it on demand.</p>

<p>And, in theory, these seem reasonable. But, computers have a very pecular sense of reasonableness, and I’m beginning to suspect I don’t share it.</p>

<h3 id="tldr">The TLDR</h3>

<p>So, before I consider this type of testing for what I’m calling “deadlift” optimization, I’ll remind myself of three things:</p>

<ol><li><strong><em>Is this something that a user is going to encounter often enough to make it painfully slow?</em></strong> If you can optimize an obscure part of the code, you should probably be doing something more important (like working on your dissertation :)</li>
<li><strong><em>Is the optimization suggested enough for a user to notice?</em></strong> The difference between ~900 seconds and ~800 seconds seems like a lot. But, if you’re getting a coffee while the routine completes anyway, it’s not clear to me whether that tradeoff is important. Since you’re more likely to gain significant optimization advantages from using any of the type/compile-based optimizers anyway, I wonder whether it’s a waste of time to optimize the pure-python implementation, unless there’s an indirect benefit (i.e. learning) or the code is <em>that</em> critical where the circumstance should obviate this question.</li>
<li><strong><em>Is the optimization suggested easier to maintain than a static/type-based compiling version in Cython/Numba?</em></strong> In general, it’s undesirable to have separate, faster implementations that rely on extra dependencies. But, before chasing the performance dragon, you should really consider whether a compile-based solution would get you where your operational constraints are pushing you to be.</li>
<li><strong><em>Is the optimization potentially fast enough to warrant reimplementation?</em></strong> This is possibly the least important, and with stuff like <a href="https://github.com/joealcorn/laboratory" target="_blank">laboratory</a>, I’m hoping it continues to recede. But, again, you don’t want to spend more time than I did to realize a speedup smaller than 1.13X like I found.</li>
</ol>

*Originally posted on [yetanothergeographer.tumblr.com](https://yetanothergeographer.tumblr.com/139404598754/a-bite-of-a-free-lunch-optimizing-numerical).*
