---
title: "Working between python 2 and python 3"
date: 2016-11-18T01:52:16Z
tags: ["python"]
draft: false
---

<p>I am so <b>done with programming for python 2</b>.</p><figure data-orig-width="807" data-orig-height="199" class="tmblr-full"><img src="/images/archive/tumblr_inline_ogtscycgfh1qeher0_raw.png" alt="image" data-orig-width="807" data-orig-height="199"/></figure><p>This is the convergence in a spatial parameter in a spatial model I’m working with. This is a very long run that took a few hours to complete. Tests of this on my (python 3) compute server went fine, tuned correctly, but didn’t have this dramatic convergence.</p><p>Once the sampling finished, I looked at the acceptance rate and, sure enough, the AR on the metropolis step of this Gibbs sampler was like&hellip; .98.</p><p>In the test runs on my compute server, it <i>always</i> tuned to be between .2 and .3 for any reasonable tuning procedure. So, I was perplexed&hellip;</p><p>Doing more tuning on the compute server, I noticed that, even when the accept rate was real high, my tuned metropolis class was reducing the jump size. It seemed like no matter what, the proposal scale was decreasing.</p><p>In fact, it decreased exponentially, which means the tuner never considered the AR as above the target AR. Yet, the AR increases rapidly to 1.</p><p>Cue:</p><pre><code>
acceptance_rate = sampler.n_accepted / sampler._total_iterations

</code></pre><p>Whereas on my test server, this works, on the python 2 compute server, this yields zero always, since <code>n_accepted</code> is an <code>int</code> less than the int <code>_total_iterations.</code> Thus, the tuner takes the acceptance rate to be zero, even when the acceptance rate is likely never zero.</p><p>After my summer of code experience, I’ve come to think this division behavior is <i>the scariest</i> design choice in python 2, and makes the jump from 2 to 3 and back <i>so fraught</i>. I totally forget about <code>__future__</code> because in 90% of my environments, I don’t need it. And, when I have to maintain code in both 2 and 3, bugs like this can’t be autoconverted, and often will only show by causing the program to act strangely, but not fail.</p>

*Originally posted on [yetanothergeographer.tumblr.com](https://yetanothergeographer.tumblr.com/153334732599/working-between-python-2-and-python-3).*
