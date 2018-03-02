---
Post ID: 152313215409
Date: Tue, 25 Oct 2016 19:53:11
Url with slug: yet-another-elections-simulator
Reblog key: 1BuCfrJd
Reblog Url:
Reblog Name:
Title: Yet another elections simulator
---<p>After reading <a href="https://normaldeviate.wordpress.com/2012/08/04/mixture-models-the-twilight-zone-of-statistics/" target="_blank">Wasserman’s </a>2012 blog post about mixture models, I’m glad that I’ve finally figured out &amp; implemented the Gelman-King 1994 electoral model as well. Based on pretty straghtforward regression modeling, the setup trades the representational simplicity of the Linzer GMM model for a much more complex modeling structure, but that I guess provides better guarantees about its own properties. </p><figure data-orig-width="949" data-orig-height="662" class="tmblr-full"><img src="http://78.media.tumblr.com/4852c6f2f4675f0a1405b04dc45b3c7f/tumblr_inline_ofmni4BWew1qeher0_540.png" data-orig-width="949" data-orig-height="662"/></figure><p>Regardless, it’s a bummer that these guys stuff, the<a href="http://ljwolf.org/post/151986067019/finally-got-this-elections-simulator-working-the" target="_blank"> Linzer 2012 paper </a>on simulating seats/votes curves using gaussian mixtures &amp; the JudgeIt stuff for the GK1994 paper was never merged into the <a href="https://cran.r-project.org/web/packages/pscl/index.html" target="_blank">political science computational library. </a>Eventually, I think this kind of stuff either falls into disrepair, since there’s <a href="http://ljwolf.org/post/147934748469/a-post-scipy-chicago-update#" target="_blank">incredibly low incentive for academics to maintain research software</a>. </p><p>While I think that keeping everything small minimizes the maintenance burden of any individual package, I’m hoping that new stuff like nightli.es and pervasive FOSS CI might get leveraged for academic projects to make sure that when stuff falls into disrepair, it’s clearly labeled as such. I was so bummed when JudgeIt got dropped from CRAN, but my patch suggestion wasn’t ever (as far as I can tell) publically merged :(</p>
Tags: python, foss, academia, programming

Post ID: 151491815214, Date: Fri, 07 Oct 2016 20:09:36