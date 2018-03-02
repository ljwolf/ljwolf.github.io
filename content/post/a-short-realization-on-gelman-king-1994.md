---
id: 97287883904
date: 2014-09-12T04:01:00Z
url: a-short-realization-on-gelman-king-1994
title: A Short Realization on Gelman-King (1994)
tags: ["politics"," bayesian statistics"," bayes"," elections"," electoral analysis"," statistics", "imported"]
---
<h1>Elections, Bayes, and one realization about the Gelman-King Model</h1>
<p>People make a lot of hay out of the rise of <a href="http://www.forbes.com/sites/singularity/2012/11/07/nate-silver-and-the-rise-of-political-data-science/" target="_blank">Nate Silver</a> and <a href="http://www.stat.columbia.edu/~gelman/research/published/election15Feb.pdf" target="_blank">Bayesian poll averaging</a> when it comes to the rise of data-driven electoral prediction and analysis. When it comes to data-driven politics, these methods are pretty neat. But, they&rsquo;re based on very old understandings of statistics which, in the right light, seem quite intuitive. </p>
<h2>Gelman-King (1994)&rsquo;s model</h2>
<p>Say, for instance, we&rsquo;re examining some vector of electoral outcomes, \(y\) given some predictor matrix \(\mathbf{X}\). Depending on the strength and significance of the predictors recorded in <span>\(\mathbf{X}\), we may have strongly-related and weakly-related factors. Let&rsquo;s call a vector of these strengths \(\beta\). </span>Because we&rsquo;re charitable, we allow our prediction to be a little bit wrong, so we include some unknown (or stochastic) error term \(\epsilon\).</p>
<p>But, elections are somewhat deterministic: an electoral system, when given full information about voters, will transform them into an electoral result. However, we don&rsquo;t have perfect information, so let&rsquo;s model that bit of information we don&rsquo;t know as \(\gamma\). </p>
<p>Thus, our model for how the electoral system outcome will look after all the votes are cast looks something like:</p>
<p>$$ y = \mathbf{X}\beta + \gamma + \epsilon$$</p>
<p>This is the <a href="http://r.iq.harvard.edu/docs/judgeit/1.4.0/The_Gelman_King_Model.html" target="_blank">Gelman &amp; King (1994)</a> model of electoral outcomes. What was novel in this model formulation for 1994 was how it walked the thin (and often blurry) line of observed data and the theoretical constructs that data supposedly measures. In essence, this distinction between &ldquo;random error&rdquo; and &ldquo;structural error&rdquo; allows Gelman and King to hop between different &ldquo;states of information&rdquo; in their understanding of the electoral system, which is quite novel. </p>
<h2>Bayes Estimators?</h2>
<p>But, getting to the connection with Bayesian statistics, let&rsquo;s consider what happens when our structural error, \(\gamma\) significantly affects our model.</p>
<p>Bayesian statistics has a few central realizations, and there are many many good discussions of <a href="https://en.wikipedia.org/wiki/Bayes'_theorem" target="_blank">Bayes&rsquo; Theorem</a> and the differences between <a href="https://en.wikipedia.org/wiki/Probability_interpretations" target="_blank">frequentist and subjectivist statistics</a>[^1]. But, let&rsquo;s instead focus on an important realization that ties Bayesian prediction back to the &ldquo;normal&rdquo; world of statistics.</p>
<p>Say we have some well-derived statistical estimator for \(y\), \(\hat{y}\)[^2]. We could then use <span>\(\hat{y}\), if we only observe \(\mathbf{X}\), to predict what \(y\) <em>will </em>be or <em>would</em> be, given different circumstances. This kind of analysis is typically called &ldquo;predictive&rdquo; or &ldquo;counterfactual,&rdquo; respectively, and plays a big part in strategic planning of campaigns. </span></p>
<p>But, let&rsquo;s say you think you know something about how the system behaves that could take into account <em>different </em>information than what we&rsquo;ve observed? That is, before you really think about analyzing the data and generating potential estimates of the election outcomes, you have some <em>belief</em> (\(b\))of how the electoral system will go. Could that be used?</p>
<p>If you had to combine both pieces of information into one estimator, you could <em>weight </em>the relative importance of both sets of information and include them in your predictions. Given some weight \(w\) between zero and one, you could have a convex combination of the two pieces of information that shows your relative confidence in the estimator and your beliefs:</p>
<p><br/> $$ \hat{\theta} = w * b + (1 - w)\hat{y]$$</p>
<p>Typically, when that prior belief $b$ is well-behaved and \(\hat{y}\) has certain properties, we call  <span>\( \hat{\theta} \) a <em>Bayes Estimator</em>. </span></p>

<h2>Getting back to Gelman</h2>
<p>So, where do Gelman &amp; King&rsquo;s electoral model come into this?</p>
<p>Given that we&rsquo;ve estimated our model outlined above, we get that, to find some <em>new, hypothetical </em>electoral outcome, we use the equation:</p>
<p>$$ P(v^{hyp} | v) = N\left(v^{hyp} | \lambda v + (X^{hyp} - \lambda X)\hat{\beta} + \delta^{hyp}, (1 - \lambda^2)\sigma^2I + (X^{hyp} - \lambda X)\Sigma_\beta (X^{hyp} - \lambda X) &rsquo; \right)$$</p>
<p>That is, we have some new voting results \(v^{hyp}\), given some believed voting outcome \(v\), is distributed over a normal distribution with variance.</p>
<p><span>But, what if we have a real good idea of \(v\) occurs? Like, so good that it already happened? Could we make this something like a Bayes </span>Estimator?</p>
<p>The short answer is <em><strong>yes.</strong></em></p>
<p><span>See, Gelman &amp; King&rsquo;s model is really quite useful for prediction of </span><em>hypothetical </em><span>outcomes. This allows our prior belief \(b\) to be based on </span><em>actual </em><span>information that we observe, like \(v\). Thus, given that we <em>actually can observe voting outcomes </em>and let&rsquo;s call them \(v_o\), our expectation of electoral outcomes becomes:</span></p>
<p>$$ E(v_i^{hyp} | v_o ) = \lambda v_o + (1 - \lambda) X \beta$$</p>
<p>where \(lambda\) is just a meaningful version of the \(w\) presented above, based on the variance of the process observed. </p>
<h2>So what?</h2>
<p>Before getting into other applied <a href="https://jakevdp.github.io/blog/2014/03/11/frequentism-and-bayesianism-a-practical-intro/" target="_blank">perambulations</a> that occasionally implicate the <a href="http://www.stat.columbia.edu/~gelman/research/published/fellerip-1.pdf" target="_blank">&ldquo;largely-overblown&rdquo;</a> concerns about the differences between Frequentism and Bayesianism discussed earlier, it&rsquo;s clear that there are intuitive connections to Bayesian perspectives that simply have not been explored in most treatments of probability. Here, for electoral study, we have so much <em>historical knowledge</em>, all quantified in high-quality data, that can provide reasonable expectations about our prior expectations.</p>
<p>Statistical analyses that fail to take into account these priors can&rsquo;t really leverage the large value which Bayesian analysis methods can provide, in terms of making prior expectations useful. </p>
<p>If you&rsquo;re studying a phenomena in social science with a long literature, you might want to check out Bayesian methods in your field or subdomain. </p>
<p>[^1]: as an aside, I think these articles (and conflicts over frequentist v. subjectivist statistical interpretations) is of the most edited &amp; flamed topics on wikipedia. </p>
<p>[2^]: <span>Usually (and I mean almost always), Bayesians use Maximum Likelihood estimators, but the choice of estimator is not critically important here.</span></p>



<small><i> imported from:</i> <a href='https://yetanothergeographer.tumblr.com/97287883904/a-short-realization-on-gelman-king-1994'<tt>yetanothergeographer</tt></a></small>