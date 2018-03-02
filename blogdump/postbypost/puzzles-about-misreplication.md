---
id: 144578503954
date: 2016-05-18T20:48:03Z
url: puzzles-about-misreplication
title: Puzzles about (mis)Replication
---
<p>So, a while back, I was using a <a href="http://www.tandfonline.com/doi/abs/10.1080/00045608.2014.941732" target="_blank">new compactness metric</a> to <a href="http://www.tandfonline.com/doi/abs/10.1080/00045608.2015.1039109" target="_blank">extend some gerrymandering studies</a>. In attempting the replication, I found some minor math errors in the original paper that made it diffcult to get valid values for the statistic.</p>

<p>After trying multiple times to verify whether it was my code or if the published statistic had some typographical error, I went to the original author, shared my concerns, and found that it did. Armed with the original code, the replication was easy.</p>

<p>Now, I&rsquo;m working on implementing some Gibbs samplers for hierarchical linear models with spatial effects. And, even though I&rsquo;m <em>incredibly</em> interested in the topic, I&rsquo;ve been finding it incredibly frustrating replicating the paper my grant has targeted.</p>

<p>At the outset, my grant obtained the code the authors used. After really sitting down and groking it, there are numerous discrepancies between the math that the code implements and the math that is published in the paper. In some cases, the code generates the correct result, and in other cases, the paper describes something that the code doesn&rsquo;t implement correctly.</p>

<p>Is the paper non-replicable because they take a logarithm incorrectly? If you sample from the conditional posteriors in this unnamed paper, you <strong>will not</strong> get samples from the correct joint distribution. If you use the techniques in the code, you will get the results they published, but will find that, in other contexts, some of the conditional posteriors are inconsistent for the true parameter.</p>

<p>So, after 8 months, I&rsquo;ve &ldquo;replicated the paper,&rdquo; since I&rsquo;ve transliterated their code. But, does this really <em>replicate the paper?</em></p>

<p>I think what I&rsquo;ve done so far is yet another instance of the <a href="https://plus.google.com/u/0/+TerenceTao27/posts/TGjjJPUdJjk" target="_blank">&ldquo;compilation errors&rdquo;</a> Terence Tao refers to as common among graduate students.</p>

<p>Maybe, if I had</p>

<ul><li><em>read the paper</em> as an artifact of human knowledge, possibly inconsistent or errorful,</li>
<li>understood the mathematical object the authors propose</li>
<li>derived its properties for myself and understood their implications</li>
<li>implemented it how I think it should be implemented</li>
</ul><p>I might&rsquo;ve saved myself a ton of frustration and wasted time.</p>

<p>This leads me to wonder really, what is replicability in social science? What does replicability look like when papers fail to compile? I strongly doubt that a focus on &ldquo;science in a box&rdquo; will solve this, even though it&rsquo;s quite important to make sure that what you implement to do your science is, at least, repeatable.</p>

<p>Maybe in response to these experiences, I think the real crux of replicability is actually <em>validity</em>: if you do something that&rsquo;s broadly in line with the theoretical, empirical, and statistical thesis of the paper, you should get similar results.</p>
Tags: science, replicability, geography, statistics, python, programming