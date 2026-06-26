---
title: "So Say Youre Working With Two States"
date: 2014-08-22T19:55:00Z
tags: ["r", "scientific computing", "data", "data wrangling", "factor issues", "computing"]
draft: false
---

<figure> <img src="/images/archive/tumblr_naqfs9Q1p51ts05oao1_raw.png"> </figure>
<p>Then, you have to read it into R because JudgeIt is only an R package so far. </p>
<p>All the dataframes were made in <em>the same way</em>, yet R decided that indicators of California&rsquo;s 112th districts were factors, not numbers, causing JudgeIt sanity checks to fail. </p>
<p>these errors read something like:a</p>
<blockquote>
<p><code>ERROR: Some $VOTE values not in [0,1] interval.</code></p>
</blockquote>
<p>So, I did what the reasonable person who is used to using <code>float()</code> would do and just <code>as.numeric()</code> over the offending columns.</p>
<p>And got garbage. </p>
<p>Now, I worked in R to do my undergraduate honors thesis, about two years ago. I don&rsquo;t remember this being an issue, but I really should go back and revisit my code. But, apparently,<code>as.numeric()</code> translates factors into some &ldquo;internal R&rdquo; representation rather than what the typical individual (I&rsquo;d assume) <em>would </em>want from a function that converts something to numeric type. instead, to convert factors to numbers, you have to </p>
<blockquote>
<p><code>as.numeric(as.character($1))</code></p>
</blockquote>
<p>or, convert the factor into its character-based representation, and then convert those characters into numbers. To me, that&rsquo;d be like converting from some object to numbers by using <code>float(str($1))</code>. Not impossible, but definitely unintuitive. </p>
<p>More annoying than this double-conversion, though, is the fact that <em>only one</em> of my data frames, generated from the same base document, parsed in the <em>same <code>for</code> loop</em><em> in the same way</em>, was designated a factor. And, all that work to move the code into a standard dataframe type into some unique, non-standard object for the analysis package I&rsquo;m using. </p>
<p>Just silly. Data-wrangling in R is easy, but unintuitive, coming from someone whose first language was R 2.8.</p>

*Originally posted on [yetanothergeographer.tumblr.com](https://yetanothergeographer.tumblr.com/95500223474/so-say-youre-working-with-two-states).*
