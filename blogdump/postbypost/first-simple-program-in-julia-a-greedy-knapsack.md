---
id: 77264557503
date: 2014-02-20T04:32:52Z
url: first-simple-program-in-julia-a-greedy-knapsack
title: 
tags: ["julia"," programming"," optimization"," graduate school"," academia"," geography"," operations research"," college"," julialang", "imported", "image"]
---
<figure> <img src=//78.media.tumblr.com/5ce4365476d1ec01436f7aef07446414/tumblr_n1afusw5EX1ts05oao1_500.png> </figure>
<p>A couple realizations:</p>
<p>1.) Having learned R first, I&rsquo;m continually disappointed when standard arrays don&rsquo;t implement sorting well.</p>
<p>2.) I wanted to implement this using a queue, but apparently those aren&rsquo;t standard elements in Julia&rsquo;s base. And, for some reason, Julia&rsquo;s package/module interface wasn&rsquo;t working. Their explanation is cryptic on how to import functions from modules made out of installed packages, and what the relationship is between modules and packages. Maybe that&rsquo;s because it&rsquo;s at 2am, but I never had a problem understanding python import, Haskell&rsquo;s Cabal, or R taskview/package structure at any time of night.</p>
<p>3.) I haven&rsquo;t bench-marked this against my tail-recursed Haskell implementation that&rsquo;s &ldquo;Faster than C&quot;™ but I&rsquo;ll have to soon. For such a simple algorithm, I have a hard time believing anything can beat tail recursion.</p>
<p>4.) Julia has some serious benefits over python in terms of speed and sparse matrix handling. It&rsquo;s also nicer to write in Julia than in R. If it achieves parity with statistics and visualization, it&rsquo;d be hard to go back. Of course, it&rsquo;s an open question as to whether or not Julia&rsquo;s going to.</p>
<p>5.) There is already a shapefile library, a k-nearest library, and some other geography-related stuff. Add in a baked-in connection to NLOpt, Gurobi, Cplex, and BLAS and it looks like this environment was custom-made for spatial optimization. It&rsquo;s nice to see equal focus on optimization and statistics, where R&rsquo;s optimization libraries are anemic at best and slower than enumeration at worst :)</p>
<p>6.) The community is small, and I see a gap where I can contribute some serious tools. One might be building an interface between it and PySAL. Of course, if Julia&rsquo;s the faster language, it might be smarter to go the other way around, doing analysis in Julia and piping it back into PySAL. Who knows. With IJulia, it might be better to do it that way. I&rsquo;ll think on it.</p>
<p>7.) I&rsquo;m getting better at programming. This took me much less time than the last few times I implemented similar (or more simple algorithms). Maybe I should move to out of kilter implementation as my first-case work&hellip;</p>
<p>8.) I might just have become a Julian!</p>



<small><i> imported from:</i> <a href='https://yetanothergeographer.tumblr.com/77264557503/first-simple-program-in-julia-a-greedy-knapsack'<tt>yetanothergeographer</tt></a></small>