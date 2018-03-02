---
id: 78062682127
date: 2014-02-27T20:42:45Z
url: metaprogramming-in-julia
title: <p>After doing the <a href="http://www.youtube.com/playlist?list=PLB63C06FAF154F047" target="_blank">MIT SICP lectures</a> over the summer, I&rsquo;m always amazed at how language development frequently attempts to implement functionality implementable in LISP. It seems like LISP really was a consummately expressive language, and it&rsquo;s frustrating sometimes to not have simple things I got used to when learning the functionals like Scheme and Haskell.</p>
---
<p>That aside, this macro capability is amazing, and it&rsquo;s illustrated here to quite the effect. The stupidly simple knapsack I implemented is done here in three lines that are much more semantically meaningful than my larger program. They macros are extended at run time into full expressions, and their data structure is much better for the solution of general knapsack problems. </p>
<p>My implementation would not quite win out in a diff battle.</p>
<p>This paper by Miles Lubin at MIT really demonstrates the ability of these macros to reduce general overhead and waste for IP/MIP. I&rsquo;d really recommend a read, if you&rsquo;re interested in math programming. The more I learn about this language, the more I&rsquo;m convinced it&rsquo;s posed, from an infrastructural perspective, to replace <em>at least </em>R. But, then again, I&rsquo;m not getting my degree in telling futures to the present.</p>
Metaprogramming in Julia
http://arxiv.org/pdf/1312.1431v1.pdf

Tags: math, mathematical programming, julia, julialang, integer programming, linear programming, optimization, MIT