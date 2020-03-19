---
id: 146381586854
date: 2016-06-23T20:59:03Z
url: partially-applied-classes-with-new
title: Partially Applied classes with __new__
tags: ["gsoc"," python"," functional programming"," kind of"," but definitely not all the way", "imported"]
---
<p>Python’s got some pretty cool ways to enable unorthodox behavior. For my project, I’ve found myself writing a lot of closures around our existing class init functions, and have decided it might be easier &amp; more consistent to express this as what it really is: partial application. </p><p>Partial application is pretty simple to enable for python class constructors, since the separate new method allows you to construct closures around initialization routines. </p><p>Since embedding math &amp; code directly has been a pain on Tumblr recently, I’ll just link to the <a href="http://nbviewer.jupyter.org/gist/ljwolf/80b7f6518e44688ed85c8d9f3c67613e" target="_blank">example notebook</a> and (eventually) move this blog to gh-pages. </p>



<small><i> imported from:</i> <a href='https://yetanothergeographer.tumblr.com/146381586854/partially-applied-classes-with-new'<tt>yetanothergeographer</tt></a></small>