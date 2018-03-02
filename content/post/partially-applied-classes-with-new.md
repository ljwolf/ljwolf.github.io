---
Post ID: 146381586854
Date: Thu, 23 Jun 2016 20:59:03
Url with slug: partially-applied-classes-with-new
Reblog key: XB2P1XPf
Reblog Url:
Reblog Name:
Title: Partially Applied classes with __new__
---<p>Python’s got some pretty cool ways to enable unorthodox behavior. For my project, I’ve found myself writing a lot of closures around our existing class init functions, and have decided it might be easier &amp; more consistent to express this as what it really is: partial application. </p><p>Partial application is pretty simple to enable for python class constructors, since the separate new method allows you to construct closures around initialization routines. </p><p>Since embedding math &amp; code directly has been a pain on Tumblr recently, I’ll just link to the <a href="http://nbviewer.jupyter.org/gist/ljwolf/80b7f6518e44688ed85c8d9f3c67613e" target="_blank">example notebook</a> and (eventually) move this blog to gh-pages. </p>
Tags: gsoc, python, functional programming, kind of, but definitely not all the way

Post ID: 145529412394, Date: Mon, 06 Jun 2016 19:11:28