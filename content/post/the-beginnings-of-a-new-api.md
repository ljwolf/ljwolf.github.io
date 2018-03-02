---
Post ID: 145377908429
Date: Fri, 03 Jun 2016 20:07:42
Url with slug: the-beginnings-of-a-new-api
Reblog key: GwB2wxnl
Reblog Url:
Reblog Name:
Title: The Beginnings of a new API
---<p><em>NOTE: A demo of the relevant code I&rsquo;m referring to for the new labelled array API in <code>pysal.weights</code> in this update is available in <a href="https://github.com/ljwolf/pysal/blob/pdio/from%20classmethods.ipynb" target="_blank">this notebook</a>, and the actual code lives in a <code>weights2</code> module in my <a href="https://github.com/ljwolf/pysal/tree/pdio" target="_blank">gsoc feature branch</a>.</em></p>

<p>I&rsquo;ve decided to target our <code>weights</code> module to prototype the labelled array interface. In general, we&rsquo;ll need extensions built into at least our exploratory spatial data analysis module, <code>esda</code>, our spatial regression module, <code>spreg</code>, and our spatial dynamics module, <code>spatial_dynamics</code>.</p>

<p>So far, I&rsquo;ve focused on <code>weights</code> because it&rsquo;s so central to everything else that the library does. It also poses unique challenges to deisgn around, and I&rsquo;ve already done a bit of work before GSOC in making a labelled array interface for it.</p>

<p>I&rsquo;ve been building constructors that let us build spatial weights objects from the primitives defined in various other computational geometry packages. Fortunately, what&rsquo;s required is nothing more than building efficient type conversions &amp;, where possible, relying on duck typing.</p>

<p>I&rsquo;m somewhat concerned that just relying on duck typing may make this API more fragile than we&rsquo;d like, so I&rsquo;m trying to be eager about conversions to our native geometric objects,  as long as the conversion is computationally cheap.</p>

<p>Altogether, this means that I&rsquo;ve done quite a bit of redesigning of the <code>weights</code> module. But, in general, it still supports the same basic interaction style, but now can build weights from arbitrary iterables of shapes or PostGIS-style dataframes.</p>

<p>Trying to balance this work and my own independent work on <a href="http://ljwolf.org/post/145342859409/visual-display-of-complex-data-can-be-pretty" target="_blank">my dissertation</a> has been challenging so far, but fortunately, the GSOC work has been more forthcoming than I expected. Hopefully, as the project matures, balancing this will be simpler.</p>
Tags: gsoc, python, GIS, geography, pysal

Post ID: 144898901434, Date: Wed, 25 May 2016 04:23:38