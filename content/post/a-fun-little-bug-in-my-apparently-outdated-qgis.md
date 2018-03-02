---
id: 98840800049
date: 2014-09-30T18:57:00Z
url: a-fun-little-bug-in-my-apparently-outdated-qgis
title: vornoi bugs
tags: ["computing"," academia"," qgis"," gis"," open source"," oss"," foss"," geography"," grad school", "imported", "image"]
---
<figure> <img src=//78.media.tumblr.com/7202d2213a242e8dd3eabff5e36edee5/tumblr_ncql4nMXCj1ts05oao2_1280.png> </figure>

<p>I am in the process of matching North Carolina congressional districts (CD) to their corresponding voronoi polygons drawn from the CD centroids.</p>

<p>At the top, the voronoi polygons for the North Carolina CD centroids are calculated at an 80% buffer distance. This means that the algorithm is trying to extend the voronoi polygons out to 80% of the minimum bounding rectangle.</p>

<p>But, if you know things about <a href="https://en.wikipedia.org/wiki/Voronoi_diagram" target="_blank">voronoi polygons</a>, you know that they</p>

<ol><li>always generate convex shapes</li>
<li>extend an indifference frontier to infinity (i.e. the lines equidistant from the points being diagrammed continue forever if they do not intersect another indifference frontier)</li>
</ol><p>But, for that southern part of the top map, we see that <em>something</em>&rsquo;s screwed up.</p>

<p>Below, setting the buffer distance to 50% doesn&rsquo;t have this happen.</p>

<p>There was apparently a <a href="https://hub.qgis.org/issues/8002" target="_blank">related bug</a> up on the QGIS project issue tracker, but this is unrelated to overlapping polygons, as it actually changes the <em>angle</em> of the indifference frontier between a few pairs of points. That bug may have also been present in <a href="http://opensourcegisblog.blogspot.com/2013/03/grass-vs-arcgis-thiessen-polygons.html" target="_blank">ArcGIS current as of its posting</a>, so maybe I can test against Arc 10.2.2 as well.</p>

<p>I bet <a href="http://pysal.github.io" target="_blank">PySAL&rsquo;s</a> got this on lock.</p>

<p>Now, I have to update QGIS, hope nothing breaks, and then try again. Maybe another reason why <a href="http://yetanothergeographer.tumblr.com/post/89112989819/my-time-using-linux-has-apparently-made-me-quite%5D" target="_blank">I wish my work computer was a rolling-release linux distro</a>&hellip;</p>



<small><i> imported from:</i> <a href='https://yetanothergeographer.tumblr.com/98840800049/a-fun-little-bug-in-my-apparently-outdated-qgis'<tt>yetanothergeographer</tt></a></small>
