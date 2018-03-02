---
id: 98908896789
date: 2014-10-01T15:28:13Z
url: voronoi-gripes
title: Voronoi Gripes
---
<p>Wow, did not expect <a href="http://yetanothergeographer.tumblr.com/post/98840800049/a-fun-little-bug-in-my-apparently-outdated-qgis" target="_blank">this</a> to be as big of an issue as it has been.</p>

<p>Pysal <a href="https://groups.google.com/forum/#!msg/openspace-list/OwDen5Yr1Lw/JZ_2IZUroc4J" target="_blank">removed</a> the code for generating voronoi diagrams for the standard library. Not sure why.</p>

<p>Instead, on the mailing list, the project <a href="https://en.wikipedia.org/wiki/Benevolent_dictator_for_life" target="_blank">BDFL</a> provides code for a diagram of randomized points.</p>

<p>Adapting this, I should be good. Though, I&rsquo;ll have to ask why it was dropped from pysal.core or pysal.cg.</p>
Tags: GIS, pysal