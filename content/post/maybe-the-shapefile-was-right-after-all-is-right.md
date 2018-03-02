---
id: 120881995674
date: 2015-06-06T17:26:38Z
url: maybe-the-shapefile-was-right-after-all-is-right
title: “Maybe the Shapefile was right after all” is right after all
---
<p>The more that I work with broad datasets, collected from many different means and used in many different contexts, the more I think that you really <a href="http://sgillies.net/blog/2014/01/02/maybe-the-shapefile-was-right-after-all.html" target="_blank">only need the &ldquo;multi&rdquo; instantiation of polygons, lines, and points</a>. Usually, due to database typecasting, you want to coerce any heterogeneous datatypes into the same datatype. Since it&rsquo;s much simpler to cast Polygons upwards into MultiPolygons, I&rsquo;m rarely seeing any Polygons or Lines, even in datasets where the majority of features are single-component.</p>

<p>Overall, Multi- datatypes are more general than their single-component counterparts. So, exploiting that, I&rsquo;m seeing that most of the data I see across our workflows gets cast upwards into Multi-geometries, even when there is only one component.</p>

<p>It&rsquo;d be so much more succinct to just rename Multi-geometries as their singly-instanced geometries, and then use &ldquo;length&rdquo; or &ldquo;component&rdquo; counts to determine how many elements the geometry collection contained. I know that I&rsquo;ll be doing this consistently in my research database, at least.</p>
Tags: geometry, geography, GIS, python, WKT, simplefeatures, geojson, ogc