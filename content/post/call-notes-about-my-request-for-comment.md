---
Post ID: 144898901434
Date: Wed, 25 May 2016 04:23:38
Url with slug: call-notes-about-my-request-for-comment
Reblog key: dn44DEWe
Reblog Url:
Reblog Name:
Title: Call notes about my Request for Comment
---<p>The followng were comments I recieved on my <a href="https://gist.github.com/ljwolf/9730be2bfa14327a262d21e78d55d5f5#file-a-model-proposal-ipynb" target="_blank">Request for Comment</a> submitted a bit ago.</p>

<ul><li>Questions about <a href="https://gist.github.com/ljwolf/9730be2bfa14327a262d21e78d55d5f5#file-a-model-proposal-ipynb" target="_blank">Request for
Comment</a>:

<ul><li>What should I prioritize? NOGR or Labeled Array Interface?</li>
<li>Labeled Array. This is critical to get correct, and will make NOGR need
and scope clearer.</li>
<li>How deep into PySAL should the Labeled Array interface go?</li>
<li>Design it like the library were getting built now.</li>
<li><strong>Do not</strong> fail on import. Instead, use soft dependencies/optional import
patterns</li>
<li>if necessary, write Python3-only components safely, so that new features
can be leveraged.</li>
<li>What should get deprecated?</li>
<li>Anything that looks less smooth in the unlabeled array interface should
get flagged with a depwarning.</li>
<li>If the tabular IO is smooth and works parallel to the older interface,
then throw a deprecation warning on the FileIO components.</li>
</ul></li>
<li>Deliverables in the medium term (targeting midterm eval for GSOC):

<ul><li>Two Contrib Modules:</li>
<li>GeoTable: interfaces between PySAL labeled arrays &amp; Geopandas arrays</li>
<li>Pdio: extend and improve tabular interface already in PySAL</li>
<li>Some work in core:</li>
<li>Polymorphic weights constructors

<ul><li>i.e. work on any arbitrary iterable of shapes</li>
<li>return correct weights object from the iterable</li>
<li>possibly indexed by a second collection of indices</li>
</ul></li>
<li>Revamp &amp; scaffold new IO system revolving around multiple alternative
packages &amp; their drivers:

<ul><li>expose all pandas <code>read_</code> functions</li>
<li>ensure pysal objects get serialized correctly into wkb/wkt by <code>to_</code> on
dataframes</li>
<li>wrap Fiona &amp; geopandas constructors to provide identical output to
pdio.read_files</li>
</ul></li>
</ul></li>
<li>Plan to connect with new geopandas contributors at SciPy</li>
<li>Investigate possibility of serializing with Libfeather (remote, if time
remaining)</li>
</ul>
Tags: gsoc, gis, python

Post ID: 144823917304, Date: Mon, 23 May 2016 17:20:58