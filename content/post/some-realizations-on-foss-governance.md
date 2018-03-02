---
id: 130708836859
date: 2015-10-07T18:10:06Z
url: some-realizations-on-foss-governance
title: Some realizations on FOSS-Governance
---
<p>If you haven’t read/paid attention to the <a href="https://mail.scipy.org/pipermail/numpy-discussion/2015-September/073599.html" target="_blank">Numpy Governance Discussions</a> goingon in the numpy discussion mailing list, I wouldn’t blame you. They’re probably dry and boring.</p>

<p>But, for me, as someone who has been involved in a few Free/Open source communities over the past 8 years, I’m sensing a very interesting divide in the Numpy discussion (and FOSS Python more broadly) that I think is presented more strongly there than I’ve seen it elsewhere. I’ll try to make my understanding of this divide clear.</p>

<p>First, it’s helpful to provide some context. Numpy is <em>the</em> numerical python library, designed to support and enable scientific computation in python. It’s central to scientific computation in python, and has been an incredibly old project. I can’t give its history a full presentation, but it suffices to say, this project contains (or itself <em>is</em>) the “core” of the scientific Python ecosystem.</p>

<p>Travis Oliphant, a longstanding contributor to Numpy, <a href="https://mail.scipy.org/pipermail/numpy-discussion/2015-September/073599.html" target="_blank">recommended some adjustments</a> to a Numpy governance document. Nathaniel Smith, a very active developer in the project, initially resisted or rejected these recommendations in a <a href="https://mail.scipy.org/pipermail/numpy-discussion/2015-September/073630.html" target="_blank">titanic response</a>. This set up an interesting exchange between the two that (I think) demonstrates some pretty evident division in the community. Highlighting this isn’t meant to exacerbate them, but is intended to illustrate them for my own benefit.</p>

<h3>Who is We?</h3>

<p>In the discussion on the mailing list, Nathaniel frequently refers to a network, a community, a &ldquo;we,&rdquo; of which his perspective represents &ldquo;all.&rdquo; Travis is very skeptical of this, and I think he has rights to be.</p>

<p>Focus on consensus in FOSS Projects is inherently cliquish. Some voices are louder than others in FOSS comunities, and, as Travis rightly <a href="https://mail.scipy.org/pipermail/numpy-discussion/2015-September/073642.html" target="_blank">points out in a later response</a></p>

<pre><code>    How did "we" all work it out when not everyone was there?
</code></pre>

<p>Who is the &ldquo;all&rdquo; that Nathaniel refers to? Well, it&rsquo;s the people who speak up to assent and those who aren&rsquo;t bothered enough to dissent.</p>

<p>That&rsquo;s more of a rule-by-clique than anything else. Having sat on the outside of this inner circle quite a few times now, I&rsquo;m very uncomfortable with the cliquishness of this group.</p>

<p>Nathaniel refers to the relatively anemic Debian CTTE as somehow a counterexample to Travis&rsquo;s desire for more long-rooted involvement in any Numpy advisory council. The point that is totally missed is that the CTTE is not very frequently used because Debian uses a referenda voting system to resolve WAY more debates than the CTTE could ever adjudicate.</p>

<p>Without some formal instantiation of a consensus <strong>VOTING</strong> procedure, any reference to the opinion of &ldquo;we&rdquo; is incredibly suspect.</p>

<p>Of course, voting comes with its own issues. I think this is why successful open source projects typically have a BDFL who is a founder or longstanding contributor-type who contributes institutional knowledge. However, the voting process makes clear where the &ldquo;we&rdquo; in the community is.</p>

<h3>The Johnny-Come-Lately</h3>

<p>But, there&rsquo;s more than resistance to this &ldquo;we&rdquo; that Travis is speaking to. He mentions reluctance to empower people who have many recent commits over people with experience in the project. Nathaniel is again dismissive of this in the exchange.</p>

<p>Why would Travis want that? Why would any long-standing contributor/originator feel the desire to add this experience to the governance of an open source project?</p>

<p>At first brush, this seems like simple ageism. And, I&rsquo;ll admit, that&rsquo;s what I felt it was upon reading the exchange initially.</p>

<p>But, after thinking on it, I believe there&rsquo;s more to it than that.</p>

<p>Python has been around for a long time. Numpy is like&hellip; 20 years old. Many (if not all?) of the non-Debian project governance examples Nathaniel cites in this discussion are from Jupyter/Ipython, which is an incredibly young, superstar project. And, resistance to the <em>de facto</em> standards imposed by these new superpopular projects is understandable. But, as I&rsquo;ve been meaning to state flatly in my own dev community:</p>

<p>I started learning python right around 2.7&rsquo;s initial move into End-of-life. I hardly ever used the python interpreter before IPython. I have never done social science without decent python GDAL bindings &amp; pandas tabular data.</p>

<p>And, in all likelihood, I&rsquo;ll have to relearn quite a bit.</p>

<p>You see, longstanding contributors who feel ownership over their python packages are usually concerned about the diffusion of responsibility. This fear of diffusion comes into play when considering to add a dependency on the newest, flashiest framework or when transitioning from BDFL to community-governance models. Really, any time a project becomes less self-contained, these kinds of anxieties resurface.</p>

<p>And, I think this is understandable. There&rsquo;s a python ecosystem, but it is anchored on different packages that operate as self-contained islands. And, it&rsquo;s difficult to understand exactly <em>how</em> transitory some things are in this ecosystem. The islands never move, but flotsam and jetsam abound.</p>

<p>Travis and other longstanding contributors built Numpy for specific reasons and have unique insight into both the package and the vision of what it <em>should be</em> that is simply unavailable to any johnny-come-lately like myself. A community where every voice is equal is one that relies on &ldquo;meritocracy&rdquo; to do quite a bit of heavy lifting.  It&rsquo;s also one that should bear in mind the classic Asimov caution about democracy being the state where:</p>

<pre><code>my ignorance is just as good as your knowledge.
</code></pre>

<p>Founders/longstanding contributors have unique, irreplacable institutional knowledge about the software they have helped create. In my own life, coordinating new ideas with code debt and legacies of involvement has both been frustrating, but ultimately rewarding for the project.</p>

<p>And, I hope projects like Numpy find ways to involve their tall trees that don&rsquo;t blot out the sun for saplings like me.</p>
Tags: numpy, python, scientific python, gis, geography