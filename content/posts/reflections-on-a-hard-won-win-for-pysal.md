---
id: 125215868119
date: 2015-07-27T21:10:56Z
url: reflections-on-a-hard-won-win-for-pysal
title: Reflections on a hard-won win for PySAL
tags: ["python"," gis"," python 3"," py3k"," geography"," pysal"," statistics"," spatial analysis", "imported"]
---
<p>As a way to learn Python (and I mean <strong>really come to know it</strong>), I took on the project of converting <a href="https://github.com/ljwolf/pysal/tree/py3conv" target="_blank">PySAL</a> to be compatible with Python 3.</p>

<p>I started work on this two years ago. A tentative candidate for release is available <a href="https://github.com/ljwolf/pysal/tree/py3conv" target="_blank">in my fork</a></p>

<p>Now, I’m glad I did this. I still worry about the day when Apple decides to ship the next OSX (Half Moon Bay? Big Sur? Salton Sea?) as Python 3 only, like many popular Linux distributions are aiming to.</p>

<p>But, this work was not always so easily motivated. There were countless discussions online that the conversion process wasn’t worth it, that it didn’t really matter, that scientific computing wasn’t ever going to take it up. Some even stated that widespread adoption in the scientific community would <a href="https://jakevdp.github.io/blog/2013/01/03/will-scientists-ever-move-to-python-3/" target="_blank">take quite a bit</a>. None of this was reassuring, no matter how quickly the python 3 <a href="https://python3wos.appspot.com/" target="_blank">Wall of <strike>Shame</strike> Superpowers</a> was changing to green.</p>

<p>I took a lot of solace in Nick Coghlan’s <a href="http://python-notes.curiousefficiency.org/en/latest/python3/questions_and_answers.html" target="_blank">Q &amp; A</a>, and I stumbled through <a href="https://pythonhosted.org/six/" target="_blank">six</a> documentation. I used <a href="https://www.archlinux.org/" target="_blank">Arch Linux</a>, and their move to Python 3 from Python 2 as the default <code>python</code> executable (prompting this <a href="https://www.python.org/dev/peps/pep-0394/" target="_blank">PEP</a>) was super encouraging.</p>

<p>But, as I finally started wrapping my head around how Metaclasses worked and how our library was organized, I realized: This wasn’t as hard as it appeard initially. In fact, I was writing Python 3 code almost by default.</p>

<p>By far, the hardest part of the conversion effort was the fact that every division operation was suspect. In typical, non-scientific applications, which is probably not too hard to fix. But, lacking the most basic of indications about whether or not a <code>float</code> or an <code>int</code> were being passed to <code>/</code> meant that figuring out when and where things could fail was tedious. Indeed, small bugs were uncovered, where floor division defaults in Python 2 could yield wrong answers in statistical calculations given certain input data. No warnings, no errors, but incorrect results: a nightmare for any library maintainer.</p>

<p>This was my first <strong>real</strong> tussle with static v. dynamic typing, and I have to say, I think it really pushed me a little bit closer to actually <em>desiring</em> static typing. Part of the big reason I want PySAL to start being written in python 3 is the benefits of <a href="https://www.python.org/dev/peps/pep-0484/" target="_blank">type annotations</a> (<a href="https://www.youtube.com/watch?v=2wDvzy6Hgxg" target="_blank">Guido’s perspective</a>) to give a <a href="http://wphomes.soic.indiana.edu/jsiek/what-is-gradual-typing/" target="_blank">gradually-typed</a> flavor to Python.</p>

<p>At this point, I’m firmly in the gradual type camp, if not totally won over by Julia’s <a href="http://stackoverflow.com/questions/28078089/is-julia-dynamically-typed" target="_blank">speedups due to type inference</a>. Although, I should admit my formative experiences in Haskell’s <a href="https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system" target="_blank">type inference system</a> may be resurfacing here. These questions are also driving factors behind the speed of Cython and the <a href="https://www.youtube.com/watch?v=wsczq6j3_bA" target="_blank">slowness of Python</a>: dynamic typing is much slower than static typing, and 99% of the time (in scientific computing) <strong>we don’t need dynamic typing</strong>.</p>

<p>In addition, this was also the first time that I encountered such beautiful things as <a href="http://stackoverflow.com/questions/15458613/python-why-is-read-only-property-writable" target="_blank">read-only properties in new-style classes</a>, but not because I wanted to use them. Instead, some base classes we overrode in subclasses would error out when assignment occurred.</p>

<p>The tricky thing is, these errors <strong>only</strong> arose when assignment happened, so some were working correctly in some cases. In addition, it was difficult to identify which properties needed to be overwritten at what point in the hierarchy, because the underlying inheritance structure was less <em>in code</em> and more <em>in filesystem</em> and replicated (by good ol’ <code>CTRL-c</code>).</p>

<p>Overall, the experience of porting a rather large project between versions has taught me:</p>

<ol><li>“<a href="https://www.python.org/dev/peps/pep-0020/" target="_blank">explicit is better than implicit</a>” seems a bit at crossed-purposes with  <a href="https://en.wikipedia.org/wiki/Duck_typing" target="_blank">duck typing</a>, so be explicit about when a duck is a duck if you can.</li>
<li>Namespaces, inheritance, and composition are <em>amazing</em> ideas, and python handles them better than the file system.</li>
<li>Don’t be concerned about “learning” idiomatic Python 3 if you know Python 2.7.  </li>
<li>If given the option to write something in a new feature of a language instead of an old feature, always pick the new feature. By the time you wrap your head around it, it’ll be considered “default” and the old feature will be deprecated.</li>
</ol><p>And, since I’m sitting here, staring at our <code>setup.py</code>, trying to figure out why it won’t correctly <code>build</code> from source, nor install correctly using the <code>build_2to3</code> builder, packaging in Python <a href="https://mail.python.org/pipermail/distutils-sig/2008-October/010210.html" target="_blank">SUCKS</a> and apparently has since 2008.</p>



<small><i> imported from:</i> <a href='https://yetanothergeographer.tumblr.com/125215868119/reflections-on-a-hard-won-win-for-pysal'<tt>yetanothergeographer</tt></a></small>