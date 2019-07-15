---
title: "Open Code is Not Enough"
date: 2019-07-15T01:46:40+01:00
---

## Towards a replicable future for geographic data science
#### Levi John Wolf, Sergio J. Rey, \& Taylor M. Oshan

Over the next few days, I'll be attending the [Conceptualizing a Geospatial Software Institute Workshop](http://gsi.cigi.illinois.edu/workshop3/) 
at [SESNYC](https://www.sesync.org/). The original submission is 
[here](https://drive.google.com/file/d/12JOyPTRZlCkV5aBKk02zQes5QKhSP-Ri/view). 
In it, I detail how important it is to ensure that open source *community* is built along side open source software. 
Given the role open source plays in Open Science and the [Open government initiative at the NSF](https://www.nsf.gov/open/), my collaborators and I felt it was critical to represent this perspective at the workshop. 
The full text of our position paper is below the fold. 

<hr>

Open science practices are a large and healthy part of computational
geography and the burgeoning field of spatial data science. In many
forms, open geospatial cyberinfrastructure adheres to a varying and
informal set of practices and codes that empower levels of collaboration
that are impossible otherwise. Often these practices are formalized,
such as the Open Source Initiative's definition of “open source” \[1\]
or Richard Stallman's “Four Freedoms” \[2\]. In practice, however, open
science generally & open geographic data science in particular has been
largely characterized by Stallman’s latter three freedoms:

1.  The freedom to study how the program works and change it.
2.  The freedom to redistribute copies so you can help others.
3.  The freedom to distribute copies of your modified versions to
    others.

Pathbreaking work in geographical sciences has explicitly brought these
concepts into focus for our current model of open science in geography
\[3-6\]. In practice, however, these blend together into a somewhat
ill-advised but easy-to-use working definition of open science: *you
know open science when you see it (on GitHub).*

However, open science lags far behind the needs revealed by this level
of collaboration. Software freedoms mean very little in practice if they
are not exercised. That is, offering scientific software under
permissive licenses for free on the internet is (surprisingly) not a
cure-all for open science. What good is the ability to modify someone
else's R package if it's not intelligible? Scientific source code is
rarely “self documenting,” no matter how many literate programming
frameworks or packages are used to embed expository discussion inside of
programs. So-called “write-only software,” designed only to finish a
specific academic project, is unfortunately common. And, while academics
are not (and, indeed, must never be) full research software engineers,
some middle ground must be found.

Despite the freedom to edit & redistribute scientific software, thorough
and effective documentation is simply *not done* at the cutting edge of
scientific work. The cost of communicating complex code knowledge is
often too high; the difficulty of writing good code documentation tends
to be much more difficult than writing good code itself. Finally, the
mathematics and statistical content of new scientific work is often
itself difficult to understand. It may be the case that only a small set
of authors understand the grounds of the work in the first place,
suggesting that the ability to see, study, & modify the source code has
incredibly limited practical relevance.

Further (and possibly more concerning), current challenges to
*replication* in science imply a different set of problems that require
a different set of solutions than the nominal provenance of software
freedoms. *Shallow replicability*, the ability to run the code used in
an analysis on the same data and achieve the same result, only ensures
Stallman’s “Zeroth” freedom to run the program as you wish, for any
purpose. More complicated concepts in *deep replicability*, involve the
ability to transport ideas or conclusions from study to study; the
“didactic relevance” \[7\] of a finding provides it with general
relevance. This requires open science to be much more than push-button
repeatability or open source; we must also *actively *ensure that
software freedoms are accessable to other scientists. Thus, **it is not
enough to create open scientific software**,** we must also strengthen
the open science community**.

As a new Co-Maintainer of the Python Spatial Analysis Library, my
colleagues and I have implemented practices for the cultivation &
maintenance of an open source community of practice, united around
common standards and agreements about code quality, style, and
documentation. In this talk, I will discuss a few of the best practices
we, as a project, have developed, including:

1.  Using a “developer contract” to provide consistency across aligned
    (but separate) projects.
2.  Instituting active, robust scientific code review for papers,
    programs, and pedagogical material.
3.  Creating easy, effective, and open technical spaces for contributors
    to write and maintain documentation for fellow developers.
4.  Building space, tools, and practices to help junior developers get
    invested and involved.
5.  Developing mentoring so interested users can eventually become
    developers themselves.
6.  Ensuring each problem is solved exactly one time.
7.  Providing simple, effective, and usable documentation for external
    scientific users.

The practices we have developed are, by no means, the only ones possible
for open replicable science. However, we hope that by sharing them with
*Workshop 3: Strategic Plan & Governance of GSI*, we can instil the
important message, that open science is not enough to ensure
reproducible science; we also must have a sustainable & open scientific
community.

\[1\] Open Source Initiative, *The Open Source Definition*.
[*https://opensource.org/osd*](https://opensource.org/osd). Accessed: 9
October, 2018

\[2\] Stallman, Richard. *What is Free Software?
*[*https://www.gnu.org/philosophy/free-sw.en.html*](https://www.gnu.org/philosophy/free-sw.en.html)
Accessed: 9 October, 2018

\[3\] Rey, Sergio J. and Luc Anselin, “PySAL: A Python Library of
Spatial Analytical Methods,” *The Review of Regional Studies* 37, no. 1
(2007): 5–27;

\[4\] Rey, Sergio J. "Open regional science." *The Annals of Regional
Science* 52, no. 3 (2014): 825-837.

\[5\] Rey, Sergio J. “Show Me the Code: Spatial Analysis and Open
Source,” *Journal of Geographical Systems* 11, no. 2 (June 2009):
191–207,[
](https://doi.org/10.1007/s10109-009-0086-8)[*https://doi.org/10.1007/s10109-009-0086-8*](https://doi.org/10.1007/s10109-009-0086-8);

\[6\] Rey, Sergio J. et al., “Open Geospatial Analytics with PySAL,”
*International Journal of Geo-Information* 4 (2015): 815–836.

\[7\] Johnston, Ron. “On the Nature of Explanation in Human Geography,”
*Transactions of the Institute of British Geographers* 5, no. 4 (1980):
402–412.
