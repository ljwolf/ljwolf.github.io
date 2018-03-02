---
Post ID: 149764483499
Date: Wed, 31 Aug 2016 18:08:02
Url with slug: the-joys-of-using-political-data
Reblog key: WYv4gsp8
Reblog Url:
Reblog Name:
Title: The joys of using political data
---<p>When you work with Census-derived data, you can usually be sure it meets some standard of regularity, which makes it easy to use with other data. Where it doesn’t, people have built </p><p>When you work with congressional district data, you’re in for a bad time. </p><p>Right now, I’m working on relating the <a href="http://www.electiondataarchive.org/index.html" target="_blank">CLEA</a> to the <a href="http://cdmaps.polisci.ucla.edu" target="_blank">cdmaps</a> maintained by UCLA.</p><p>This poses a few challenges. First, CLEA records are one big, flat table of candidates who’ve run in a contest within some legislative constituency. Thus, in the US, a first-past-the-post single-member-district elections system, you might think it reasonable that each “contest” in an election year can be mapped to one district in each election cycle. Then, one district in each election has one “contest” with one winner. If youi’re interested in modeling constituency-level electoral behavior, this seems like a reasonable databasing scheme. </p><p>But, political geographies don’t like regularity. For example, in 1962, <a href="https://en.wikipedia.org/wiki/United_States_House_of_Representatives_elections,_1962#Alabama" target="_blank">Alabama decided to elect all candidates at large</a>. So, while the US election system is <i>typically</i> a single-member district electoral system, it’s not always one. While papers like <a href="http://pan.oxfordjournals.org/content/20/3/400" target="_blank">Linzer (2012)</a> omit this particularly tricky one by removing southern districts, boiling the CLEA down to contest-level is a fun little databasing exercise. <br/></p><p>Alternatively, I might just pull down the replication data from Linzer and use that&hellip; haven’t decided yet. </p>
Tags:

Post ID: 149447103099, Date: Thu, 25 Aug 2016 00:18:37