---
title: "Challenge in Science"
date: 2018-04-20T16:54:18+01:00
---

I just finished attending the [2018 GIS Research UK conference](https://leicester.gisruk.org) at Leicester University. 

I presented twice; once on some new methods in [spatial clustering](https://ljwolf.org/post/2018-gisruk) and once for the [CDRC brexit data analysis competition](http://ljwolf.org/post/cdrc-brexit/). I had a really good time participating in the data analysis competition, and it struck a chord with me reflecting on quite a few conversations I've had with my friend & colleague [Taylor Oshan](https://twitter.com/tayloroshan) and something [Morton O'Kelly](https://geography.osu.edu/people/okelly.1) said at this year's annual American Association of Geographers meeting. 

As graduate students, Taylor and I often talked about the debates in Geography during the 1980s & early 90's over spatial structure in [spatial interaction models](https://www.researchgate.net/profile/Daniel_Griffith3/publication/23536862_Explorations_into_the_Relationship_Between_Spatial_Structure_and_Spatial_Interaction/links/568562e708ae051f9af1deec/Explorations-into-the-Relationship-Between-Spatial-Structure-and-Spatial-Interaction.pdf), the impact of [geography on ecological inference methods](https://onlinelibrary.wiley.com/doi/abs/10.1111/0004-5608.00211), or the debate about [general-to-specific model specifications](https://www.researchgate.net/publication/4791617_The_Relevance_of_Hendry's_Econometric_Methodology_Experimental_Simulation_Results_for_Linear_Spatial_Models) in econometrics in the early 2000s. 
These were quite robust exchanges in the literature; they engaged with specific questions about what methods meant, how they were used, and whether they *actually* solved the problems they intended to or did what they purported to do. 
Indeed, well-heeled geographers still revere & replicate the Hartshorne/Schaffer debate, even though that wasn't really a debate, and more of a single epistle exchange.

I fully admit maybe both Taylor and I are exaggerating the interest and importance of the past's (apparently more) rigorous debate over core constructs in geography. 

## But, I don't think we are exaggerating.

Take, for instance, the recent (nearly 20-year) scholarly debate about potential problems with multicollinearity in Geographically-Weighted techniques. After initial [results](https://link.springer.com/content/pdf/10.1007/s10109-005-0155-6.pdf) spawn a plethora of [debunkings](https://link.springer.com/article/10.1007/s10109-016-0239-5), and just as many re-bunkings, I recently felt compelled to try to [add a new dimension to this debate](https://onlinelibrary.wiley.com/doi/abs/10.1111/gean.12147); focus more on what we can empirically expect from formal differences in estimators, and stop chasing the sometimes-realized-but-not-always-generalizable results from simulation experiments.

I felt compelled to do this precisely because the debate is active, dynamic, interesting, and engaging; we're all talking about the same things, with the same theoretical underpinnings, but focused on different aspects of how those theoretical underpinnings either mesh or fail to. 
There is a large set of papers disagreeing with one another, providing scrutiny of methods, replications of quantitative geographic research, and sharing code/methods/data to ensure that [their horse is the fastest horse in the race.](https://docs.google.com/presentation/d/1yeRuh80ouxB8qr73M5zg2vBmviQij9YVWEccYOIsM1k/edit?usp=sharing)

This is *more than* reproducible science; this is **Contestible** science. 
It means that:

1. someone can understand what you did and what you're arguing. (Cogent science)
2. they care enough to check whether what you're arguing is empirically true or false outside of your laptop. (Replicable science)
3. they find that, for whatever reason, things that convinced you have not convinced them. (Controversial science)
4. they challenge your conclusions or your results on these grounds (Contestible science)

## Replication means contestibility means critique, and critique hurts.

This brings the brilliant question [Morton O'Kelley](https://geography.osu.edu/people/okelly.1) posed into view.
In this vein, he asked (paraphrasing):

> Maybe the fact that we don't have many retractions/post-submission revisions in [Geographical Analysis](https://onlinelibrary.wiley.com/journal/15384632) means no one is paying attention? What would it look like if people were contesting these results? How much of what we do is useful enough to even contest if it were replicable?

Personally, I don't know. 
But, I do know that the few times I've tried to replicate the frontier of quantitative geographic research, it's exceptionally difficult. 
In fact, I've been cautioned by others in the field before to just avoid replication altogether, since that'll take so much more time and effort than publishing novel results. 
Thus, I believe we end up publishing past each other; having parallel conversations on nearly identical topics. 
Distinct conceptual and statistical challenges like those faced in the local models literature are often not as clearly defined or facilitated in other domains.
This is partially because we are unable or unwilling to fully challenge each other's results, but also because doing so then singles out our own work for critique.
It's so hard to replicate, we can't even get to pose meaningful challenges. 

Quantitative geographic research is difficult to replicate because there's no immediate benefit in replicating it --- bummer for us, though: there are clear direct costs in replicating or challenging it, even if the science has clear grounds to pose controversy. 
If you find a bug in someone's code, it's embarrassing. 
If someone finds a bug in your code, it can make you feel panicky and insecure: *What else is wrong? are my results stable?*

Again, the higher profile [Reinhart-Rogoff replication & contestation](https://www.newyorker.com/news/john-cassidy/thereinhart-and-rogoff-controversy-a-summing-up) or the [Lacour fakery](https://en.wikipedia.org/wiki/When_contact_changes_minds) are great examples. 
For much of the less publicly political research, there is no direct incentive to challenge in this manner, so Morton's probably right: we don't retract because, by & large, we don't challenge.
Stakes are low, political incentives lax, personal inertia great, and reputational risk high. 
Thus, the oft-suggested model of effective science, that renown and resource follows those who overturn bad/old knowledge, meets a morass of weak incentives and the inherently (small-c) conservative character of academic practice.

Other than assignations of intrinsic moral good, it's not really clear why *science ought be replicable* when thinking locally, since that means subjecting yourself & someone else (who you might know or like) to potential grievous critique. 
I see this in my students in the UK more than I ever expected: trepidation about challenging things for fear of blowing your own spot.  
[I guess those lines really are the same length.](https://en.wikipedia.org/wiki/Asch_conformity_experiments)
Despite the fact that public replicable research has overwhelming benefits, this incentive structure is the one visible when thinking about your next deadline (or the one you've just missed). 
Our culture needs to change and, if there's one thing the replicability crisis in some corners of science demonstrates, it's that challenge brings clarity, sharpens our ineffective or inefficient interpretations of results, and forces us to think in the somewhat unnatural and cumbersome patterns of more rigorous reasoning. 

## Reproducibility as praxis; competition as the engine of science.

This is where I see more competition-driven models of science as super beneficial for our field. 
Like [Hilbert's Program](https://en.wikipedia.org/wiki/Hilbert%27s_program) or the [Millenium problems](http://www.claymath.org/millennium-problems), focusing attention onto specific, identifiable common problems in a field and incentivizing their solution through reputation or modest financial gains is a great way to help tie together academic conversations.
The benefits of these kinds of competitions/challenge programs is that they *force* us to consider similar/identical problems, speak on common datasets, and actually **engage with each others' research!**
While it's sometimes difficult to frame these questions in sufficiently precise statements as to make them answerable, this is the point of setting collective research agendas. 
The broader quant geographic community may benefit from these kinds of exercises. 
Personally, it was so stimulating to attend the GISRUK CDRC data challenge and see three other groups of people use totally different methods to solve a similar problem on the same data as what I did; their perspectives both helped me deepen my own understanding of *why I did what I did*, and helped me understand how I might do something differently in the future.

## Existing pseudocompetitive modes are not enough.

#### Peer Review

Ideally, this style of replication & challenge might be done in peer review.
Indeed, theoretically, we blind reviewers so that they feel safe enough to challenge.
Unfortunately, we also currently tie their hands. 
While peer review is not replication (nor should it be), how is peer review effective when one can only engage with the work in a very limited way?
Reviewers are often not able to directly access the data or code; without this the mechanisms by which work can really be challenged are curtailed.

Thus, reviewers often must resort to superficial challenges ("That's not how I would've done it.") contextual challenges ("This is useful, but somehow inapt in this specific case."), or conceptual challenges ("The premise of the analysis is fundamentally flawed."). 
Unless the reviewer also happens to have access to (and knowledge of how to use) shared scientific libraries code (or is in possession of their own secret stash), practical challenge ("This conclusion is empirically flawed.") is impossible.
This is not robust.

In contrast to peer review, data competitions & challenge-lead research levels the playing field and clarifies the objectives for review and engagement. 
Instead of the onus being on the author to provide data to source the claims, the facilitator of the competition provides the data. 
This makes the practical challenge possible, providing specific, useful critiques about data analysis, interpretation, or presentation. 
In competitions & challenges, the person who provides the data or the agenda (ideally) has no specific interest in ensuring a given entrant will be successful. 
While the three types of challenge from peer review are still available, the possibility of practical challenge means contextual or conceptual challenges are emboldened: the reviewer has empirical proof of their claims of inapplicability or conceptual error. 
The current way, where data is provided by the author being reviewed, there is incentive for deception, delay, stalling, stonewalling... essentially anything you might read about in the fallout from the [Cornell food lab](https://www.theguardian.com/science/head-quarters/2017/mar/02/fresh-concerns-raised-over-academic-conduct-of-major-us-nutrition-and-behaviour-lab). 

#### The Special Issue/Festscrift

It's also plausible to think this can be handled in special issues or festschrifts. 
However, there again peer review is unable to provide adequate checks on the research. 
And, further, the increasing rapidity and volume of academic publishing means that somewhere, your special issue is happening.

Routine special issues are not special issues at all; instead they reflect a strategy common among popular periodicals.
There, themed issues contain many entries tied together through a common topical focus.
There may be no overarching theoretical or conceptual unity beyond the topic of the special issue or the festschrift, and there is often no coherence on the datasets analyzed or methods used.
Indeed, work that was *too* similar probably would not garner enough readers.

Special issues and festschrifts get us closer to what competition & challenge-driven programs provide, but they are not enough by themselves.
They should aim to be stronger on the vision & agenda; doing so may reduce the breadth of contribution or participation initially, but will also heighten attention to precisely-posed common problems in the short run.
If *these* are fundamental, they will continue to attract attention, and the search for solutions or mitigations takes the characteristics of challenge-driven research. 
Thus, these share the agenda-setting aspect, but leave the competitiveness latent. 

## Competition: it's not about the win, it's about the game. 

One thing that's reaching common consideration in [Political Science is the concept of preregistration.](https://blog.oup.com/2014/09/pro-con-research-preregistration/) 
While I don't know if preregistration is sufficient (nor applicable) to tie together our too-diffuse conversation in quantitative geography, I think that these kinds of challenges focus attention and energy in a way similar to preregistration for special issues.
The point of holding data competition or posing research program questions is to unify, clarify, and motivate research along specific, common paths. 
It's a form of academic agenda-setting; leadership, if you will.  

Announcing intent to do an analysis, having that intent be selected by your peers to pursue, then conducting the analysis and delivering the exact results you promised to allows the facilitators of the competitions to act as mediators for the conversation.
Data challenges & challenge-driven research draw the discipline together a bit more tightly than special issues & broad symposia, since they directly incentivize us to focus: we ask common questions, conduct similar analyses on centrally-provided datasets in pursuit of common answers.
Efficiency, comparability, and knowledge production is clear and easy to integrate into the disciplinary discussion more broadly. 
Plus, as those past literature debates show, some amount of friction is necessary to hone your points.
While I've often heard working with academics as described as herding cats, they [all tend to come running when the treats come out](https://www.youtube.com/watch?v=9EYZnSXEla0).

So, I'll be pulling where I can, however I can to arrange, develop, or participate in these kinds of announced challenges, competitions, or collaborative challenge-driven research events. 