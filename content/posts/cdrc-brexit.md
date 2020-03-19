---
title: "GISRUK I: CDRC Brexit Analysis Competition"
date: 2018-04-18T01:46:46+01:00
---
My entry in the [Consumer Data Research Center's Brexit Data Competition](http://leicester.gisruk.org/cdrc-gisruk-data-challenge/) is called ["Tension Points: A Theory & Evidence"](https://docs.google.com/presentation/d/1Umyoq7FnylmWPPZh9sJsQvI5oWUShf-8zbzOmV3rcqs/edit?usp=sharing) ([static](talks/wolf_gisruk2018_brexit.pdf)), which I talked about at the [2018 GISRUK conference](http://leicester.gisruk.org/)

There is [an abstract](papers/2018_cdrcdc.pdf) describing some of the work that I submitted to get to the final round, but if you're computationally inclined, you'll find everything sufficient to replicate my modelling & analysis in this [Jupyter Notebook](notebooks/2018_gisruk-clean_bregmans.html) ([raw](notebooks/2018_gisruk-clean_bregmans.ipynb)). You'll need `scikit-learn`, `pystan`, `statsmodels`, and `geopandas` at minimum to run. Also, the data is in a `sqlite` data store, but it's too large for me to host on GitHub and part of it is protected by the CDRC, so I cannot share it until these protections are lifted. 

# The long & short of it:

I find that [the Economist's claim](https://www.economist.com/news/britain/21702228-areas-lots-migrants-voted-mainly-remain-or-did-they-explaining-brexit-vote), that "high numbers of migrants don't bother Britons, high rates of change do," is only partially correct. It depends on the type of change. 

I split change into four types using a theory about how individuals may be changing/influenced in their vote choice by changes in their community:
<ol>
<li> Non-UK born population changes ("people who aren't like me") </li>
<li> Migrants from outside the UK ("people changing the community who are new to Britain") </li>
<li> Migrants from within the UK ("people changing the community who are new to the community")</li>
<li> Population structure volatility ("ethnicity/character of the community changing regardless of origin")</li>
</ol>

I find that raw non-UK population changes (factor 1) and Migrants *from within the UK* (factor 3) are the ones associated with a place voting Leave. Further, I find that the opposite occurs for factors (2,4), which are actually associated with Remain voting. This suggests that both population growth and internal migration within the UK matter to drive Brexit voting. It's not enough to just consider the raw foreign born population changes, since those provide an incomplete picture of how change in population structure affected Leave voting. 

I find this in a varying-slopes model, which allows for different regions of England & Wales to have different levels of baseline Brexit support. Further, I also model the first-order effects of many of these changes using the non-UK born population (white & ethnic), which provides the first-order effects for the various measures. 
