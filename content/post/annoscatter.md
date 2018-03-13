---
title: "Annoscatter"
date: 2018-03-12T23:52:32Z
tags: ["python"]
---

I found this function super useful in my dissertation and more generally in my work. What it does is take `x`,`y` coordinates and a set of strings and annotates a scatterplot using those labels. For example, here's a figure from my dissertation where I use it to annotate a plot of regression leverage by year. I jitter the points a little to provide legibility to the text labels, but basically it's just a call like `annoscatter(df.year, df.leverage, df.district_id)`. 

![annoscatter](images/annoscatter.png)

Not sure if there's a more elegant solution to this using matplotlib, but I really like how it looks, especially since there's also passthrough keyword dictionaries implemented like seaborn. Not sure if [@mwaskom](https://github.com/mwaskom) would take a PR for something as simple as this, though. 

```python
import matplotlib.pyplot as plt
def annoscatter(x,y, text, marker='.', markersize=1, 
                ax = None, fig = None, color='k',
                fig_kw = dict(), 
                scatter_kw = dict(), ann_kw=dict()):
    if ax is None:
        if fig is None:
            fig = plt.figure(**fig_kw)
        ax = plt.gca()
    if not (marker is '' or markersize == 0):
        scatter_kw['marker'] = marker
        scatter_kw['s'] = markersize
        ax.scatter(x,y, **scatter_kw)
    else:
        scatter_kw['alpha'] = 0
        ax.scatter(x,y,**scatter_kw)
    if isinstance(color, str):
        colors = [color]*len(x)
    else:
        colors = list(color)
    for xi,yi,string,c in zip(x,y,text,colors):
        ax.annotate(s=string, xy=(xi,yi), color=c, **ann_kw)
    return ax
```
