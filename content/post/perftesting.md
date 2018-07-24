---
title: "Perftesting"
date: 2018-07-24T11:30:20-07:00
---

I've always been interested in how [PySAL](https://github.com/pysal/pysal) stacks up with [NetworkX](https://githu.com/networkx/networkx) for building the dual graph of polygonal lattices, so [I did some perftesting](notebooks/perftesting_graphs.html). This is by far the most common spatial operation I do on a day to day basis, and it looks like PySAL's constructors still are the fastest for cases where we can assume planarity. But, with how simple the `geopandas`-only solution is, I look foward to the day when the `geopandas.sindex` code is sufficient. 

