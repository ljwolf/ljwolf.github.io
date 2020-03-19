---
title: Spatial Autocorrelation Functions
date: 2018-03-09T17:34:20Z
tags: ["spatial autocorrelation", "esda", "dissertation", "swing", "elections"]
---
I looked into using spatial autocorrelation functions in my dissertation to characterize the ``scale'' at which processes operate electorally. 
I did an analysis of presidential vote by county, trying to identify where, exactly, clusters of votes tend to become decorrelated. 
The typical diameter at which the so-called "spatial autocorrelation function" goes to zero denotes how wide a typical spatial cluster might be,
and the partial spatial autocorrelation function gives an anticipated order at which spatial autocorrelation may hold. 

This will be published along with my dissertation when it becomes unembargoed. I also gave a talk on this in the [2017 AAG](https://docs.google.com/presentation/d/1JHbZ1gQTgQ-v9x09udPhfxDGfxNyWo39qRYCogGchHU/edit?usp=sharing). So, below is the initial exploration of what a spatial autocorrelation/partial autocorrelation function might look like. 

```python
import pysal as ps
import numpy as np
import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
sns.set_style('white')
%matplotlib inline
```

To talk about the spatial (partial) autocorrelation function, which is kind of like a mixture in concept between the geostatistical variogram and the (partial) autocorrelation function in time series analysis, let's use presidential vote choice results at the county level for 2008, 2012, and 2016. 

To do this, I'll first grab the results from a github repo I've been tracking. Thanks to user [@tonmcg](https://github.com/tonmcg) for making this available in plaintext, so we can grab it using pandas without downloading it.


```python
votes = pd.read_csv('https://raw.githubusercontent.com/tonmcg/'
            'County_Level_Election_Results_12-16/master/'
            'US_County_Level_Presidential_Results_08-16.csv')
```

Since the spatial autocorrelation function (and variogram) are related to the *spatial positions* of our data (or, in the least, a topological arrangement of our data), we need to merge these county-level results with the actual geometries of each county. To do this, I'll use the example county dataset in PySAL, [the Python spatial analysis library](https://github.com/pysal/pysal). 


```python
geoms = gpd.read_file(ps.examples.get_path('NAT.shp'))
```


```python
votes.head()
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>fips_code</th>
      <th>county</th>
      <th>total_2008</th>
      <th>dem_2008</th>
      <th>gop_2008</th>
      <th>oth_2008</th>
      <th>total_2012</th>
      <th>dem_2012</th>
      <th>gop_2012</th>
      <th>oth_2012</th>
      <th>total_2016</th>
      <th>dem_2016</th>
      <th>gop_2016</th>
      <th>oth_2016</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>26041</td>
      <td>Delta County</td>
      <td>19064</td>
      <td>9974</td>
      <td>8763</td>
      <td>327</td>
      <td>18043</td>
      <td>8330</td>
      <td>9533</td>
      <td>180</td>
      <td>18467</td>
      <td>6431</td>
      <td>11112</td>
      <td>924</td>
    </tr>
    <tr>
      <th>1</th>
      <td>48295</td>
      <td>Lipscomb County</td>
      <td>1256</td>
      <td>155</td>
      <td>1093</td>
      <td>8</td>
      <td>1168</td>
      <td>119</td>
      <td>1044</td>
      <td>5</td>
      <td>1322</td>
      <td>135</td>
      <td>1159</td>
      <td>28</td>
    </tr>
    <tr>
      <th>2</th>
      <td>1127</td>
      <td>Walker County</td>
      <td>28652</td>
      <td>7420</td>
      <td>20722</td>
      <td>510</td>
      <td>28497</td>
      <td>6551</td>
      <td>21633</td>
      <td>313</td>
      <td>29243</td>
      <td>4486</td>
      <td>24208</td>
      <td>549</td>
    </tr>
    <tr>
      <th>3</th>
      <td>48389</td>
      <td>Reeves County</td>
      <td>3077</td>
      <td>1606</td>
      <td>1445</td>
      <td>26</td>
      <td>2867</td>
      <td>1649</td>
      <td>1185</td>
      <td>33</td>
      <td>3184</td>
      <td>1659</td>
      <td>1417</td>
      <td>108</td>
    </tr>
    <tr>
      <th>4</th>
      <td>56017</td>
      <td>Hot Springs County</td>
      <td>2546</td>
      <td>619</td>
      <td>1834</td>
      <td>93</td>
      <td>2495</td>
      <td>523</td>
      <td>1894</td>
      <td>78</td>
      <td>2535</td>
      <td>400</td>
      <td>1939</td>
      <td>196</td>
    </tr>
  </tbody>
</table>
</div>



Then, to merge things up, I'll create a common key based on the FIPS code of the county and merge the data


```python
votes['FIPS'] = votes.fips_code.apply(lambda x: str(x).rjust(5,'0'))
```


```python
votes = pd.merge(votes, geoms[['FIPS', 'STATE_NAME', 'geometry']], how='right', on='FIPS')
votes = gpd.GeoDataFrame(votes)
```

Finally, since I'm mostly interested in two-party vote shares, rather than raw votes, I'll construct the two party vote share in each year as:

\\[ tpv\_{it} = \frac{d\_{it}}{d\_{it} + r\_{it}} \\]

where \(d\_{it}\) is raw vote cast in county \(i\) for the Democrat candidate in time \(t\), and \(r\_{it}\) is the comparable raw vote cast for the Republican candidate. We can just do simple series operations to get this done:


```python
votes['tpv_2008'] = votes.dem_2008 / (votes.dem_2008 + votes.gop_2008)
votes['tpv_2012'] = votes.dem_2012 / (votes.dem_2012 + votes.gop_2012)
votes['tpv_2016'] = votes.dem_2016 / (votes.dem_2016 + votes.gop_2016)
```

These distributions tend to appear Gaussian, if not a slightly skewed Gaussian. But, we're not really making any distributional analyses (like we were in my [exploratory spatial regression notebook](http://ljwolf.org/post/153806452919/a-short-exploration-of-the-2016-electoral-swing)), so I'll let this sit for now. 


```python
f,ax = plt.subplots(1,3, figsize=(2*3*1.6, 2))
for i,col in enumerate(['tpv_2008','tpv_2012','tpv_2016']):
    sns.kdeplot(votes[col].values, shade=True, color='slategrey', ax=ax[i])
    ax[i].set_title(col.split('_')[1])
```

    /home/ljw/anaconda3/envs/py3/lib/python3.5/site-packages/statsmodels/nonparametric/kde.py:454: RuntimeWarning: invalid value encountered in greater
      X = X[np.logical_and(X>clip[0], X<clip[1])] # won't work for two columns.
    /home/ljw/anaconda3/envs/py3/lib/python3.5/site-packages/statsmodels/nonparametric/kde.py:454: RuntimeWarning: invalid value encountered in less
      X = X[np.logical_and(X>clip[0], X<clip[1])] # won't work for two columns.
    /home/ljw/anaconda3/envs/py3/lib/python3.5/site-packages/statsmodels/nonparametric/kdetools.py:20: VisibleDeprecationWarning: using a non-integer number instead of an integer will result in an error in the future
      y = X[:m/2+1] + np.r_[0,X[m/2+1:],0]*1j



![png](images/spacf_output_12_1.png)


Next, since I'll be mapping our data, I'll use [geopandas](https://github.com/geopandas/geopandas) to reproject the raw data from PySAL (in Plate Caree projection) into a better projection for choropleth mapping, the Albers Equal Area Conic projection:


```python
votes.crs = {'init':'epsg:4326'}
votes = votes.to_crs(epsg='5070')
```

Now, before we get any further, let's make some maps of the two-party vote shares in 2008, 2012, and 2016 (alongside the vote distributions), and explore what spatial distribution dynamics might be going on:


```python
f,ax = plt.subplots(3,2, figsize=(1.6*6 + 1,6*3), gridspec_kw=dict(width_ratios=(6,1)))
for i,col in enumerate(['tpv_2008','tpv_2012','tpv_2016']):
    votes.plot(col, linewidth=.05, cmap='RdBu', ax=ax[i,0])
    ax[i,0].set_title(col.split('_')[1] + ' Two Party Vote (% Dem)')
    ax[i,0].set_xticklabels('')
    ax[i,0].set_yticklabels('')
    sns.kdeplot(votes[col].values, ax=ax[i,1], vertical=True, shade=True, color='slategrey')
    ax[i,1].set_xticklabels('')
    ax[i,1].set_ylim(0,1)
f.tight_layout()
plt.show()
```

    /home/ljw/anaconda3/envs/py3/lib/python3.5/site-packages/statsmodels/nonparametric/kde.py:454: RuntimeWarning: invalid value encountered in greater
      X = X[np.logical_and(X>clip[0], X<clip[1])] # won't work for two columns.
    /home/ljw/anaconda3/envs/py3/lib/python3.5/site-packages/statsmodels/nonparametric/kde.py:454: RuntimeWarning: invalid value encountered in less
      X = X[np.logical_and(X>clip[0], X<clip[1])] # won't work for two columns.
    /home/ljw/anaconda3/envs/py3/lib/python3.5/site-packages/statsmodels/nonparametric/kdetools.py:20: VisibleDeprecationWarning: using a non-integer number instead of an integer will result in an error in the future
      y = X[:m/2+1] + np.r_[0,X[m/2+1:],0]*1j



![png](images/spacf_output_16_1.png)


One thing that's super clear when you do these maps of two-party vote is that more counties tend to vote Republican than Democrat. In the KDE plots, you see this as the mode of the vote share distribution is well below .5, even in 2012, when President Obama won reelection handily. While the best analysis might be to drill all the way down to the voter tabulation district level, that data *attached* to its geographies is pretty hard to find, and often to large for most to work with on a national scale. I've been working on putting it together in an sqlite dump, but that takes time :)

The second thing that's clear is that the collapse of the "blue wall," Minnesota, Wisconsin, Michigan, looks like it was actually a gradual process at the county level. Lots of marginally-blue counties flipped, resulting in a statewide flip. As a geographer, another thing that's interesting about the electoral mosaic is almost how indistinguishably rural Illinois is from its surrounding areas in MO and KY. I think (if I were to finish my PhD and move into some electoral modeling), I would **seriously** look into markov random field models (say a hierarchical SAR/CAR model) of this process, since the state-based hierarchical models like will miss this type of proximity-based correlation entirely.

# How are counties related in time?

This is a reasonable first question. It's well known that past two-party vote share tends to predict future two-party vote share quite well at an aggregate level and over all ranges of vote share. Of course, what really matters in the end are how well the *wins in each state* correlate over time, which is a different question. While we could address this with county-level vote, I'm using the county-level data to look at distribution dynamics, so I'll let that slide for now. 

First, we drop the counties where we're missing data:


```python
votes.dropna(subset=['tpv_2008', 'tpv_2012', 'tpv_2016'], inplace=True)
```

And, if we make a scatterplot of the past vote (on X axes) and the future vote (on the Y axes), we see that the correlation is very strong, both when comparing 2008 vs. 2012 and 2012 vs. 2016. 

However, what's also clear is that 2012 vs. 2016 has lower correlation, especially in the range of competitve counties (between ~.4 and ~.6). I'll be looking into competitve counties (and legislative seats) later. 


```python
f,ax = plt.subplots(1,2, figsize=(4*2.1,4))
votes[['tpv_2008', 'tpv_2012']].plot.scatter('tpv_2008', 'tpv_2012', ax=ax[0])
ax[0].set_xlabel('2008 Two Party Vote (% Dem)')
ax[0].set_ylabel('2012 Two Party Vote (% Dem)')
ax[0].axis([0,1,0,1])
r = np.corrcoef(votes['tpv_2008'].values, votes['tpv_2012'].values)[0,1]
ax[0].text(.6,.2, s=r'$\rho = {:.3f}$'.format(r), fontsize=20)
votes[['tpv_2012', 'tpv_2016']].plot.scatter('tpv_2012', 'tpv_2016', ax=ax[1])
ax[1].set_xlabel('2012 Two Party Vote (% Dem)')
ax[1].set_ylabel('2016 Two Party Vote (% Dem)')
ax[1].axis([0,1,0,1])
r = np.corrcoef(votes['tpv_2012'].values, votes['tpv_2016'].values)[0,1]
ax[1].text(.6,.2, s=r'$\rho = {:.3f}$'.format(r), fontsize=20)
f.tight_layout()
plt.show()
```


![png](images/spacf_output_21_0.png)


Since we only have two time periods, the autocorrelation plot of this would look rather uninteresting. I've been working on this at the congressional district level over the 20th (and now 21st) centuries using some data I grabbed from the [CLEA](http://www.electiondataarchive.org/datacenter.html), mixed with a little [ICPSR6311](http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/6311), and merged with the [UCLA collection of congressional districts](http://cdmaps.polisci.ucla.edu), and hopefully getting released through the research cluster I work with at [UChicago](https://spatial.uchicago.edu/). Again, this all takes time, but the data is ready to go, so ask me if you're interested. 

### How about in space?

Of course, an interesting question also might be to look for clusters in vote. We know about rural/urban divides  and regional divides in American voting, so we would expect some pretty strong correlation between neighbors at a county level.

However, what's the *order* of this process? That is, how far away are counties related to one another?

This has a pretty clear analogue in time-series autocorrelation analysis. The autocorrelation function for a serially-correlated signal computes the correlation between the signal at time \(t\) and the signal at time \(t-k\), where \(k\) is some arbitrary lag. A related concept, the *variogram* in spatial statistics, computes the variance of the difference between locations as they get further and further apart. The partial autocorrelation function (which relates the signal at \(t\) and \(t-k\) when accounting for all lags between), is also available in a geostatistical context by conditioning the variogram on adjacent pairs below the range. But, this is incredibly computationally intensive (and the variogram is sufficient for all kinds of geostatistical models), so the partial variant is much less well used. 

Unfortunately, the *scale* of the US county system in terms of the distances between places gets much larger as we get west than when we are in the east. One way this is handled in spatial econometrics is to use the adjacency matrix to define neighborhoods. In this case, adjacent counties are considered neighbors, regardless of the actual distance between counties. This allows the connectivity graph relating observations to have a similar density when the polygons being related dilate but keep the same topology. I'll plot this graph over the counties below. Here, I use rook contiguity, which means two counties are adjacent if they share a boundary. 


```python
W = ps.weights.Rook.from_dataframe(votes)
f = plt.figure(figsize=(1.6*8, 8))
ax = plt.gca()
votes.plot(linewidth=.1, color='white', ax=ax)
for idx, neighbors in W:
    centroids = votes.ix[neighbors].geometry.apply(lambda pgon: (pgon.centroid.x, pgon.centroid.y))
    centroids = np.vstack(centroids.values)
    focal = np.hstack(votes.ix[idx].geometry.centroid.xy)
    for neighbor in centroids:
        ax.plot(*zip(focal, neighbor), color='firebrick', linewidth=.1)
plt.xticks([])
plt.yticks([])
plt.title('Rook Contiguity for US Counties')
plt.show()
```


![png](images/spacf_output_25_0.png)


With this adjacency matrix, we can compute a few interesting spatial statistics. The first, the Bivariate Moran statistic (from [Wartenburg (1985)](dx.doi.org/10.1111/j.1538-4632.1985.tb00849.x), a kind of [Mantel statistic](https://en.wikipedia.org/wiki/Mantel_test)), relates a set of observations to the *spatial lag* of another set of observations. 

To be clear, the spatial lag is analogous to the temporal lag of a variate. In this case, the *spatial lag* refers to the average of the neighboring values around each observation. Using a row-standardized adjacency matrix \(\mathbf{W}\), the lag of \(Y\) is expressed simply as \(\mathbf{W}Y\). 

This means that the bivariate Moran's I statistic is stated for centered attribute vectors \(y\) and \(x\):
\\[ \frac{x'\mathbf{W}y}{x'x}\\]

This results in a single statistic (and accompanying \(p\)-values computed using permutation methods) that relates the values of attribute \(x\) to the *lag* of \(y\). We can use this statistic to relate votes between two times. In the following, we see that county vote in the previous year is a good predictor of the vote in the next year:


```python
bvi = ps.Moran_BV(votes['tpv_2008'], votes['tpv_2012'], w=W)
bvi.I, bvi.p_sim
```




    (0.59905458256875777, 0.001)




```python
bvi = ps.Moran_BV(votes['tpv_2012'], votes['tpv_2016'], w=W)
bvi.I, bvi.p_sim
```




    (0.57711482669716285, 0.001)



Another way to look into this might be to look for clusters of volatility in how the vote changes betwen year to year. To do this, we'll be using the quadrants of the [Moran Scatterplot](https://docs.google.com/presentation/d/1ePyu_eDvTUOp7jZc8XnTXbGEYexCGzd1mzpQ7gqksu8/edit#slide=id.g84b4ad007_0_113) to interpret local indicators that show whether some counties are swinging together with their neighbors, or if some counties are swinging in opposition to their neighbors. Moran statistics, computable in PySAL, allow us to determine both the relative direction (in terms of more or less Republican) and the neighborhood dynamics (in terms of how the nearby counties move). The local moran statistic for a vector of centered observations \(z\) is computed:

\\[ I\_i = \frac{z\_i W\_z z}{z'z} \\]


```python
mli = ps.Moran_Local(votes.tpv_2016 - votes.tpv_2012, w=W)
votes['mlocal_1216'] = mli.Is
votes['mlocal_p_1216'] = mli.p_sim
votes['mlocal_quad_1216'] = mli.q
```


```python
sns.distplot(mli.y)
plt.title('Change in two-party vote between 2012 and 2016')
```

    /home/ljw/anaconda3/envs/py3/lib/python3.5/site-packages/statsmodels/nonparametric/kdetools.py:20: VisibleDeprecationWarning: using a non-integer number instead of an integer will result in an error in the future
      y = X[:m/2+1] + np.r_[0,X[m/2+1:],0]*1j





    <matplotlib.text.Text at 0x7f99eec38ac8>




![png](images/spacf_output_31_2.png)



```python
lmos = votes.sort_values(['mlocal_quad_1216', 'mlocal_p_1216'], ascending=False)[['county', 'STATE_NAME',
                                                                                 'tpv_2012', 'tpv_2016',
                                                                                   'mlocal_1216',
                                                                                   'mlocal_p_1216', 
                                                                                   'mlocal_quad_1216',
                                                                                   ]]
```

Then, we'll do some examination by the quadrant of the scatterplot. To make everything more clear, you might want to read this alongside [this plot](https://docs.google.com/presentation/d/1ePyu_eDvTUOp7jZc8XnTXbGEYexCGzd1mzpQ7gqksu8/edit#slide=id.g84b4ad007_0_113). We'll step by quadrant of that scatterplot:

## Spatial Clusters in Vote Swing

### Quadrant I

Observations in quadrant I are counties where both the focal county and its neighbors had large increases in democratic vote share. These would represent counties where both the focal and its neighbors intensified in support for Democrats. Unsurprisingly, we see that the largest cluster strengths (in terms of the size of the `mlocal_1216` statistic) occur in Utah & Virginia, states that were known to swing pretty strongly towards Clinton. 

Surprises might be that Montgomery County, MD and those Georgia counties where Clinton was rumored to be surging in October show up here as well. Interestingly in some, cases (like the Utah counties or Georgia counties) this improvement is sometimes from *very* low (say 14% in Cache county, UT) to much better (30% in Cache county, UT). Thus, a lot of this is probably occuring among weak partisans who might be swayed by the (supposedly hefty) respectability bias about Trump, but who might otherwise vote Republican with a clear conscience. 

And, unsurprisngly, you also see some consolidation going on, where county shifts towards the Democrats intensified in states typically won by Democrats, like the California counties in the list below. 


```python
(lmos.query('mlocal_p_1216 < .01 and mlocal_quad_1216 == 1')
   .sort_values(['mlocal_1216'], ascending=False)
   .head(25))
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>county</th>
      <th>STATE_NAME</th>
      <th>tpv_2012</th>
      <th>tpv_2016</th>
      <th>mlocal_1216</th>
      <th>mlocal_p_1216</th>
      <th>mlocal_quad_1216</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>2125</th>
      <td>Salt Lake County</td>
      <td>Utah</td>
      <td>0.400350</td>
      <td>0.580997</td>
      <td>13.797329</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>538</th>
      <td>Davis County</td>
      <td>Utah</td>
      <td>0.184858</td>
      <td>0.328788</td>
      <td>11.869800</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>101</th>
      <td>Falls Church City</td>
      <td>Virginia</td>
      <td>0.700434</td>
      <td>0.814524</td>
      <td>10.397833</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>942</th>
      <td>Wasatch County</td>
      <td>Utah</td>
      <td>0.234328</td>
      <td>0.332265</td>
      <td>9.125809</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>376</th>
      <td>Arlington County</td>
      <td>Virginia</td>
      <td>0.702165</td>
      <td>0.820099</td>
      <td>9.110773</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>2078</th>
      <td>Weber County</td>
      <td>Utah</td>
      <td>0.264168</td>
      <td>0.365917</td>
      <td>8.375549</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>2289</th>
      <td>Cache County</td>
      <td>Utah</td>
      <td>0.149895</td>
      <td>0.304994</td>
      <td>7.994395</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>466</th>
      <td>Morgan County</td>
      <td>Utah</td>
      <td>0.089851</td>
      <td>0.153433</td>
      <td>7.805697</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>924</th>
      <td>Alexandria City</td>
      <td>Virginia</td>
      <td>0.721844</td>
      <td>0.811886</td>
      <td>7.589085</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>522</th>
      <td>Charlottesville City</td>
      <td>Virginia</td>
      <td>0.772119</td>
      <td>0.858234</td>
      <td>7.025966</td>
      <td>0.007</td>
      <td>1</td>
    </tr>
    <tr>
      <th>601</th>
      <td>Fairfax County</td>
      <td>Virginia</td>
      <td>0.600144</td>
      <td>0.691918</td>
      <td>6.617549</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>1447</th>
      <td>Utah County</td>
      <td>Utah</td>
      <td>0.099862</td>
      <td>0.212752</td>
      <td>6.593047</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>2737</th>
      <td>Summit County</td>
      <td>Utah</td>
      <td>0.474938</td>
      <td>0.588766</td>
      <td>5.726012</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>3074</th>
      <td>Tooele County</td>
      <td>Utah</td>
      <td>0.236622</td>
      <td>0.292895</td>
      <td>5.218250</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>2917</th>
      <td>San Diego County</td>
      <td>California</td>
      <td>0.526027</td>
      <td>0.589864</td>
      <td>5.021068</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>1894</th>
      <td>Montgomery County</td>
      <td>Maryland</td>
      <td>0.720859</td>
      <td>0.788706</td>
      <td>4.783064</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>1488</th>
      <td>Forsyth County</td>
      <td>Georgia</td>
      <td>0.180902</td>
      <td>0.251287</td>
      <td>4.768546</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>731</th>
      <td>San Mateo County</td>
      <td>California</td>
      <td>0.728657</td>
      <td>0.799851</td>
      <td>4.744300</td>
      <td>0.003</td>
      <td>1</td>
    </tr>
    <tr>
      <th>2328</th>
      <td>Gwinnett County</td>
      <td>Georgia</td>
      <td>0.452505</td>
      <td>0.529853</td>
      <td>4.712442</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>2622</th>
      <td>Fulton County</td>
      <td>Georgia</td>
      <td>0.649931</td>
      <td>0.718616</td>
      <td>4.625660</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>2727</th>
      <td>Orange County</td>
      <td>California</td>
      <td>0.457686</td>
      <td>0.527198</td>
      <td>4.503098</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>789</th>
      <td>Teton County</td>
      <td>Idaho</td>
      <td>0.439325</td>
      <td>0.499537</td>
      <td>4.346220</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
    <tr>
      <th>487</th>
      <td>Ventura County</td>
      <td>California</td>
      <td>0.527095</td>
      <td>0.585385</td>
      <td>4.309279</td>
      <td>0.002</td>
      <td>1</td>
    </tr>
    <tr>
      <th>130</th>
      <td>Santa Barbara County</td>
      <td>California</td>
      <td>0.587822</td>
      <td>0.650300</td>
      <td>4.283089</td>
      <td>0.003</td>
      <td>1</td>
    </tr>
    <tr>
      <th>1700</th>
      <td>Box Elder County</td>
      <td>Utah</td>
      <td>0.102815</td>
      <td>0.149448</td>
      <td>4.258581</td>
      <td>0.001</td>
      <td>1</td>
    </tr>
  </tbody>
</table>
</div>



### Quadrant III

On the opposite side of the origin in the Moran scatterplot, quadrant III would indicate "low-low" clusters, areas where the two-party vote decreased significantly in both the focal and neighboring units. Thus, these would be clusters of intensifying Republican support. 

In these, you see precipitous drops in Democrat support in Missouri, Ohio, Iowa, and a cluster centered around Calhoun county, WV. This largely comports with the narrative that Ohio dropped out of being *truly* contested this cycle, with eventual vote totals falling well below the expected contestible range. Notably, this was *spatially correlated*, so not only did this affect counties in Ohio, but these clusters indicate that there was spillovers. 

Like, for Monroe, Guernsey, Noble, Morgan County, OH, bordering WV, this analysis indicates that that group of counties swung *hard* towards the Republicans, and did so in a way that's statistically nonrandom in terms of the spatial location of those counties in Ohio. Notably *absent* from this are counties in the northwest of Ohio, abutting Michigan, that might indicate nascent spillovers between those states. 

It also seems that heartland areas (Henderson county, IL, as well as the counties in southern Iowa and eastern MO, also swung together in a spatially-cluster for Trump.


```python
(lmos.query('mlocal_p_1216 < .01 and mlocal_quad_1216 == 3')
   .sort_values(['mlocal_1216'], ascending=False)
   .head(25))
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>county</th>
      <th>STATE_NAME</th>
      <th>tpv_2012</th>
      <th>tpv_2016</th>
      <th>mlocal_1216</th>
      <th>mlocal_p_1216</th>
      <th>mlocal_quad_1216</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>1764</th>
      <td>Clark County</td>
      <td>Missouri</td>
      <td>0.446931</td>
      <td>0.227530</td>
      <td>5.669215</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>797</th>
      <td>Howard County</td>
      <td>Iowa</td>
      <td>0.606798</td>
      <td>0.390665</td>
      <td>4.854837</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>315</th>
      <td>Vinton County</td>
      <td>Ohio</td>
      <td>0.459846</td>
      <td>0.259599</td>
      <td>4.456021</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>2560</th>
      <td>Iron County</td>
      <td>Missouri</td>
      <td>0.425657</td>
      <td>0.227228</td>
      <td>4.443903</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>2882</th>
      <td>Lee County</td>
      <td>Iowa</td>
      <td>0.581437</td>
      <td>0.414187</td>
      <td>4.399783</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>1937</th>
      <td>Monroe County</td>
      <td>Ohio</td>
      <td>0.459294</td>
      <td>0.256223</td>
      <td>4.358251</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>1080</th>
      <td>Harrison County</td>
      <td>Ohio</td>
      <td>0.423803</td>
      <td>0.248803</td>
      <td>4.301950</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>3012</th>
      <td>Pike County</td>
      <td>Ohio</td>
      <td>0.497959</td>
      <td>0.309845</td>
      <td>4.243976</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>52</th>
      <td>Guernsey County</td>
      <td>Ohio</td>
      <td>0.450583</td>
      <td>0.277411</td>
      <td>4.211854</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>2321</th>
      <td>Appanoose County</td>
      <td>Iowa</td>
      <td>0.482011</td>
      <td>0.310233</td>
      <td>4.145644</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>2478</th>
      <td>Reynolds County</td>
      <td>Missouri</td>
      <td>0.374757</td>
      <td>0.183549</td>
      <td>4.143634</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>1087</th>
      <td>Scioto County</td>
      <td>Ohio</td>
      <td>0.489968</td>
      <td>0.309310</td>
      <td>4.123989</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>533</th>
      <td>Henderson County</td>
      <td>Illinois</td>
      <td>0.562447</td>
      <td>0.348366</td>
      <td>4.090308</td>
      <td>0.002</td>
      <td>3</td>
    </tr>
    <tr>
      <th>1912</th>
      <td>Worth County</td>
      <td>Iowa</td>
      <td>0.573421</td>
      <td>0.384014</td>
      <td>4.032455</td>
      <td>0.002</td>
      <td>3</td>
    </tr>
    <tr>
      <th>2976</th>
      <td>Adams County</td>
      <td>Iowa</td>
      <td>0.479962</td>
      <td>0.288560</td>
      <td>3.943708</td>
      <td>0.002</td>
      <td>3</td>
    </tr>
    <tr>
      <th>2926</th>
      <td>Noble County</td>
      <td>Ohio</td>
      <td>0.373118</td>
      <td>0.212868</td>
      <td>3.918989</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>1136</th>
      <td>Monroe County</td>
      <td>Iowa</td>
      <td>0.460944</td>
      <td>0.285521</td>
      <td>3.801247</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>122</th>
      <td>Morgan County</td>
      <td>Ohio</td>
      <td>0.469194</td>
      <td>0.283936</td>
      <td>3.775632</td>
      <td>0.003</td>
      <td>3</td>
    </tr>
    <tr>
      <th>1785</th>
      <td>Union County</td>
      <td>Iowa</td>
      <td>0.519643</td>
      <td>0.352876</td>
      <td>3.721560</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>752</th>
      <td>Washington County</td>
      <td>Missouri</td>
      <td>0.404518</td>
      <td>0.214644</td>
      <td>3.665785</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>2113</th>
      <td>Calhoun County</td>
      <td>West Virginia</td>
      <td>0.383437</td>
      <td>0.183945</td>
      <td>3.654698</td>
      <td>0.002</td>
      <td>3</td>
    </tr>
    <tr>
      <th>431</th>
      <td>Davis County</td>
      <td>Iowa</td>
      <td>0.415776</td>
      <td>0.263998</td>
      <td>3.581305</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>1753</th>
      <td>Chickasaw County</td>
      <td>Iowa</td>
      <td>0.556043</td>
      <td>0.377145</td>
      <td>3.534017</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>2551</th>
      <td>Hancock County</td>
      <td>Illinois</td>
      <td>0.402929</td>
      <td>0.233874</td>
      <td>3.505805</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
    <tr>
      <th>2870</th>
      <td>Ringgold County</td>
      <td>Iowa</td>
      <td>0.463683</td>
      <td>0.292654</td>
      <td>3.498472</td>
      <td>0.001</td>
      <td>3</td>
    </tr>
  </tbody>
</table>
</div>



### Quadrant II


```python
(lmos.query('mlocal_p_1216 < .01 and mlocal_quad_1216 == 2')
   .sort_values(['mlocal_1216'], ascending=False)
   .head(25))
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>county</th>
      <th>STATE_NAME</th>
      <th>tpv_2012</th>
      <th>tpv_2016</th>
      <th>mlocal_1216</th>
      <th>mlocal_p_1216</th>
      <th>mlocal_quad_1216</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>1425</th>
      <td>Linn County</td>
      <td>Oregon</td>
      <td>0.411225</td>
      <td>0.348152</td>
      <td>-0.072804</td>
      <td>0.008</td>
      <td>2</td>
    </tr>
    <tr>
      <th>2163</th>
      <td>Lake County</td>
      <td>Colorado</td>
      <td>0.624395</td>
      <td>0.558659</td>
      <td>-0.150249</td>
      <td>0.002</td>
      <td>2</td>
    </tr>
    <tr>
      <th>58</th>
      <td>Carbon County</td>
      <td>Utah</td>
      <td>0.313157</td>
      <td>0.248662</td>
      <td>-0.159067</td>
      <td>0.001</td>
      <td>2</td>
    </tr>
    <tr>
      <th>665</th>
      <td>Lake County</td>
      <td>California</td>
      <td>0.585876</td>
      <td>0.520347</td>
      <td>-0.175529</td>
      <td>0.001</td>
      <td>2</td>
    </tr>
  </tbody>
</table>
</div>



There aren't many observations in this quadrant. This indicates quadrants whose swing was more Republican than average while their neighbors' swings were more *Democrat* than average. Note that this relates to the *mean national swing* as the unweighted average swing at the county level. So, this captues the counties who swung *more* Republican than average over counties **while their neighbors swung more Democrat** than average.

We see some pretty counter-iintuitive counties here. Linn county, OR and Lake county, CO weren't things that were on my radar, but it seems they've moved anomalously towards the Republicans while their neighbors intensified in Democratic support. 

### Quadrant IV


```python
(lmos.query('mlocal_p_1216 < .01 and mlocal_quad_1216 == 4')
   .sort_values(['mlocal_1216'], ascending=False)
   .head(25))
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>county</th>
      <th>STATE_NAME</th>
      <th>tpv_2012</th>
      <th>tpv_2016</th>
      <th>mlocal_1216</th>
      <th>mlocal_p_1216</th>
      <th>mlocal_quad_1216</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>1598</th>
      <td>Nicollet County</td>
      <td>Minnesota</td>
      <td>0.540244</td>
      <td>0.483152</td>
      <td>-0.041772</td>
      <td>0.006</td>
      <td>4</td>
    </tr>
    <tr>
      <th>3059</th>
      <td>Ohio County</td>
      <td>West Virginia</td>
      <td>0.385058</td>
      <td>0.329845</td>
      <td>-0.107641</td>
      <td>0.002</td>
      <td>4</td>
    </tr>
    <tr>
      <th>1240</th>
      <td>McDonough County</td>
      <td>Illinois</td>
      <td>0.492355</td>
      <td>0.437391</td>
      <td>-0.153496</td>
      <td>0.001</td>
      <td>4</td>
    </tr>
    <tr>
      <th>1713</th>
      <td>Kanawha County</td>
      <td>West Virginia</td>
      <td>0.439592</td>
      <td>0.391678</td>
      <td>-0.188802</td>
      <td>0.009</td>
      <td>4</td>
    </tr>
    <tr>
      <th>2443</th>
      <td>Cass County</td>
      <td>North Dakota</td>
      <td>0.484879</td>
      <td>0.440538</td>
      <td>-0.308489</td>
      <td>0.002</td>
      <td>4</td>
    </tr>
    <tr>
      <th>2153</th>
      <td>Linn County</td>
      <td>Iowa</td>
      <td>0.589217</td>
      <td>0.548273</td>
      <td>-0.359697</td>
      <td>0.009</td>
      <td>4</td>
    </tr>
    <tr>
      <th>2541</th>
      <td>La Crosse County</td>
      <td>Wisconsin</td>
      <td>0.587588</td>
      <td>0.551186</td>
      <td>-0.428908</td>
      <td>0.008</td>
      <td>4</td>
    </tr>
    <tr>
      <th>1230</th>
      <td>Jackson County</td>
      <td>Illinois</td>
      <td>0.553239</td>
      <td>0.516216</td>
      <td>-0.509392</td>
      <td>0.002</td>
      <td>4</td>
    </tr>
    <tr>
      <th>172</th>
      <td>Eau Claire County</td>
      <td>Wisconsin</td>
      <td>0.568727</td>
      <td>0.539251</td>
      <td>-0.680042</td>
      <td>0.001</td>
      <td>4</td>
    </tr>
    <tr>
      <th>2747</th>
      <td>Monongalia County</td>
      <td>West Virginia</td>
      <td>0.450422</td>
      <td>0.443524</td>
      <td>-0.984685</td>
      <td>0.008</td>
      <td>4</td>
    </tr>
    <tr>
      <th>2968</th>
      <td>Olmsted County</td>
      <td>Minnesota</td>
      <td>0.516450</td>
      <td>0.504157</td>
      <td>-1.047390</td>
      <td>0.001</td>
      <td>4</td>
    </tr>
  </tbody>
</table>
</div>



Like the second quadrant, these are also outliers. However, these are areas whose swings are more Democrat than average while their neighbors had swings that are more Republican than average. This seems to pick up a couple of Democratic areas where the general trend towards Trump failed to spill over between counties *as strongly* as it would have otherwise. 

So, for example, Nicollet county, MN swung towards Trump at slightly more than the unweighted average shift towards Trump at the county level. But, this indicates its neighbors swung *even more strongly*. 

To match this up, a map of the swing and the cluster statistics might be helpful. First, the swing map:


```python
f,ax = plt.subplots(1,1,figsize=(1.6*8,8))
votes['swing_1216'] = votes.tpv_2016 - votes.tpv_2012
votes.plot('swing_1216', cmap='RdBu', ax=ax, linewidth=.1, alpha=.8)
plt.title('Swing in Two-Party vote in 2016', fontsize=20)
plt.show()
```


![png](images/spacf_output_48_0.png)


And then the cluster map:


```python
votes['stat_quad'] = votes.mlocal_quad_1216 * (votes.mlocal_p_1216<.01)
```


```python
cp = sns.crayon_palette(['White', 'Cerulean', 'Tropical Rain Forest', 
                                 'Scarlet', 'Vivid Violet'])
import matplotlib.colors as cmaps
mymap, _ = cmaps.from_levels_and_colors(np.arange(-.5,5.5, 1), cp)
```


```python
f,ax = plt.subplots(1,1,figsize=(1.6*8,8))
votes.plot('stat_quad', linewidth=.1, cmap=mymap, ax=ax, alpha=.7)
ax.set_xticklabels([])
ax.set_yticklabels([])
plt.title('Local Indicators of Partisan Swing', fontsize=20)
plt.show()
sns.palplot(cp, size=1.5)
plt.show()
print('NSD from Mean   Dem Cluster   Rep Outlier   Rep Cluster   Dem Outlier')
```


![png](images/spacf_output_52_0.png)



![png](images/spacf_output_52_1.png)


    NSD from Mean   Dem Cluster   Rep Outlier   Rep Cluster   Dem Outlier


More specifically for these labels:

1. `NSD from Mean`: swing is about the national average in both the neighboring counties and the focal county. 
2. `Dem Cluster`: Swing is better for Democrats in both the focal and the neighboring counties than it is nationally. *(Dems overperform in this spatial cluster)*
3. `Rep Outlier`: Swing is better for Republicans in this county than it is nationally **and** better for Democrats in neighboring counties than it is nationally *(Reps overperform in this county vs. nearby counties and the nation)*
4. `Rep Cluster`: Swing is better for Republicans in this county and the neighboring counties than it is nationally *(Reps overperform in this spatial cluster)*
5. `Dem Outlier`: Swing is better for Democrats in this county than nationally, but better for Republicans in neighboring counties than nationally *(Dems overperform in this county vs. nearby counties and the nation)*

# Gauging typical Cluster size

With this, we can try to identify the "range" at which counties are related to one another. If we can identify this, we might be able to tell the graph distance at which counties tend to be come uncorrelated with one another. 

To compute this, we can use the (partial) spatial autocorrelation functions to identify this. In a similar manner to the (partial) temporal autocorrelation function, the (partial) spatial autocorrelation function relates each observation to its \(k\)th order neighbors. In the spatial context, the \(k\)th order neighbors of observation \(y\_i\) is the set of observations \(y\_j\) that are *first* reached in \(k\) steps. This means that the graph distance between observation \(y\_j\) and \(y\_i\) is exactly \(k\):
\\[ \{y\_{ik}  : min(||y\_j - y\_i||) = k ~ ~ ~ ~ ~ ~ \forall j= 1, 2, \dots, n\}\\]
Thus, the \(k\)th order spatial autocorrelation function is:

\\[\rho\_k = cor(y, \mathbf{W}^ky)\\]

where \(\mathbf{W}^k\) is the adjacency matrix for \(k\)-minimal neighbors. The \(k\)th-order *partial* spatial autocorrelation function is:

\\[ \dot{\rho}\_k = cor(y, \mathbf{W}^ky ~|~ \mathbf{W}^{k-1}y, \mathbf{W}^{k-2}y, \dots, \mathbf{W}^{1}y )\\]

I plot these for the vote shares in 2016 below, 


```python
import spacf
```

First, the full spatial autocorrelation plot:


```python
lags = spacf.spacf(votes[['tpv_2016']].values - votes[['tpv_2012']].values,W, order=20)
plt.figure(figsize=(1.6*4*2,4))
plt.plot(lags, linewidth=5, color='slategrey')
plt.title('Spatial ACF', fontsize=20)
plt.hlines(0,0,len(lags), linestyle=':', color='k')
plt.axis([0,25,-.2,1])
plt.show()
```


![png](images/spacf_output_58_0.png)


Interpreting this, we have to move around 16 counties out before the autocorrelation between counties becomes negative. Remember, this statistic considers only \(k\)-minimal neighbors, not all observations below \(k\)th order neighbors. If you don't consider \(k\)-minimal neighbors (rather than \(k\)th order), then sets of higher-order neighbors will contain the set of lower-order neighbors. 

In a time series context, this would be akin to considering both the observation from 2 periods ago *and* the previous observation in the set of 2nd order neighbors. In contrast, this graph shows the correlation as the set of "considered" counties radiates uniformly outwards from each focal county. 

Thus, a typical ``cluster'' in the sense of counties being more related to each other than not, is a subgraph somewhere south of 15-counties in radius. If this seems too big to you, you're right. We need to account for the *whole* neighborhood contained with the \(k\)-radius cluster:

### Conditional width of cluster size

The partial correlation plot *does* condition on the neighbors below \(k\)-th order. So, the correlation between the \(k\)-minimal neighbors and the source observations is conditional on 1st through \(k-1\)-minimal neighbors. We can use this plot to adequately identify the ``order'' of the spatial process, if we treat it as a spatial markov random field.


```python
plags = spacf.sppacf(votes[['tpv_2016']].values -votes[['tpv_2012']].values , W, order=10)
plt.figure(figsize=(1.6*4,4))
plt.plot(plags, linewidth=5, color='slategrey')
plt.title('Partial Spatial ACF')
plt.hlines(0,0,len(plags), linestyle=':', color='k')
plt.show()
```


![png](images/spacf_output_62_0.png)


Interpreting this, we see that the first order neighbors are typically sufficient to capture the full extent of correlation between counties. Conditional on the first-order neighbors, higher-order neighbor correlation pretty-much disappears.. 
