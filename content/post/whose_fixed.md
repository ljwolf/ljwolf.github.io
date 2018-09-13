---
title: "Who's Fixed?"
date: 2018-09-13T15:25:50+01:00
---
When comparing a multilevel model to a fixed-level model, it's important to consider how things are parameterized. For instance, let's say you're conducting comparisons between a no-pooling model and a partial pooling variance components model. In this case, we have:

$$ y= \Delta u + \epsilon$$

as the specification, where $\Delta$ is the dummy variable matrix, `$y$` is the outcome of interest, and `$\epsilon$` is the usual homoeskedastic error term for the responses. Note that the no pooling model contains no intercept and instead contains `$J$` means, one for each of the $j$ groups. 

But, let's consider what happens when we look at the multilevel estimate for an analogous model:

$$ y = \Delta \theta + \epsilon$$

$$\theta = \mu + \zeta$$

where *now*, $\zeta$ is a zero-mean error term for our *region-level model* of `$\theta$`. Our $\theta$ term expresses the unique effects of each observation with respect to a *higher-level mean*, `$\mu$`, with its own estimating uncertainty. 

So, what is the correct comparison between terms in these models?  Well Gelman has his whole pooled/not pooled dichotomy, and I think that makes sense. Most of the multilevel literature compares `$\zeta$` to `$u$`, since `$\zeta$` reflects the fact that we have "separated" the uncertainty in estimating the global mean out, placed it in `$\mu$` and now are only concerned with estimating the noise in the distinct contribution added by *being in a group*. Thus, again, for centered `$y$`, `$u$` sould look like `$\zeta$`, but with slightly wider intervals and slightly more extreme point estimates, and `$\mu$` should be around zero. This argument is quite persuasive, and grounds the interpretation of these models in the literature. 

But, the "direct" comparison involves comparing $\theta$ to `$\gamma$`, which both serve the same direct function in the mean predictor of the model. Practically speaking, if we think this is the correct apples-to-apples comparison, then *both* the uncertainty in `$\alpha_0$` and `$\zeta$` are included in comparisons to the fixed effect model's unique region estimate. An example of how this might be done is:

1. run a sampler for the parameters
2. at convergence, resampling `$\mu$` and `$\zeta$` from their posteriors and obtain one draw of $\theta$.

I showed this in [my recent presentation at the Royal Statistical Society conference](https://docs.google.com/presentation/d/1fSsFpfmgePGTISXydI5pvLx3x4FlHLe8DoYmeyskbGY/edit#slide=id.p). The uncertainty in estimating the state-specific effect $\zeta$ [obeys the usual behavior](https://docs.google.com/presentation/d/1fSsFpfmgePGTISXydI5pvLx3x4FlHLe8DoYmeyskbGY/edit#slide=id.g4005b47210_0_23). But, if we actually simulate samples of $\theta$ from the posterior, we get [estimate bands that look much more similar to the original bands](https://docs.google.com/presentation/d/1fSsFpfmgePGTISXydI5pvLx3x4FlHLe8DoYmeyskbGY/edit#slide=id.g40c00010b8_0_96).

I'm sympathetic to the arguments that you shouldn't lump the uncertainty in $\mu$ into the uncertainty around `$\zeta$`, but I also think that, from the fixed-effects user perspective, the "right" intuitive comparison is between `$\theta$` and `$u$`.  Not sure what this means for applied work; personally, I nearly always want to make the Gelman-style comparison, but I'm wondering if there are cases where it's more useful to think critically about what uncertainty around `$\mu$` means in the multilevel specificaation. For my spatial statistical context, end of the day, our predictions for any specific data point *will* involve both the uncertainty in `$\mu$` and `$\zeta$` for any observation, so while we can separate them out in the regression model, we cannot really in the discussion of prediction.

