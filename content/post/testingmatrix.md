---
title: "Using lookahead testing"
date: 2018-07-24T13:52:51-07:00
---

To help subpackages that depend on `libpysal`, the API will change shortly to be in line with our desired `migrating.pysal.org` API.

If you test your package against the pypi version of libpysal (which you get using `pip install libpysal`), you already know that your changes don't break with respect to what exists.
However, if you'd like to give yourself some lead time to detect if there are breaking changes in the development version of `libpysal` on github, feel free to follow these directions on how to set up *optional* tests on travis. These tests are run alongside the rest of your tests, but they're allowed to fail without marking your build as *failing* in total.
I call these tests *lookahead* tests, since they're "peeking" at the next release of the dependency.  

### the `env` environment
In your `.travis.yml` file, which governs the tests that are run on travis-ci.org, the [`env`](https://docs.travis-ci.com/user/environment-variables/#Defining-public-variables-in-.travis.yml) section is used to define *enironment variables*, which you can think of like options that describe how travis-ci has configured your build. In the case of [libpysal](https://github.com/pysal/libpysal/blob/master/.travis.yml#L11), we have traditionally defined two sets of tests: one against our bare-bones dependencies (only scipy,numpy, and now pandas), and one against our "plus" environment, which includes geopandas, shapely, numba, matplotlib, and a few others. We do this to make sure our plus-enabled code (such as the numba map classifiers in `mapclassify`) always have a usable fallback if the user does not have the required optional dependency.

In this case, we can use the same strategy to implement a testing *matrix*, which defines all of the combinations of environment variables we might want to use in our test. If you'd like to make sure that you're testing against the github versions (but are ok with these failing), then you can slightly modify your `.travis.yml` file.

#### defining the testing matrix
Travis testing matrices are somewhat verbose by default. While small and simple combinations of environments are built by default, it's simpler to state explicitly the combinations in environments you want. So, below, I'll show you how to set up an environment for the case of one additional environment variable `PYSAL_PLUS`, and one versioning variable: `PYSAL_PYPI`:

In the `env:` block, we define all of the distinct combinations of environment variables we want to run:
```
env: 
    - PYSAL_PYPI=true PYSAL_PLUS=true
    - PYSAL_PYPI=true PYSAL_PLUS=false
    - PYSAL_PYPI=false PYSAL_PLUS=true
    - PYSAL_PYPI=false PYSAL_PLUS=false
```
then, in the `matrix:` block, we explicitly flag the exact configurations we want to allow to fail. These match against *all* of the previous configuration options above, including python versions:
```
matrix:
  allow_failures:
      - python: 3.5
        env: PYSAL_PYPI=false PYSAL_PLUS=false
      - python: 3.5
        env: PYSAL_PYPI=false PYSAL_PLUS=true
      - python: 3.6
        env: PYSAL_PYPI=false PYSAL_PLUS=false
      - python: 3.6
        env: PYSAL_PYPI=false PYSAL_PLUS=true
```
With these two sections, we have defined eight unique tests; two versions of Python (3.5 and 3.6), and the combinations of `PYSAL_PLUS` and `PYSAL_PYPI`. Note also a few pitfalls:

1. make sure there is no space between the equals sign and the value you're setting the env to
2. make sure that your repeat `env` every time you have an entry in `allow_failures`, so travis can attempt to match your previously-defined setup patterns. 
3. don't forget to include the python versions you're testing in the first line of the `allow_failures` entries.

Great; we've defined a testing matrix where we can check 8 combinations of tests. However, by default, these will only queue builds with these configurations; if nothing in our build script takes these variables into account, we won't do anything different when we run our tests. So, to make this useful, we *then* have to use these environment variables in our `.travis.yml`'s `before_install` or `install` sections. I'm building this example for subpackages (and have worked this out for [`spreg`](https://github.com/ljwolf/spreg/blob/master/.travis.yml#L9) and [`esda`](https://github.com/ljwolf/esda/blob/master/.travis.yml#L10)), so this'll be focused on allowing `libpysal` to get installed from `pypi` versus from `github`. 

To change your `before_install` section depending on these environment variable settings, we need to use [bash conditionals](https://github.com/ljwolf/esda/blob/master/.travis.yml#L39). These are just like Python conditionals (`if/else` statements), but they're a bit less easy to make *just work*. So, I'd recommend you follow this pattern *exactly* in your `.travis.yml` for a variable named `PYSAL_PYPI` which has been set to either `true` or `false`:

```
- if "$PYSAL_PYPI"; then
    echo "do option PYSAL_PYPI"; do second command;
    else echo "do not do option PYSAL_PYPI"; do more commands; 
  fi;
```

Formatting in bash can be fairly tricky, so try to stick closely to how this is formatted. Notably, the conditional `if "$PYSAL_PYPI"` should only be used if setting variables to `true` or `false`. Finally, make sure that you always return to your original location if you intend to use `cd`; there is no automatic tracking of the base directory like in a `make` command. 

So, for instance, consider the following snippit from the proposed `esda` build configuration. This uses the same build structure above, testing all python versions in a "rich" environment with `numba` and a "bare" environment without `numba`, and either grabs `libpysal` from PyPI or grabs its current `master` branch on github:

```
  - if "$PYSAL_PYPI"; then
        echo 'testing pypi libpysal' && pip install libpysal;
        else echo 'testing git libpysal'; git clone https://github.com/pysal/libpysal.git; cd libpysal; pip install .; cd ../;
    fi;
  - conda install --yes --file requirements.txt;
  - if "$PYSAL_PLUS"; then conda install --yes numba; fi
```

The first conditional, when `$PYSAL_PYPI` is `true`, will use `pip install libpysal` to install `libpysal` from PyPI. When it is `false`, however, the line starting with `else` will execute. There, I've chained a few commands together. I always use that `echo` command to ensure that I can go back through the travis logs and make sure which branch of the conditional has executed. Then, after the `echo`, I run many commands all on the same line. There is no line length limit, and using line continuation characters can get misread by the way travis parses the `.travis.yml`. This is just me being risk-averse, though, so if you figure out a nicer way to do this, feel free to do so. In this section following the `else`, I clone `libpysal` from github, `cd` into my new clone, install this cloned version using `pip`, and then change back to my original directory. Together, this gives me the version of `libpysal` that exists directly in the github `master` branch. 
The second conditional checks if `$PYSAL_PLUS` is set to `true`. If it is, we also want to enrich our testing environment with `numba`, so we can check the `numba`-accelerated computations. 


### why do this at all?

For a few reasons, but mainly as a hedge against change: 
1. Keep your submodule up-to-date with possible breaking changes in your dependencies, especially those dependencies you use frequently.
2. Ensure your library wont break if another maintainer releases a new version that behaves slightly differently from what you expect or what you recall. 
3. Check if your fixes to an unpublished but under-development API change in a dependency work. 

### The beauty of social solutions

Overall, though, tests (and ci, and github itself) is nothing without social norms driving this process. 
Using this lookahead testing, you can even decide to merge PRs that *fail* the pypi-facing tests but pass the *lookahead*, since the*lookahead* is the future version of the package your package will need to accommodate.  
And, finally, this is only intended to give you peace of mind; if you feel this is too complicated to implement or maintain, just run your tests manually and don't get too surprised if breakage occurs.
