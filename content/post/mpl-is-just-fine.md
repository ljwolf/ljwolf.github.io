---
title: "Mpl Is Just Fine"
date: 2018-03-18T22:02:05Z
---

I've been using matplotlib for nearly 5 years at least once a week.
I'm still learning things that exist within the `pylab` interface... not the most ideal UX.
For instance, I just learned about `plt.axvline`, which I could use
to draw vertical lines in my code instead of what I usually use, `plt.vlines(coordinate_list, *plt.gca().get_ylim())`,
but it's not as general as `plt.vlines` since it actually plots a rectangle. 
Still, though, for most of what I do (which is a single vertical line for drawing specific axes/time breaks in a plot) it's easier. 

The fact that there's also no `abline` still gets to me, too, when I step back from it. 

I wonder if you could make it "better" by making a better API that focuses on the UX, 
adding a bit more semanic structure/nesting to the `pyplot` api that users could understand. 

CLI UX is really hard, especially for cases where you can't pipe, but you can nest.

