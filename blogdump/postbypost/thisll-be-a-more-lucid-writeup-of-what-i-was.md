---
id: 146080088369
date: 2016-06-17T18:27:53Z
url: thisll-be-a-more-lucid-writeup-of-what-i-was
title: 
tags: ["programming"," python"," bayesian statistics"," statistics"," geography", "imported", "image"]
---
<figure> <img src=//78.media.tumblr.com/98dc66ae957766a0eff84e65c5324df1/tumblr_o8xt2hUN5T1ts05oao1_1280.png> </figure>

<p>Recently, I’ve been working on getting spatial econometric models implemented using <a href="https://github.com/pymc-devs/pymc3" target="_blank">PyMC3</a>. I’ll put pu an example later, but right now I’m primarily concerned with making the example more efficient for slightly larger datasets.</p>

<p>You see, some spatial econometric models require that the log determinant of a very large matrix be computed. Since most of these models are estimated using Maximum Likelihood, this is somewhat painful, but can be minimized by exploiting sparsity in the large matrix. Most of the time, these large matrices are essentially approximations/estimates of something that looks a lot like a <a href="https://en.wikipedia.org/wiki/Laplacian_matrix#Random_walk_normalized_Laplacian" target="_blank">Random Walk Normalized Laplacian Matrix</a>, essentially a matrix describing centrality and spatial proximity between observations.</p>

<p>Now, to make these models fast, I did a lot of work characterizing the best way to do a sparse log determinant for our purposes, and I decided the best way was to use the SuperLU library in <code>scipy.sparse.linalg</code>. We’ve been shipping this as a method in our Maximum Likelihood econometric models for a <a href="https://github.com/pysal/pysal/blob/26c23b0bd86cfa1886bb5d722ab02142d902b090/pysal/spreg/ml_lag.py#L580" target="_blank">few months now</a>. Since adjacency matrices can sometimes be asymmetric, I picked this over <code>scikits.sparse.cholesky</code>, which has a much more straightforward <code>.logdet</code> method. I think this might have a ton of applicability elsewhere (I’m looking @ you, statsmodels), but I don’t know where I should send this type of code. It’d be neat to put it in as <code>scipy.sparse.linalg.logdet</code>, but I doubt that’d get merged.</p>

<p>I’ve recently figured out how to fit spatial econometric models in PyMC3 as well, and I’ll push up that implementation soon. But, I’m currently quite a bit dissatisfied with it because it’s using <em>dense log determinants</em> in factor potentials. So, I’m trying to get this all to work with the sparse stuff I’ve put together, and it’s not very straightforward.</p>

<p>First, I’d like to use the Hamiltonian Monte Carlo stuff in PyMC3. I can use the baked in log and determinant from Theano, and was excited to see <a href="https://github.com/Theano/Theano/issues/150" target="_blank">this op</a> get some movement, but it appears to have died. And, again, that implementation is dense. So, in my case, I need to define a Theano <code>Op</code> on a sparse matrix with a <code>grad</code> method. The grad of a log determinant (specifically, of a known-positive definite matrix), is very simple: it’s the transposed inverse.</p>

<p>So, I’ve got three versions of the <code>Op</code> I need, and they all yield correct sparse log determinants and, as far as I can tell, correct gradients. They’re profiled above. The first method just calls out to numpy’s <code>linalg.slogdet</code>, so it actually is a dense computation. The second is a hybrid, where a dense matrix is cast to a sparse matrix each iteration, then factored in sparse form, and the log determinant computed. This is actually quite efficient, even though the conversion costs each round are high. We <em>know</em> the conversion costs are high because the third method uses straightforward sparse matrix LU factorization and is an order of magnitude faster.</p>

<p>What seems to be tripping me up is the gradient. Unfortunately, the inverse of the input matrix is rarely sparse, so there are usually no gains in that aspect, so I always cast the matrix down to dense, compute the inverse using theano, and then push it back up into sparse if necessary.</p>

<p>But, I’m having an issue using the <code>Op</code> in a PyMC3 model when the gradient is sparse. I’m unsure if the error is in my implementation, if it’s something with PyMC3, or if it’s related to <a href="https://github.com/Theano/Theano/issues/2782" target="_blank">this bug</a>. I believe it’s the latter, since NUTS is invariably combining the gradient of my LogDet op with other gradients in the model, but I haven’t figured it out yet.</p>

<p>Regardless, the implementations of the <code>Op</code>s, focusing on the <code>perform</code> methods,</p>


<pre><code>

import scipy.sparse as spar
import scipy.sparse.linalg as spla
from theano import Op
import theano.tensor as tt
import theano.sparse as ts
import numpy as np



Class Dense_LogDet(Op):
def make_node(self, A):
A = tt.as_tensor(A)
ld = tt.scalar(dtype=A.dtype)
return Apply(self, [A], [ld])

#dlogdet: use np.linalg.slogdet on a dense matrix
def perform(self, node, inputs, outputs):
(A, ) = inputs
(z, ) = outputs
sgn, ld = np.linalg.slogdet(A)
if sgn not in (-1, 0, 1):
raise Exception('Significant loss of precision in log determinant')
ld *= sgn
z[0] = ld

def grad(self, inputs, outputs):
(gz, ) = outputs
(A, ) = inputs
dinv = tt.nlinalg.inv(A).T
dout = gz * dinv
return [dout]


class Sparse_LogDet(Op):
def make_node(self, A):
A = ts.as_sparse(A)
ld = tt.scalar(dtype=A.dtype)
return Apply(self, [A], [ld])

#spLUlogdet: use sparse LU decomp on a sparse input matrix
def perform(self, node, inputs, outputs):
(As, ) = inputs
(z, ) = outputs
Ud = spla.splu(As).U.diagonal()
ld = np.sum(np.log(np.abs(Ud)))
z[0] = ld

def grad(self, inputs, g_outputs):
(gz, ) = g_outputs
(A, ) = inputs
dA = ts.dense_from_sparse(A)
dinv = tt.nlinalg.matrix_inverse(dA).T
dout = gz * dinv
return [ts.csc_from_dense(dout)]
</code></pre>

<p>So, when I use this in a PyMC3 model, the model builds successfully, but when it comes time to compute the joint posterior gradient in <code>pymc3.NUTS()</code>, theano complains about shape errors:
<code>ValueError: MulSD.grad returned object of shape (N, N) as gradient term on input 1 of shape ()</code></p>

<p>Where N,N is the size of the matrix before the log determinant is computed. In theory, I think this means that there’s an issue with the gradient being a sparse matrix, but I can’t tell what’s going on yet.</p>

<p>This is a bummer, since, as I show above, the log determinant runs quite quickly. So, I’m stuck trying to figure out whether this is:</p>

<ol><li>An error in how I’m doing the gradient of the sparse log determinant</li>
<li>An error in how PyMC3 compares gradients of heterogeneous collections</li>
<li>An error in how Theano computes the gradient of a compound heterogeneous collection</li>
</ol><p>But, the main point is that the <code>Op</code>, as far as I can tell, has a working <code>perform</code> and <code>grad</code>, and I’m pretty close to having this available to do efficient sparse log determinants while using PyMC3 on spatial econometric models.</p>



<small><i> imported from:</i> <a href='https://yetanothergeographer.tumblr.com/146080088369/thisll-be-a-more-lucid-writeup-of-what-i-was'<tt>yetanothergeographer</tt></a></small>