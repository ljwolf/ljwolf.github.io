---
id: 120805077929
date: 2015-06-05T18:01:54Z
url: cenpy-an-exploratory-interface-to-the-census
title: Untitled
---
<h1>CenPy - An exploratory interface to the Census API</h1>

<p>So, I was looking to do some programmatic access to US Census Bureau data and
was finding myself a little frustrated with how limited current tools in Python
were to work with the connections. Mainly, most supported only a few protocols,
provided no documentation or exploration functionality, and left it primarily
up to exeternal documentation to provide information on how to query the
API. Now, I know the day of National Civic Hacking is <a href="http://links.govdelivery.com/track?type=click&amp;enid=ZWFzPTEmbWFpbGluZ2lkPTIwMTUwNTI3LjQ1MzI3MjYxJm1lc3NhZ2VpZD1NREItUFJELUJVTC0yMDE1MDUyNy40NTMyNzI2MSZkYXRhYmFzZWlkPTEwMDEmc2VyaWFsPTE3MDA5OTYwJmVtYWlsaWQ9ZGZvbGNoQGZzdS5lZHUmdXNlcmlkPWRmb2xjaEBmc3UuZWR1JmZsPSZleHRyYT1NdWx0aXZhcmlhdGVJZD0mJiY=&amp;&amp;&amp;109&amp;&amp;&amp;http://hackforchange.org/?eml=gd&amp;utm_medium=email&amp;utm_source=govdelivery" target="_blank">tomorrow</a>, but I figured I&rsquo;d get a jumpstart&hellip;</p>

<p>So, I designed a small package to interface with <em>any</em> Census API that
follows the same <code>get</code>, <code>for</code>, <code>in</code> key specifications. The tool,
<a href="https://github.com/ljwolf/cenpy.git" target="_blank">cenpy</a> provides a <code>Connection</code> object,
which pulls down the metadata needed to explore the dataset. Connection objects
have a <code>query</code> method that is easily to use and outputs directly to a pandas
dataframe. The connection object transparently records what queries are executed
and is easy to extend. Plus, if new connections are announced that follow the
specifications provided by the census currently, the package will discover and
provide those connections too!</p>

<p>Right now, I haven&rsquo;t packged it up for pip yet. But, adding it to your
pythonpath should work just fine. Alternatively, check out the
<a href="https://github.com/ljwolf/cenpy/blob/master/demo.ipynb" target="_blank">demo</a> and see its
functionality. Honestly, I&rsquo;m super excited about the number of datasources that
this makes easily accessible!</p>
Tags: python, census, US Census Bureau, civic hacking, API, geography