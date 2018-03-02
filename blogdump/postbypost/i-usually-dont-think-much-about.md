---
id: 121892041274
date: 2015-06-18T23:49:13Z
url: i-usually-dont-think-much-about
title: Untitled
---
<p>I usually don’t think much about
credentialization online. I have a few internet aliases (like
yetanothergeographer) that I consider almost totally synonymous with my
real name and persona. And even other personas on tumblr or other social
networking sites are easily traced back to my “full” identity.</p>

<p>But,
something that I found pretty shocking recently was the fact that
<strong>any</strong> github profile you can find shows your public ssh keys to
everyone. And I mean <a href="%5Bhttps://github.com/torvalds.keys%5D(https://github.com/torvalds.keys)" target="_blank"><strong>everyone</strong></a>.
By affixing <code>.keys</code> to the end of anyone’s github user profile (or
organization profile) you can check if the account has any associated
public ssh keys attached to it.</p>

<p>Now, this isn’t too scary if you’re consistent and diligent with your use of ssh keys. But, as Ben Cox <a href="%5Bhttps://blog.benjojo.co.uk/post/auditing-github-users-keys%5D(https://blog.benjojo.co.uk/post/auditing-github-users-keys)" target="_blank">points out</a>, many people aren’t.</p>

<p>Another
angle Ben didn’t really discuss was the potential for using publically
available ssh keys as a way to <em>deanonymize</em> people who use github under
a pseudonym. For instance, the <a href="%5Bhttps://tox.im/%5D(https://tox.im/)" target="_blank">Tox Project</a>
has had a relatively tumultuous private history, full of quite a bit of
vitriol and venom between disgruntled developers and cries of
conspiracy. For example, two developers (and I’m sure many more) of
Tox’s core group, <a href="%5Bhttps://github.com/irungentoo.keys%5D(https://github.com/irungentoo.keys)" target="_blank">irungentoo</a> and <a href="%5Bhttps://github.com/stqism.keys%5D(https://github.com/stqism.keys)" target="_blank">sqtism</a>, have public keys to their account.</p>

<p>This
means that anyone could look for matches between the key that they use
on their pseudonymous account and other named users. This would present a
problem if they, like many people I know, develop on a few computers,
pushing code to many different projects that <em>all use your public ssh
key as an indicator of identity</em>.</p>

<p>This isn’t really anything
surprising, as the <em>public</em> key is (surprise!) public. But, what I don’t
think people realize is that some more noxious trolls (like the
<a href="%5Bhttps://github.com/The-Feminist-Software-Foundation.keys%5D(https://github.com/The-Feminist-Software-Foundation.keys)" target="_blank">Feminist Software Foundation</a>) could possibly be deanonymized by checking their ssh keys against a candidate set like that built by Ben Cox.</p>

<p>To
be clear, <em>I am not doing this, nor do I advocate deanonymizing people
who want to be anonymous!</em> But, I do think it’s important for people to
realize that this is another one of a long line of digital fingerprints
people are leaving in ways they may not realize.</p>

<p>I guess the real
important part of the story comes from the fact that as information
becomes easier to search and scrape, it becomes easier to collate and
corroborate the disparate parts of your internet identity. This is just
one (admittedly very arcane) example of the destruction of
depersonalized/deidentified space on the internet.</p>

<p>Digital unnamed space is rapidly disappearing one ssh key at a time.  </p>
Tags: and this is about as, human geography, as I get these days, geography, gis, python, github, ssh, publicity, anonymity, pseudonymity, feminist software foundation, tox, identity