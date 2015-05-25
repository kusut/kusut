---
tags: python, blogofile
title: Hello World
date: 2011-03-12
---

**Update** : I chose to use fabric to manage this blog. Only compiled html files are deployed (build locally).
**Update** : I switched to haskell.

I finally decided to stop using wordpress and do static blogging instead. I chose `Blogofile <http://blogofile.com>`_ as my new blog engine because it's python, pretty active and has good docs.

My Changes
----------
These are what I learned. 

* I want to write my blog in rst format. I am not familiar with it so I am going to use my blog as a learning tool. The problem is, currently, rst does not play nice with code highlighting. Luckily, there is already `a great solution <http://techspot.zzzeek.org/2010/12/06/my-blogofile-hacks>`_  for that.

* I feel pygments' default style, 'murphy', does not look right in my theme so I changed the default style to 'friendly' by putting this on syntax_highlighter.py

  .. code:: python

     config = {'style': 'friendly'}


* Blogofile assumes that your disqus username is similar to your site name (sitename.disqus.com). You might want to store them in a different variables if they are different, and use them accordingly on template files.

* Be sure sure to load your css files made by pygments

  .. code:: html+mako

     % for css_file in bf.config.filters.syntax_highlight.mod.css_files_written:
         <link rel='stylesheet' href='${css_file}' type='text/css' />
     % endfor


That is probably it. I also learned about mako a bit. So, all set? Not quite. There is one thing left.

Deployment Method
-----------------
Using a static blogging engine (web compiler) needs you to figure out how you want to update your blog on live server. It is a lot of work compared to usual dynamic blogging but with the right tools it can be handled. The plus is it is easy moving/updating sites once you are set.

I am still undecided on this issue. Here are my concerns:

* Do I include compiled files in version control? or just the source?
* What is the favorable method for me to bring the site live? mercurial hook or fabric and scp/rsync?

I tried mercurial hooks and it went well. I also installed python 2.7.1 and blogofile on the server, so the blog can be built there. I have a feeling that I will not decide on this matter for a long time and continue to use only ssh and scp. Nevertheless, I am going to put this on revision control even if I do not end up using its hooks for deployment.


