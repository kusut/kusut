---
tags: python, pyramid
title: Pyramid Traversal and MongoDB
date: 2011-03-27
---

***Update***: **This app uses new request subscriber to add mongo connection. The new blessed way to do this is by using `add_request_method <http://docs.pylonsproject.org/projects/pyramid/en/latest/narr/hooks.html#adding-methods-or-properties-to-request-object>`_. The code is also available on `github <https://github.com/kusut/troll>`_.**

This post will talk about creating a simple web application with `Pyramid <http://pylonsproject.org>`_ and `MongoDB <http://mongodb.org>`_. It will also talk about using a traversal method to map a requested URL to a callable function (controller/method/view/etc) in your application code.

The App
_______
Enter Troll, an anonymous board web application. It is a simple application that is similar to a blog app (post and comment). The features are:

* Sorted by activity (recently commented posts come first).
* Automatic removal for the least commented post (there is  a maximum number of posts).
* Lord Inglip summoning pedestal (read: recaptcha).

The non-features:

* No pagination. Maybe later with webhelper. Post limit is set to 10 by default anyway.
* No test.
* etc

The app uses a single mongodb collection for posts. The comments are embedded to a post document.

Source code is available `here <https://bitbucket.org/kusut/troll/src>`_.

The Stack
_________
This app uses few technologies

* `Mako <http://makotemplates.org>`_ - a templating engine
* `WTForms <http://wtforms.simplecodes.com/>`_ - form generation and validation, similar to django (new)forms
* `wtforms-recaptcha <http://pypi.python.org/pypi/wtforms-recaptcha>`_ - Inglip
* `MongoDB <http://mongodb.org>`_ (`pymongo <http://api.mongodb.org/python/>`_) - data persistence



Traversal
_________

**NOTE** : Traversal is optional in Pyramid. You can still use URL dispatch (pattern matching thing).

Traversal is a method of matching a requested URL to your application code just like a more familiar method, URL parsing and comparing it to a set of patterns. Traversal requires you to build a resource `tree <http://en.wikipedia.org/wiki/Tree_%28data_structure%29>`_ which is probably analogous to a file-system hierarchy. Pyramid will take a URL, and then traverse your resource tree trying to find a resource for that URL. Once a resource is found, Pyramid will try to find a function associated with that resource. The resource object found (or last traversed) is called context.

`This post <http://www.serverzen.net/2010/11/8/getting-started-with-pyramid-a-notes-application>`_ gave me a good idea about traversal. 

For this application, two kinds of resource object are needed. The first one is the root object. This object will act as a container to post objects.

.. code:: python

   class Root(object):
       __name__ = None
       __parent__ = None

       def __init__(self, request):
           self.collection = request.db.post

       def __getitem__(self, name):
           post = Post(self.collection.find_one(dict(_id=ObjectId(name))))
           return _assign(post, name, self)

       def __len__(self):
           return self.collection.count()

       def __iter__(self):
           return (
               _assign(Post(x), str(x['_id']), self)
               for x in self.collection.find().sort('updated', DESCENDING)
           )



The __getitem__ method will return post object (the child of the root object). The root object contains all posts. _assign is just a simple function to set some attributes of resource object.

.. code:: python

   def _assign(obj, name, parent):
       obj.__name__ = name
       obj.__parent__ = parent
       return obj


Next is the post object. This object is a slightly modified python dictionary (pymongo returns mongodb document as a python dict). Resource tree objects need to have a __parent__ attribute.

.. code:: python

    class Post(dict):
        def __init__(self, a_dict):
            super(Post, self).__init__(self)
            self.update(a_dict)
            self.__name__ = None
            self.__parent__ = None


The Views
_________
This is the function for viewing a resource object.

.. code:: python

    @view_config(renderer='single.html', context=resources.Post)
    @view_config(renderer='index.html', context=resources.Root)
    def view(context, request):
        form = TrollForm()
    	return {'p': context, 'form': form}

Pyramid allows you to write a function once and then register it multiple times for different contexts. Coming up is the function for handling post and comment addition.

.. code:: python

    @view_config(name='add', request_method='POST', context=resources.Post)
    @view_config(name='add', request_method='POST', context=resources.Root)
    def add(context, request):
        author = request.params['name']
        content = request.params['content']
        _add(context, author, content)
    	return HTTPFound(location=request.resource_url(context))

Here is the _add function.

.. code:: python

    def _add(context, author, content):
        if context.__parent__ is None:
            _post(context.collection, author, content)
        else:
            _comment(
                context.__parent__.collection,
                context['_id'],
                author,
                content,
            )

Finally, doing insert/upsert to MongoDB.

.. code:: python

    def _post(collection, author, content):
        p = dict(
            author=author,
            content=content,
            comments=[],
            updated=datetime.utcnow(),
            time=datetime.utcnow()
        )
        collection.insert(p)

        #remove unpopular post if >  10
        if collection.find().count() > 10:
            _id = [x for x in collection.find().sort("updated", DESCENDING)][-1]['_id']
            collection.remove({'_id': _id})


    def _comment(collection, post_id, author, comment):
        post = collection.find_one(dict(_id=post_id))
        time = datetime.utcnow()
        post['comments'].append(dict(author=author, comment=comment, time=time))
        post.update(dict(updated=time))
        collection.save(post)


Templating
__________
Post form and comment form have the same fields (author, content, and captcha) and use the same form class (from wtforms). To prevent typing the same thing in many places, I created a template macro.

.. code:: mako

    <%def name="createform(c, form)">
        <% link = request.resource_url(c)%>
     	<form method="POST" action="${link}@@add">
      	    <div> ${form.name.label}: ${form.name(size=50)}</div>
      	    <div> ${form.content.label}: ${form.content(rows=5, cols=50)}</div>
       	        ${form.captcha}
            <input type="submit" value="Submit!" />
        </form>
    </%def>


This macro takes a context and a form object, to generate an html form. URL for any resource can easily be retrieved via resource_url method on request object. The '@@' means the start of a view name. Pyramid will traverse the URL until '@@', and search for a view named 'add' for that context.

This is how to use it.

.. code:: html+mako

    ${createform(request.context, form)}


If you want to use it on another template file, import it first.

.. code:: html

    <%namespace file="base.html" import="createform" />

Conclusion
__________
Pyramid is a fun framework to tinker with.

`Source Code <https://bitbucket.org/kusut/troll/src>`_
