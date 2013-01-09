SOLR support for Zotonic
========================

This module adds support for Solr to Zotonic. It comes with Solr built
in: just install this module and off you go.

Once set up, Zotonic will push each resource it saves into Solr as
wel, so it can be queried on. To reindex the whole site into Solr,
press the 'rebuild search indexes' button in the admin.


Installation
------------

You need to manually add a 'core' to the embedded Solr instance for
each site you want to enable Solr support for. This is done in the
priv/cores/solr.xml file inside the module: copy the solr.xml.in file
to solr.xml and add as many cores as you have sites, naming them the
same way.

Regular query syntax
--------------------

Most of the query options are similar to the query-search syntax:
http://zotonic.com/docs/0.9/manuals/datamodel/search.html,
but you can do more nice things with Solr.

You can refactor your existing queries (from m.search[{query ....}]):

    {% with m.search[{solr text="collage" cat="media" }] as r %}
        {% print r %}
    {% endwith %}

Works exactly the same, including paging using the pager scomp. Only
thing that is different are that there are no custom pivots, and some
sort fields might be named differently.


Finding similar resources
--------------------------

Using Solr's MoreLikeThis feature we implemented a simple
matcher. Following example finds 4 resources which are closely related
to resource nr 332:

    {% for id in m.search[{match id=332 pagelen=4}] %}
    {{ m.rsc[id].title }},
    {% endfor %}


Full-text search and text highlighting
--------------------------------------

Fulltext search works out of the box. You can tell solr to highlight
the relevant sections of a document for you. Currently this does only
work for the title and the summary (the document body is only indexed
in solr, not stored).

Note that if you use highlighting you get an extended search result
format. Following query searches fulltext for the string "hello world"
in docs with the category "text" (or below). It tells solr to
highlight the found titles and summaries (with <em> tags):

    {% with m.search[{solr text="hello world" highlight="title,summary" cat="text"}] as r %}

The Ids are now in "r.result.ids":

    {% for id in r.result.ids %}

And to show the title highlighted:

    {{ r.result.highlight[id].title|default:m.rsc[id].title }}

..and the summary:

    {{ r.result.highlight[id].summary|default:m.rsc[id].summary }}


    {% endfor %}


Facetting
---------

Facetting is the subdivision of the search result into a couple of
relevant groups, like Zotonic's categories for instance. With a single
search result, you can show the division of how many items are in a
certain group.

The following tells Solr to facet on the rsc category:

    {% with m.search[{solr text="hello world" facet="category"}] as r %}

    {% for category_name,count in r.result.facet_fields.category %}
        <li>{{ category-name}} contains {{ count }} items.</li>
    {% endfor %}

    {% endwith %}

