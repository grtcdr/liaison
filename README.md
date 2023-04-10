Liaison, [which has recently migrated to SourceHut](https://git.sr.ht/~grtcdr/liaison), generalizes and
automates the process of linking an *HTML* document to its
corresponding source document originally written in the *Org* markup
language. Its purpose is to generate URLs that point to a specific
page in a Git web interface, e.g. the tree, blob, blame, edit pages,
and more.

For more information on how to install and integrate the package in
your project, you are encouraged to read the [online manual](https://grtcdr.tn/liaison/manual.html).


# How does it work?

Check out [the project's website](https://grtcdr.tn/liaison) to get a feel for what the package can
do. Although it may not seem like it, the hompage gives a
comprehensive tour of the functionalities of the package.


# Who is it for?

Anyone with a blog or documentation website built on top of the
`org-publish-project-alist` variable provided by the built-in
`ox-publish` library or high-level static site generators such as
[ox-hugo](https://ox-hugo.scripter.co/).


# What web interfaces does it support?

-   GitHub
-   GitLab
-   SourceHut
-   Gitea
-   cgit

When applicable, custom instances are supported too.


# Who's using it?

-   [grtcdr.tn](https://grtcdr.tn), in the postamble of blog posts and documentation files.
-   [ushin.org](https://ushin.org), in the postamble of every page.

If you are using Liaison in your project and would like to be included
in the list, feel free to [contact me](mailto:tahaaziz.benali@esprit.tn).


# Citing

If your research involves this project in any way, you may cite it
like so:

    @misc{ab22liaison,
      author = {Aziz Ben Ali},
       title = {Generic Git URIs for Org Mode},
         url = {https://grtcdr.tn/liaison/},
        year = 2022
    }

