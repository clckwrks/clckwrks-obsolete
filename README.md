# Clckwrks

## Installing and Testing clckwrks

There are three cabal project directories:

clckwrks             - contains the core clckwrks server code
clckwrks-theme-basic - an example clckwrks theme
clckwrks-dot-com     - source for the (future) clckwrks.com website

To run this demo you will also need the follow javascript libraries:

 jquery
 jquery-ui
 json2
 jstree

The first two can be installed via apt-get:

    $ apt-get install libjs-jquery libjs-jquery-ui

The last two can be installed by running 'make' it the top-level directory.

You will also need to install the binaries:

 HsColour
 markdown

These can be installed via:

    $ apt-get install markdown HsColour

The demo clckwrks server can be built statically or with dynamic plugins enabled. 

### Static clckwrks-dot-com

To build the server statically run 'cabal install' in clckwrks, clckwrks-theme-basic, and finally clckwrk-dot-com.

The clckwrks-dot-com is currently configured to be run from the clckwrks-dot-com directory. So to run the server do:

    cd clckwrks-dot-com
    ./dist/build/clckwrks-server/clckwrks-server

It should print out:

    Static Server Started.

You can then point your browser at: http://localhost:8000/

### Dynamic clckwrks-dot-com

To use plugins to load the theme file you need only install clckwrks. Then do the following:

    cd clckwrks-dot-com    
    ln -sf ../clckwrks-theme-basic/Theme .
    runhaskell Setup clean
    runhaskell Setup configure --user -fplugins
    runhaskell Setup build
    ./dist/build/clckwrks-server/clckwrks-server

It should print out:

    Dynamic Server Started.

Now if you edit Theme.Home and reload the homepage, you should see your changes reflected. It may take a second or two for the changes to appear.

Note that symbolic link is needed due to the plugins-auto not handling include directories very well. So, we create a symlink as a work-around.

## A tour of the web-site

When you first open the site, there should be a big blue box and under it there are three columns which say, "Invalid PageId x".

Those columns are supposed to show previews of the 2nd, 3rd, and 4th pages. But those pages have not been created yet. 

So, click on 'admin' to get to the admin console. (in theory, you need to login first, but that is currently not hooked up).

Next click on 'Create New Page'.

The body of the page is processed usThe code is ing markdown. The syntax for markdown is here:

http://daringfireball.net/projects/markdown/syntax

markdown allows you to insert arbitrary HTML as well. So you can either just write some simple text, or you can go nuts with HTML.

If you check the 'Highlight Haskell code with HsColour' option, then you can use bird-notation (aka, >) and include literate Haskell which will be colored using HsColour.

After you click 'update' you should be taken to a view of the page you just created. Notice that the menu on the left has been updated to include the newly created page.

If you now click 'This title rocks!' you should be taken back to the homepage. You will see that the first column now contains a preview of page 2.

If you click on 'admin' again. And then 'this title rocks'. You can edit the title of the page and change it to 'Home'.

In the admin console there is also an 'Edit Menu' feature. This feature is currently incomplete and does not work. It allows you to interactively create and organize the menu.

## A tour of the code

Looking at the code, we see it is divided into three packages: clckwrks, clckwrks-theme-basic, and clckwrks-dot-com.

### clckwrks

A majority of the code lives in clckwrks. As an end-user, you would install this library, but never need to modify the code in it. It contains all the codefor the core features. 

### clckwrks-theme-basic

clckwrks-theme-basic is a theme package. Themes conform to a simple theme API so that you can easily swap out one for another. Themes are designed so that they could be distributed as .tar.gz bundles. Switching to a different theme should not require any code changes. 

For the static server, you would just edit the clckwrks-dot-com.cabal and change clckwrks-theme-basic to clckwrks-theme-awesome.

For the dynamic server you currently need to edit `clckwrksConfig` and change the path to the theme. But ultimately, that is a value that you would change via the admin console. I envision that you would be able to go to the theme store, and one-click install and activate new themes.

A typical clckwrks user may never look inside, create, or edit a theme bundle. They could just use the prepackaged themes from the site.

Theme designers, on the other hand would need to know how to modify the theme files. So, let's look briefly at the structure.

Right now the theme API is simple. The theme should provide the follow modules:
page :: XMLGenT (Clck ClckURL) XML
Theme.Home
Theme.Page

which export a function:

page :: XMLGenT (Clck ClckURL) XML

any assets such as .css, .jpgs, etc should go in the 'data' directory.

Looking at Theme/Page.hs we see that it tries hard to look like a normal HTML template. There is a bit of noise at the top of the file that would be nice to reduce/simplify. 

The `Clckwrks` module exports a ton of stuff so that you do not need a bunch of imports.

Looking at the template we see that it is primarily html, with some simple calls to fill in content such as: `getPageTitle` and `getPageContent`. Those functions come from the clckwrks modules such as `Page.API`.  The `.API` modules are supposed to provide a friendly interface for template designers to use.

The internal URLs use web-routes.

Looking at this template, it seems like it should be easy for template designers to do there thing with out having to have much understanding of Haskell.

Obviously, the big hang-up is the error messages that you can encounter while using HSP. As Niklas mentioned in this thread:

https://groups.google.com/group/haskell-server-pages/browse_thread/thread/1b136c7acb448136

There is a long-term plan for addressing that.

Looking at the Theme.Home file, we see a slightly more advanced template that uses a helper function `summaryBox`.

Hopefully the themes are simple enough that end users can modify an existing theme with out really having to learn any Haskell.

### clckwrks-dot-com

This is where we tie it all together. There are only two files so far.

    Main.hs
    PageMapper.hs

`Main.hs` is a bit scary looking due to the `CPP` code to select between static and dynamic mode. But there is really not much code there. Basically we create a `ClckwrksConfig` and then call `simpleClckwrks` to start the server.

The static code path just imports the `PageMapper.hs` directly and calls `pageMapper`. The dynamic code path imports that symbol via `withMonadIOFile`.

It would be nice to hide some of that ugliness away because it is pretty much boiler-plate. The tricky part is that we don't want to link against plugins-auto at all when building the static server. The current solution is the obvious way to do that. But, hopefully, not the best.

`PageMapper.hs` just contains a simple function `pageMapper`. The purpose of this function is to allow you to specify different page templates for specific pages. For example, we see that PageId 1 will use `Home.page` and all the other pages will use `Page.page`. 

If you comment out the `(PageId 1)` you will see that the bluebox disappears from the home page. If you are using the dynamic server, you can comment that line out and refresh the page and it should be automatically recompiled/reloaded. If you are using the static server, then you will need to rebuild to see the changes.

If you are using the dynamic server and have created the symlink, you could also edit the `Theme.Home` or `Theme.Page` templates and see the changes reflected after a short delay.

### Beyond `simpleClckwrks`

The `simpleClckwrks` entry point is good if you just want a simple wordpress-like site. But, `clckwrks` is also designed to be integrated into a more sophisticated site such as `patch-tag.com`.

An example is forth-coming. The essence is that you would create a new URL type for your site which embeds `ClckURL`:

    data MySite = C ClckURL
                | Home
                | Foo

And then in your routing function you would call `routeClck` for the `ClckURL` case.

## TODO

There is clearly a bunch of stuff left to do including:

 * requiring authentication for the admin console
 * adding an image gallery (code partial exists but needs to be integrated)
 * adding JSON import/export for database
 * integrate comment module
 * add RSS feed / bloggy-stuff
 * tags, searching, etc / CRM
 * menu editor

And then there are a bunch of add-on modules that would be great to have such as:

 * mailing list
 * web-store
 * paypal
 * social media integration
 * members-only content
 * post scheduling (aka, write a bunch of posts now, but have them published automatically on a schedule)
 * and more!


 