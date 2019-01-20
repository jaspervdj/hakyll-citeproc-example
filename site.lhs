Example site using a bibliography
=================================

> {-# LANGUAGE OverloadedStrings #-}
> import Hakyll

We have one function -- the main one.

> main :: IO ()
> main = hakyll $ do

We'll use a simple HTML template.

>     match "default.html" $ compile templateBodyCompiler

Next, we load two auxiliary items that we will use later on.  By using these
Hakyll compilers we get nice dependency tracking for our bibliography.

>     match "chicago.csl" $ compile cslCompiler
>     match "refs.bib" $ compile biblioCompiler

In our simple example, we also just have a single page.

>     match "page.markdown" $ do
>         route   $ setExtension "html"
>         compile $ do

We load in the two auxiliary items.

>             csl <- load "chicago.csl"
>             bib <- load "refs.bib"

We cannot use the standard `pandocCompiler` because we need to process the
references.  This is we we put together our own, based on three simple steps:

1.  Read out the page as a `String` (`getResourceBody`)
2.  Parse the `String` to a `Pandoc` document (with resolved references, using
    `readPandocBiblio`).
3.  Write the `Pandoc` document back out to a `String` (`writePandoc`).

>             getResourceBody
>                 >>= readPandocBiblio defaultHakyllReaderOptions csl bib
>                 >>= return . writePandoc

The next line is fairly standard and applies our HTML template.

>                 >>= loadAndApplyTemplate "default.html" defaultContext
