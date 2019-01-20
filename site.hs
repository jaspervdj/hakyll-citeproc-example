--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "chicago.csl" $ compile cslCompiler
    match "refs.bib" $ compile biblioCompiler

    match "page.markdown" $ do
        route   $ setExtension "html"
        compile $ do
            csl <- load "chicago.csl"
            bib <- load "refs.bib"

            getResourceBody
                >>= readPandocBiblio defaultHakyllReaderOptions csl bib
                >>= return . writePandoc
                >>= loadAndApplyTemplate "default.html" defaultContext

    match "default.html" $ compile templateBodyCompiler
