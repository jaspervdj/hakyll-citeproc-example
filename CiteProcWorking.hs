-- minimal example of using CiteProc

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module  Main where

import Text.Pandoc
import Text.CSL (parseCSL, Style, Reference)
import Text.CSL.Input.Bibtex (readBibtexString)
import Text.CSL.Pandoc (processCites)

import System.Process  as System (readProcess)
import qualified Data.Text.IO as T (readFile, putStrLn)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Control.Monad.IO.Class


main :: IO ()
main = do
    page <- T.readFile "page.markdown"  -- biblio line
    bibString <-  readFile "refs.bib"
    bibReferences :: [Reference] <-  readBibtexString (const True) True False bibString

    styleString <- liftIO $ readFile   "chicago.csl"
    let style1 = parseCSL  styleString :: Style

    -- process with my code
    html1 <- unPandocM $    processCites2 style1 bibReferences page

    -- process with system call to pandoc
    html2 <- processCites2x page


    T.putStrLn "The two results should be very close: \n With haskell code : \n"
    T.putStrLn html1
    putStrLn "\nwith pandoc cmd : \n"
    T.putStrLn html2

    return ()

processCites2 ::  Style ->  [Reference] -> Text ->  PandocIO Text
-- process the citations, with biblio in first file and csl in second
processCites2 style1 bibReferences  t  = do

        pandoc3   <- readMarkdown markdownOptions  t

        let pandoc4 = processCites style1 bibReferences pandoc3

        writeHtml5String  html5Options pandoc4

-- | Handle possible pandoc failures in the Pandoc Monad
unPandocM :: PandocIO a -> IO a
unPandocM op1 = do
        res   <- runIO $ op1
        case res of
            Left msg -> do
                            putStrLn . show $ msg
                            error (show msg)
            Right a  -> return a

-- | Reasonable options for reading a markdown file
markdownOptions :: ReaderOptions
markdownOptions = def { readerExtensions = exts }
 where
  exts = mconcat
    [ extensionsFromList
      [ Ext_yaml_metadata_block
      , Ext_fenced_code_attributes
      , Ext_auto_identifiers
      , Ext_citations           -- <-- this is the important extension
      ]
    , githubMarkdownExtensions
    ]

-- | Reasonable options for rendering to HTML
html5Options :: WriterOptions
html5Options = def { writerExtensions     = writerExtensions def
                   }

processCites2x ::   Text ->   IO Text
-- porcess the cites in the text
-- using systemcall because the standalone pandoc works
-- call is: pandoc -f markdown -t html  --filter=pandoc-citeproc
-- the csl and bib file are used from text, not from what is passed

processCites2x   t  = do
        let cmd = "pandoc"
        let cmdargs = ["--from=markdown"
                        , "--to=html5"
                        , "--filter=pandoc-citeproc"
                        , "--bibliography=refs.bib"
                        , "--csl=chicago.csl"]

        let cmdinp = T.unpack t
        res :: String <-   System.readProcess cmd cmdargs cmdinp

        return  . T.pack $ res

