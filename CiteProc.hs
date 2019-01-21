-- minimal example of using CiteProc without Hakyll

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module  Main where

import Text.Pandoc
import Text.CSL (parseCSL)
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
    let bib = "refs.bib"
    let csl = "chicago.csl"
    -- process with my code
    html1 <- unPandocM $    processCites2 csl bib page

    -- process with system call to pandoc
    html2 <- processCites2x page

    if html1 == html2 then   T.putStrLn "OK - same result \n"
                     else  do
                            T.putStrLn "*** not same result \n no references: \n"
                            T.putStrLn html1
                            putStrLn "\nthe html should be : \n"
                            T.putStrLn html2

    return ()

processCites2 ::  FilePath ->  FilePath -> Text ->  PandocIO Text
-- process the citations, with biblio in first file and csl in second
processCites2 cslfn bibfn  t  = do

-- read style and biblio file in and convert separately
-- check that files are present and converted results look ok

        styleString <- liftIO $ readFile   cslfn
        let style1 = parseCSL  styleString

        liftIO $ putStrLn "style1 \n"
        liftIO $ putStrLn . take 60 . show $ style1

        bibString <- liftIO $ readFile bibfn
        bibReferences <- liftIO $ readBibtexString (const True) True False bibString
        -- second parameter is true = bibtex false = biblatex
        liftIO $ putStrLn "\nbibReferences \n"
        liftIO $ putStrLn . take 60 . show $ bibReferences

-- process to HTML
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
      , Ext_citations
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
        let cmdargs = ["--from=markdown", "--to=html5", "--filter=pandoc-citeproc", "--bibliography=refs.bib" ]

        let cmdinp = T.unpack t
        res :: String <-   System.readProcess cmd cmdargs cmdinp

        return  . T.pack $ res

