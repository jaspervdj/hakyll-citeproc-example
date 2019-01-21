-- minimal example of using CiteProc without Hakyll

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module  Main where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens

import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Text.CSL (readCSLFile)
import Text.CSL.Input.Bibtex (readBibtex)
import Text.CSL.Pandoc (processCites)

import System.Process  as System (readProcess)
import qualified Data.Text.IO as T (readFile, putStrLn)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Maybe
import Control.Monad.IO.Class


main :: IO ()
main = do
    -- prepare
    pageWB <- T.readFile "pageWithBiblio.markdown"

    -- process with my code
    html <- unPandocM $ markdownToHTML4x pageWB

    -- process with system call to pandoc
    html2 <- processCites2x pageWB

    if html == html2 then   T.putStrLn "OK - same result \n"
                     else  do
                            T.putStrLn "*** not same result \n"
                            T.putStrLn html
                            putStrLn "\nthe html should be : \n"
                            T.putStrLn html2

    return ()


-- | Convert markdown text to html only if biblio file is in markdown ;
-- to get biblio filename

markdownToHTML4x :: Text -> PandocIO Text
markdownToHTML4x t  = do
    pandoc1   <- readMarkdown markdownOptions  t
    let meta2 = flattenMeta (getMeta pandoc1)

    let bib =  T.unpack . fromJust $  ( meta2) ^? key "bibliography" . _String ::  FilePath

    pandoc2 <-  processCites2 "chicago.csl" bib t

    if (pandoc1 == pandoc2) then  liftIO $ putStrLn "\n*** result without references ***\n"
                            else return ()

    text2 <- writeHtml5String  html5Options pandoc2

    return text2

processCites2 ::  FilePath ->  FilePath -> Text ->  PandocIO Pandoc
-- process the citations, with biblio in first file and csl in second
processCites2 cslfn bibfn  t  = do

        style1 <- liftIO $ liftIO $ readCSLFile Nothing   cslfn

        bibReferences <- liftIO $ readBibtex (const False) False False bibfn

        pandoc3   <- readMarkdown markdownOptions  t

        let pandoc4 = processCites style1 bibReferences pandoc3

        liftIO $ putStrLn . unwords $  ["processCite2 - result pandoc2", show pandoc4]
        return pandoc4

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
      ]
    , githubMarkdownExtensions
    ]

-- | Reasonable options for rendering to HTML
html5Options :: WriterOptions
html5Options = def { -- writerHighlightStyle = Just tango
                    writerExtensions     = writerExtensions def
                   }



getMeta :: Pandoc -> Meta
getMeta (Pandoc m _) = m


processCites2x ::   Text ->   IO Text
-- porcess the cites in the text
-- using systemcall because the standalone pandoc works
-- call is: pandoc -f markdown -t html  --filter=pandoc-citeproc
-- the csl and bib file are used from text, not from what is passed

processCites2x   t  = do

        let cmd = "pandoc"
        let cmdargs = ["--from=markdown", "--to=html5", "--filter=pandoc-citeproc" ]

        let cmdinp = T.unpack t
        res :: String <-   System.readProcess cmd cmdargs cmdinp

        return  . T.pack $ res

--readMarkdown2 :: Text -> IO Pandoc
--readMarkdown2 text1 =  unPandocM $ readMarkdown markdownOptions text1
--
--writeHtml5String2 :: Pandoc -> IO Text
--writeHtml5String2 pandocRes = do
--    p <-  unPandocM $ writeHtml5String html5Options pandocRes
--    return   p


-- | Flatten a Pandoc 'Meta' into a well-structured JSON object, rendering Pandoc
-- text objects into plain strings along the way.
flattenMeta :: Meta -> Value
flattenMeta (Meta meta) = toJSON $ fmap go meta
 where
  go :: MetaValue -> Value
  go (MetaMap     m) = toJSON $ fmap go m
  go (MetaList    m) = toJSONList $ fmap go m
  go (MetaBool    m) = toJSON m
  go (MetaString  m) = toJSON m
  go (MetaInlines m) = toJSON $ stringify m
  go (MetaBlocks  m) = toJSON $ stringify m
