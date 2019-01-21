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
--import Text.Pandoc.Highlighting
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

--import Uniform.Error hiding (Meta, at)
--import Lib.FileMgt (MarkdownText(..), unMT, HTMLout(..), unHTMLout
--            , unDocValue, DocValue (..) )

main :: IO ()
main = do
    -- prepare
    pageWB <- T.readFile "pageWithBiblio.markdown"

    html <- unPandocM $ markdownToHTML4x pageWB

    html2 <- processCites2x pageWB

    if html == html2 then   T.putStrLn "OK - same result \n"
                     else  do
                            T.putStrLn "*** not same result \n"
                            T.putStrLn html
                            putStrLn "\nthe html should be : \n"
                            T.putStrLn html2

    return ()


-- | Convert markdown text to html only if biblio file is in markdown ;

markdownToHTML4x :: Text -> PandocIO Text
markdownToHTML4x t  = do
    pandoc1   <- readMarkdown markdownOptions  t
    let meta2 = flattenMeta (getMeta pandoc1)

    -- test if biblio is present and apply
    let bib =  T.unpack . fromJust $  ( meta2) ^? key "bibliography" . _String ::  FilePath
    --  csl <- liftIO $ T.readFile "chicago.csl"
    --  let csl = fmap t2s $  ( meta2) ^? key "csl" . _String :: Maybe FilePath
    pandoc2 <-  processCites2 "chicago.csl" bib t pandoc1
                    -- :: Style -> [Reference] -> Pandoc -> Pandoc
    --                res <- processCites2x csl bib t
    if (pandoc1 == pandoc2) then  liftIO $ putStrLn "\n*** result without references ***\n"
                            else return ()

    text2 <- writeHtml5String  html5Options pandoc2

    return text2

processCites2 ::  FilePath ->  FilePath -> Text -> Pandoc -> PandocIO Pandoc
-- porcess the cites in the parsed pandoc, filepath is cls and bibfile name

-- porcess the cites in the parsed pandoc, filepath is cls and bibfile name
processCites2 cslfn bibfn  t pandoc1 = do
--        let bibfn2 = fromJust bibfn
--        putIOwords ["processCite2 - filein\n", showT styleFn2, "\n", showT bibfn2]
--        styleIn <- readFile2 styleFn2
        style1 <- liftIO $ liftIO $ readCSLFile Nothing   cslfn
--        putIOwords ["processCite2 - style1", showT style1]

        bibReferences <- liftIO $ readBibtex (const True) False False bibfn
--        putIOwords ["processCite2 - bibReferences", showT bibReferences]
--        :: (String -> Bool) -> Bool -> Bool -> FilePath -> IO [Reference]
        pandoc3   <- readMarkdown markdownOptions  t

        let pandoc4 = processCites style1 bibReferences pandoc3
        liftIO $ putStrLn . unwords $  ["processCite2 - result pandoc2", show pandoc4]
        return pandoc4
--    where
--        meta2 = getMeta pandoc1 :: Meta
--        stylefn =  fmap t2s $ meta2 ^? key "csl" . _String  :: Maybe FilePath
--        bibfn =  fmap t2s $ meta2 ^? key "bibliography" . _String  :: Maybe FilePath

-- | Handle possible pandoc failure within the Action Monad
unPandocM :: PandocIO a -> IO a
unPandocM op1 = do
        res   <- runIO (do  -- liftIO $T.putStrLn "unPandocM op"
                                     a <- op1 --       error "xx"
                                     -- liftIO $T.putStrLn "error xx"
                                     return a)
        case res of
            Left msg -> do
                            putStrLn . show $ msg
                            error (show msg)
            Right a  -> return a
--   `catch` (\e -> do
--                        T.putStrLn . unwords $  ["unPandocM catchError", show e ]
--                        throwError . show $  e)


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
--        let styleFn2 = maybe apaCSL id cslfn
--            bibfn2 = fromJustNote "processCites2x ew224" bibfn   -- tested befire
--        putIOwords ["processCite2x" ] -- - filein\n", showT styleFn2, "\n", showT bibfn2]

        let cmd = "pandoc"
        let cmdargs = ["--from=markdown", "--to=html5", "--filter=pandoc-citeproc" ]
--                    ++ cmdargsbib ++ cmdargsCSL
--        let cmdargsbib =  "--bibliography=" <> showT cslfn
--        let cmdargsCSL = "--csl=" <> showT bibfn

        let cmdinp = T.unpack t
        res :: String <-   System.readProcess cmd cmdargs cmdinp

        return  . T.pack $ res
        -- error are properly caught and reported in ErrIO



readMarkdown2 :: Text -> IO Pandoc
readMarkdown2 text1 =  unPandocM $ readMarkdown markdownOptions text1

writeHtml5String2 :: Pandoc -> IO Text
writeHtml5String2 pandocRes = do
    p <-  unPandocM $ writeHtml5String html5Options pandocRes
    return   p


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
