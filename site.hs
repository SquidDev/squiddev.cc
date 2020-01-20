{-# LANGUAGE FlexibleContexts, MultiWayIf, OverloadedStrings #-}

module Main(main) where

import Control.Monad.Except

import Control.DeepSeq (rnf)
import Control.Concurrent
import Control.Exception

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.HashSet as HSet
import qualified Data.Text.Lazy as L
import Data.Function
import Data.Functor
import Data.Default
import Data.List
import Data.Char

import qualified Text.HTML.TagSoup as TS
import Text.Sass.Functions
import Text.Pandoc.Walk
import Text.Pandoc

import Hakyll.Core.Configuration
import Hakyll.Web.Sass
import Hakyll

import qualified Skylighting as S

import System.IO
import System.Process
import System.Exit

-- | Compress the HTML and CSS today.
compress :: Bool
compress = True

-- | Optionally apply this monad if 'compress' is true.
compresses :: Applicative m => (a -> m a) -> a -> m a
compresses f = if compress then f else pure

main :: IO ()
main =
  hakyllWith def { previewPort = 8080 } $
  do
    match "assets/*.svg" $ do
        route idRoute
        compile $ getResourceString
              >>= compresses minifyHtml

    match "assets/main.scss" $ do
        route $ setExtension "css"
        compile $ sassCompilerWith def { sassOutputStyle = if compress then SassStyleCompressed else SassStyleExpanded
                                       , sassImporters = Just [ sassImporter ]
                                       }

    match ("assets/**.png" .||. "favicon.ico" .||. "robots.txt") $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/**.dot" $ do
      route $ setExtension "png"
      compile $ do
        input <- itemBody <$> getResourceLBS
        (output, err) <-
          (unsafeCompiler . try $ readProcessBS "dot" ["-T", "png"] input)
          >>= either (fail . (displayException :: SomeException -> String)) pure
        unless (null err) (throwError [err])
        makeItem output

    match "index.html" $ do
        route idRoute

        compile $
          let ctx = listField "posts" postCtx (fmap (take 5) . recentFirst =<< loadAll "posts/*")
                 <> defaultContext
           in getResourceBody
          >>= applyAsTemplate ctx
          >>= defaultTemplate

    matchMetadata "posts/*.md" isPublished $ do
        -- Rosts/2018-10-12-foo.md to 2018/10/12/foo.html
        route $
          setExtension "html" `composeRoutes`
          gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" (replaceAll "-" (const "/")) `composeRoutes`
          gsubRoute "posts/" (const "")

        compile $ pandocCustomCompiler
              >>= loadAndApplyTemplate "templates/basic.html" postCtx
              >>= saveSnapshot "content"
              >>= defaultTemplateWith postCtx

    create ["atom.xml"] $ do
      route idRoute
      compile $ renderFeeds renderAtom

    create ["rss.xml"] $ do
      route idRoute
      compile $ renderFeeds renderRss

    match "templates/*.html" $ compile templateBodyCompiler

    match "syntax/*.xml" $ compile $ do
      path <- toFilePath <$> getUnderlying
      contents <- itemBody <$> getResourceBody
      debugCompiler ("Loaded syntax definition from " ++ show path)
      res <- unsafeCompiler (S.parseSyntaxDefinitionFromString path contents)
      _ <- saveSnapshot "syntax" =<< case res of
        Left e -> fail e
        Right x -> makeItem x
      makeItem contents

    match "syntax/*.xml" $ compile getResourceBody

    match "errors/*.md" $ do
      route $ setExtension "html"
      compile $ pandocCustomCompiler
            >>= loadAndApplyTemplate "templates/basic.html" defaultContext
            >>= defaultTemplate

  where
    renderFeeds f = do
      let feedCtx = postCtx <> bodyField "description"
      posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
      f siteFeed feedCtx posts


-- | The default context for the whole site, including site-global
-- properties.
siteCtx :: Context String
siteCtx = constField "site.title" "SquidDev"
       <> constField "site.description" "The personal website of Jonathan Coates."

       <> field "site.versions.main_css" (const . hashCompiler . fromFilePath $ "assets/main.scss")
       <> field "site.versions.main_js"  (const . hashCompiler . fromFilePath $ "assets/main.js")
       <> defaultContext

siteFeed :: FeedConfiguration
siteFeed = FeedConfiguration
    -- TODO: Shift all of this out into common fields with siteCtx
    { feedTitle       = "SquidDev"
    , feedDescription = "The personal website of Jonathan Coates."
    , feedAuthorName  = "SquidDev"
    , feedAuthorEmail = "contact@squiddev.cc"
    , feedRoot        = "https://squiddev.cc"
    }

-- | The extended context for posts, should be composed with 'siteCtx'.
--
-- This just adds in a date field, extracted from the post's title or
-- date property.
postCtx :: Context String
postCtx
   = dateField "date" "%b %e, %Y"
  <> defaultContext

-- | Determine if this post has been published (is not a draft).
isPublished :: Metadata -> Bool
isPublished meta =
  case lookupString "draft" meta of
    Nothing -> True
    Just "true" -> False
    Just "false" -> True
    Just x -> error ("Unknown field '" ++ x ++ "'")

-- | Apply the default HTML template
defaultTemplate :: Item String -> Compiler (Item String)
defaultTemplate = defaultTemplateWith mempty

defaultTemplateWith :: Context String -> Item String -> Compiler (Item String)
defaultTemplateWith ctx page
    = page
    & loadAndApplyTemplate "templates/default.html" (ctx <> siteCtx)
  >>= compresses minifyHtml

-- | A custom sass importer which also looks within @node_modules@.
sassImporter :: SassImporter
sassImporter = SassImporter 0 go where
  go "normalize" _ = do
    c <- readFile "node_modules/normalize.css/normalize.css"
    pure [ SassImport { importPath = Nothing
                      , importAbsolutePath = Nothing
                      , importSource = Just c
                      , importSourceMap = Nothing
                      } ]
  go _ _ = pure []

-- | Attempts to minify the HTML contents by removing all superfluous
-- whitespace.
minifyHtml :: Item String -> Compiler (Item String)
minifyHtml = pure . fmap minifyHtml'

-- | The main worker for minifyHtml.
minifyHtml' :: String -> String
minifyHtml' = withTagList (walk [] [] []) where
  walk _ _ _ [] = []
  walk noTrims noCollapses inlines (x:xs) = case x of
    o@(TS.TagOpen tag _) ->
      o:walk (maybeCons (noTrim tag) tag noTrims)
             (maybeCons (noCollapse tag) tag noCollapses)
             (maybeCons (inline tag) tag inlines)
             xs

    TS.TagText text -> (:walk noTrims noCollapses inlines xs) . TS.TagText $
      if
        | null noCollapses -> collapse (null inlines) text
        | null noTrims     -> trim (null inlines) text
        | otherwise        -> text

    c@(TS.TagClose tag) ->
      c:walk (maybeDrop tag noTrims)
             (maybeDrop tag noCollapses)
             (maybeDrop tag inlines)
             xs

    -- Strip metadata
    TS.TagComment{}  -> walk noTrims noCollapses inlines xs
    TS.TagWarning{}  -> walk noTrims noCollapses inlines xs
    TS.TagPosition{} -> walk noTrims noCollapses inlines xs

  noTrim, noCollapse, inline :: String -> Bool
  -- | Tags which should not have whitespace touched (consecutive spaces
  -- merged, or leading/trailing spaces trimmed).
  noCollapse = flip HSet.member $ HSet.fromList
    [ "pre", "textarea", "script", "style" ]
  -- | Tags which should not have whitespace trimmed.
  noTrim = flip HSet.member $ HSet.fromList
    [ "pre", "textarea" ]
  -- | Tags which are "inline" or contain inline content, and thus should
  -- have leading/trailing spaces preserved.
  inline = flip HSet.member $ HSet.fromList
    [ "a", "abbr", "acronym", "b", "bdi", "bdo", "big", "button", "cite", "code"
    , "del", "dfn", "em", "font", "figcaption", "i", "img", "input", "ins", "kbd"
    , "label" , "li", "mark", "math", "nobr", "object", "p", "q", "rp", "rt"
    , "rtc", "ruby", "s", "samp", "select", "small", "span", "strike", "strong"
    , "sub", "sup", "svg", "textarea", "time", "tt", "u", "var", "wbr"
    ]

  trim _ "" = ""
  trim strip xs =
    let isLast = not strip && isSpace (last xs)
        isFirst = not strip && isSpace (head xs)

        space True = " "
        space False = ""
    in
    case dropWhile isSpace . dropWhileEnd isSpace $ xs of
      "" -> space (isFirst || isLast)
      xs -> space isFirst ++ xs ++ space isLast

  -- | Collapse adjacent spaces into one, and optionally trim the front/back
  collapse strip = trim strip . collapse'

  -- | Collapses adjacent spaces into one
  collapse' [] = []
  collapse' (x:xs)
    | isSpace x = ' ':collapse' (dropWhile isSpace xs)
    | otherwise = x:collapse' xs

  maybeDrop y (x:xs) | x == y = xs
  maybeDrop _ xs = xs

  maybeCons True x xs = x:xs
  maybeCons False _ xs = xs

-- | Generate a trivial cachebuster hash of a cached identifier.
--
-- Note that this should be a file in the main directory, not result of a
-- match.
hashCompiler :: Identifier -> Compiler String
hashCompiler x = take 16 . SHA.showDigest . SHA.sha256 . BS.pack <$> loadBody x

-- | Custom 'WriterOptions' for Pandoc, which emits MathJax instead of MathML, and
-- loads additional syntax highlighting maps.
writerOptions :: Compiler WriterOptions
writerOptions = do
  syntaxMap <- loadAllSnapshots "syntax/*.xml" "syntax"
           <&> foldr (S.addSyntaxDefinition . itemBody) S.defaultSyntaxMap

  pure $ defaultHakyllWriterOptions
    { writerExtensions = extensionsFromList
                         [ Ext_tex_math_dollars
                         , Ext_tex_math_double_backslash
                         , Ext_latex_macros
                         ] <> writerExtensions defaultHakyllWriterOptions
    , writerHTMLMathMethod = MathJax ""
    , writerSyntaxMap = syntaxMap
    }

-- | The Pandoc compiler, but using our custom 'writerOptions'.
pandocCustomCompiler :: Compiler (Item String)
pandocCustomCompiler = do
  writer <- writerOptions
  pandocCompilerWithTransformM defaultHakyllReaderOptions writer (walkM transformInline)
  where
    transformInline :: Inline -> Compiler Inline
    transformInline (Math kind math) = unsafeCompiler $ do
      let args = case kind of
            DisplayMath -> ["-Sd"]
            InlineMath -> ["-S"]
      (contents, _) <- readProcessBS "node_modules/.bin/katex" args . L.encodeUtf8 . L.pack $ math
      pure . RawInline "html" . L.unpack . L.decodeUtf8 $ contents
    transformInline x = pure x

-- | A ByteString equivalent of readProcessWithExitCode (with some more exceptions)
readProcessBS :: FilePath -> [String] -> BS.ByteString -> IO (BS.ByteString, String)
readProcessBS path args input =
  let process = (proc path args)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  in withCreateProcess process $ \stdin stdout stderr ph ->
    case (stdin, stdout, stderr) of
      (Nothing, _, _) -> fail "Failed to get a stdin handle."
      (_, Nothing, _) -> fail "Failed to get a stdout handle."
      (_, _, Nothing) -> fail "Failed to get a stderr handle."
      (Just stdin, Just stdout, Just stderr) -> do
        out <- BS.hGetContents stdout
        err <- hGetContents stderr

        withForkWait (evaluate $ rnf out) $ \waitOut ->
          withForkWait (evaluate $ rnf err) $ \waitErr -> do
            -- Write input and close.
            BS.hPut stdin input
            hClose stdin

            -- wait on the output
            waitOut
            waitErr

            hClose stdout
            hClose stderr

            -- wait on the process
            ex <- waitForProcess ph
            case ex of
              ExitSuccess -> pure (out, err)
              ExitFailure ex -> fail (err ++ "Exited with " ++ show ex)

  where
    withForkWait :: IO () -> (IO () ->  IO a) -> IO a
    withForkWait async body = do
      waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
      mask $ \restore -> do
        tid <- forkIO $ try (restore async) >>= putMVar waitVar
        let wait = takeMVar waitVar >>= either throwIO return
        restore (body wait) `onException` killThread tid
