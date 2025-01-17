{-# LANGUAGE FlexibleContexts, MultiWayIf, OverloadedStrings, ScopedTypeVariables #-}

module Main(main) where

import qualified GHC.IO.Encoding as Encoding

import System.Directory

import Control.Monad.Except
import Control.Exception
import Control.Monad

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Base64.Lazy as BS
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.HashSet as HSet
import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Function
import Data.Foldable
import Data.Functor
import Data.Default
import Data.List
import Data.Char

import qualified Text.HTML.TagSoup as TS
import Text.Pandoc.Walk
import Text.Pandoc
import Text.Sass

import Hakyll.Core.Configuration
import Hakyll

import qualified Skylighting as S

import System.Process.Typed

-- | Compress the HTML and CSS today.
compress :: Bool
compress = True

-- | Optionally apply this monad if 'compress' is true.
compresses :: Applicative m => (a -> m a) -> a -> m a
compresses f = if compress then f else pure

main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8
  hakyllWith def { previewPort = 8080 } rules

rules :: Rules ()
rules =
  do
    match "assets/*.svg" $ do
        route idRoute
        compile $ getResourceString
              >>= compresses minifyHtml

    match "assets/main.scss" $ do
        route $ setExtension "css"
        compile sassCompiler

    match ("assets/**.scss" .||. "node_modules/**.css" .||. "assets/**.embed.png") $ compile (makeItem ())

    match ("assets/**.png" .||. "favicon.ico" .||. "robots.txt") $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/**.dot" $ do
      route $ setExtension "png"
      compile $ do
        input <- itemBody <$> getResourceLBS
        (output, err) <-
          (unsafeCompiler . try $ readProcess_ (setStdin (byteStringInput input) $ proc "dot" ["-T", "png"]))
          >>= either (fail . (displayException :: SomeException -> String)) pure
        unless (BS.null err) (throwError [L.unpack . L.decodeUtf8 $ err])
        makeItem output

    match "index.html" $ do
        route idRoute

        compile $
          let ctx = listField "posts" postCtx (recentFirst =<< loadAll "posts/*")
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
      contents <- itemBody <$> getResourceLBS
      debugCompiler ("Loaded syntax definition from " ++ show path)
      let res = S.parseSyntaxDefinitionFromText path (L.decodeUtf8 contents)
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

-- | Compiles sass files to CSS.
--
-- Similar to that provided by hakyll-sass, but correctly handles
-- dependencies.
sassCompiler :: Compiler (Item String)
sassCompiler = do
  path <- getResourceFilePath
  (contents, deps) <- unsafeCompiler $ do
    result <- compileFile path options
    case result of
      Left err -> errorMessage err >>= fail
      Right result -> do
        lPath <- canonicalizePath path
        includes <- resultIncludes result
                >>= traverse canonicalizePath
                <&> delete lPath
                >>= traverse makeRelativeToCurrentDirectory
        pure (resultString result, includes)

  -- Mark each of these as a dependency.
  traverse_ (fmap (\(_ :: Item ()) -> ()) . load . fromFilePath) deps
  makeItem contents

  where
    options :: SassOptions
    options = def
      { sassOutputStyle = if compress then SassStyleCompressed else SassStyleExpanded
      , sassIncludePaths = Just ["node_modules/normalize.css"]
      }

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
    , "del", "dfn", "em", "font", "figcaption", "h1", "h2", "h3", "h4", "h5"
    , "h6", "i", "img", "input", "ins", "kbd", "label" , "li", "mark", "math"
    , "nobr", "object", "p", "q", "rp", "rt", "rtc", "ruby", "s", "samp"
    , "select", "small", "span", "strike", "strong", "sub", "sup", "svg"
    , "textarea", "time", "tt", "u", "var", "wbr"
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

-- | The Pandoc compiler, with additional transforms and options.
--
-- Along with using 'writerOptions', this also performs the following
-- translations:
--
--  - LaTeX math expressions shell out to katex
--  - Code blocks of language "dot" shell out to graphviz.
pandocCustomCompiler :: Compiler (Item String)
pandocCustomCompiler = do
  writer <- writerOptions
  pandocCompilerWithTransformM defaultHakyllReaderOptions writer
    (walkM transformInline >=> walkM (concatMapA transformBlock))
  where
    transformInline :: Inline -> Compiler Inline
    transformInline (Math kind math) = unsafeCompiler $ do
      let args = case kind of
            DisplayMath -> ["-Sd"]
            InlineMath -> ["-S"]
      RawInline "html" <$> readProcessText "katex" args math
    transformInline x = pure x

    transformBlock :: Block -> Compiler [Block]
    transformBlock (CodeBlock ("", cls@("dot":_), attrs) text) = do
      let attrs' = ("class", T.intercalate " " cls):attrs
      contents <- unsafeCompiler $ readProcessText "dot" ["-Tsvg"] text
      xml <- fmap TS.renderTags . traverse (stripTag attrs') $ TS.parseTags contents
      case find (\(k, _) -> k == "title") attrs of
        Nothing -> pure [RawBlock "html" xml]
        Just (_, title) -> do
          caption <- captionReader title
          pure [ RawBlock "html" (T.concat ["<figure>", xml, "<figcaption>"])
               , Plain caption
               , RawBlock "html" "</figcaption></figure>" ]
    transformBlock x = pure [x]

    stripTag _ t@(TS.TagOpen "?xml" _)     = pure $ TS.TagComment (TS.renderTags [t])
    stripTag _ t@(TS.TagOpen "!DOCTYPE" _) = pure $ TS.TagComment (TS.renderTags [t])
    stripTag _ (TS.TagOpen "image" (("xlink:href", img):atts))
      | ".embed.png" `T.isSuffixOf` img = do
      let path = T.unpack img
      (_ :: Item ()) <- load $ fromFilePath path
      contents <- fmap (L.decodeUtf8 . BS.encode) . unsafeCompiler $ BS.readFile path
      pure $ TS.TagOpen "image" (("xlink:href", "data:image/png;base64," <> L.toStrict contents):atts)
    stripTag a (TS.TagOpen "svg" attr')    = pure $ TS.TagOpen "svg" (a ++ attr')
    stripTag _ x = pure x

-- | Read a figure caption in Markdown format. LaTeX math @$...$@ is
-- supported, as are Markdown subscripts and superscripts.
captionReader :: T.Text -> Compiler [Inline]
captionReader t
  = either (const (throwError ["Cannot compile '" ++ T.unpack t ++ "'"])) (pure . extractFromBlocks)
  . runPure . readMarkdown defaultHakyllReaderOptions $ t
  where
    extractFromBlocks (Pandoc _ blocks) = mconcat $ extractInlines <$> blocks

    extractInlines (Plain inlines)          = inlines
    extractInlines (Para inlines)           = inlines
    extractInlines (LineBlock multiinlines) = join multiinlines
    extractInlines _                        = []

concatMapA :: Applicative f => (a -> f [b]) -> [a] -> f [b]
concatMapA _ [] = pure []
concatMapA f (x:xs) = (++) <$> f x <*> concatMapA f xs

readProcessText :: String -> [String] -> T.Text -> IO T.Text
readProcessText cmd args input =
  L.toStrict . L.decodeUtf8
  <$> readProcessStdout_ (setStdin (byteStringInput . L.encodeUtf8 . L.fromStrict $ input) (proc cmd args))
