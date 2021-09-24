{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- Based on Text.XML.Prettify by David M. Rosenberg
-- Copyright    : (c) 2010 David M. Rosenberg
-- License      : BSD3
-----------------------------------------------------------------------------
-- Module        :  Text.XML.Prettify
--
-- Maintained by :  Marc Jakobi, 2021-09-09
-- Modifications:
--                 - Update to Haskell 2010
--                 - Replace String with Data.Text
--                 - Encapsulate internals of module
--                 - Tweak performance
--
-- License       :  GPL2
--
-- Description   :  Pretty-print XML Text
-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}

module Text.XML.Prettify
  ( XmlText,
    prettyPrintXml,
    prettyPrintXmlDefault,
    module Text.XML.Prettify.Options,
  )
where

import Control.Monad.Reader
import Data.Char (isSpace)
import qualified Data.Text as T
import Text.XML.Prettify.Options
import TextShow
import Prelude

type Prettify = Reader PrettifyOpts

-- | Pretty-print an XML text with the default options:
-- EndOfLine: LF
-- Indent style: 2 spaces
-- Returns: The pretty-printed XML
prettyPrintXmlDefault :: XmlText -> XmlText
prettyPrintXmlDefault =
  prettyPrintXml
    PrettifyOpts
      { endOfLine = LF,
        indentStyle = SPACE 2
      }

-- | Pretty-print an XML text
-- opts: The output options
-- xmlText: xml text (e.g. on one line)
-- Returnms: The pretty-printed XML
prettyPrintXml :: PrettifyOpts -> XmlText -> XmlText
prettyPrintXml opts xmlText = runReader (printAllTags tags) opts
  where
    tags = inputToTags $ T.concat $ T.lines xmlText

data TagType = IncTagType | DecTagType | StandaloneTagType
  deriving stock (Ord, Eq, Enum)

type XmlText = T.Text

type TagContent = T.Text

type OneLineXmlText = XmlText

data XmlTag = XmlTag TagContent TagType
  deriving stock (Ord, Eq)

inputToTags :: OneLineXmlText -> [XmlTag]
inputToTags "" = []
inputToTags xmlText = xtag : inputToTags xmlText'
  where
    (xtag, xmlText') = lexOne xmlText

lexOne :: OneLineXmlText -> (XmlTag, OneLineXmlText)
lexOne xmlText = case nextCharacter of
  ' ' -> (XmlTag "" StandaloneTagType, "")
  '<' -> lexOneTag xmlText
  _ -> lexNonTagged xmlText
  where
    nextWord = getWord xmlText
    nextCharacter = T.head $ nextWord <> " "

lexNonTagged :: OneLineXmlText -> (XmlTag, OneLineXmlText)
lexNonTagged xmlText = (XmlTag tagContent tagType, remaining)
  where
    nextWord = getWord xmlText
    (tagContent, remaining) = T.break (== '<') nextWord
    tagType = StandaloneTagType

getWord :: T.Text -> T.Text
getWord = T.dropWhile isSpace

lexOneTag :: OneLineXmlText -> (XmlTag, OneLineXmlText)
lexOneTag xmlText = (XmlTag tagContent tagType, res)
  where
    afterTagStart = T.dropWhile (/= '<') xmlText
    (tagContent', remaining) = T.span (/= '>') afterTagStart
    tagContent = tagContent' <> (T.singleton . T.head) remaining
    res = T.tail remaining
    tagType = case (T.index tagContent 1, T.index tagContent (T.length tagContent - 2)) of
      ('/', _) -> DecTagType
      (_, '/') -> StandaloneTagType
      ('!', _) -> StandaloneTagType
      ('?', _) -> StandaloneTagType
      (_, _) -> IncTagType

printTag :: Int -> XmlTag -> Prettify (Int, XmlText)
printTag tagIdent (XmlTag content tagType) = do
  identStyle <- asks indentStyle
  let identText = showt identStyle
  let (contentIdent, nextTagIdent) = case tagType of
        IncTagType -> (tagIdent, tagIdent + 1)
        DecTagType -> (tagIdent - 1, tagIdent - 1)
        _ -> (tagIdent, tagIdent)
  let outText = T.replicate contentIdent identText <> content
  pure (nextTagIdent, outText)

printAllTags :: [XmlTag] -> Prettify XmlText
printAllTags = printTags 0

printTags :: Int -> [XmlTag] -> Prettify XmlText
printTags _ [] = pure ""
printTags ident (tag : tags) = do
  eol <- asks endOfLine
  (nextTagIdent, tagText) <- printTag ident tag
  remainingTagText <- printTags nextTagIdent tags
  pure $ T.intercalate (showt eol) [tagText, remainingTagText]
