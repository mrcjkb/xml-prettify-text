-----------------------------------------------------------------------------
-- Based on Text.XML.Prettify by David M. Rosenberg
-- Module       : Text.XML.Prettify
-- Copyright    : (c) 2010 David M. Rosenberg
-- License      : BSD3
--
-- Modified by  : Marc Jakobi, 2021-09-09
-- Modifications:
--                - Update to Haskell 2010
--                - Replace String with Data.Text
--                - Encapsulate internals of module
--
-- Description  : Pretty-print XML Text
-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}


module Text.XML.Prettify
  ( XmlText,
    prettyPrintXml,
  )
where

import Prelude
import qualified Data.Text as T

prettyPrintXml :: XmlText -> XmlText
prettyPrintXml xmlText = printAllTags tags
  where
    tags = inputToTags xmlText

data TagType = IncTagType | DecTagType | StandaloneTagType
  deriving stock (Ord, Eq, Enum)

type XmlText = T.Text

data XmlTag = XmlTag
  { content :: XmlText,
    tagtype :: TagType
  }
  deriving stock (Ord, Eq)

inputToTags :: XmlText -> [XmlTag]
inputToTags "" = []
inputToTags xmlText = xtag : inputToTags xmlText'
  where
    (xtag, xmlText') = lexOne xmlText

lexOne :: XmlText -> (XmlTag, XmlText)
lexOne xmlText = case nextC of
  ' ' -> (XmlTag "" StandaloneTagType, "")
  '<' -> lexOneTag xmlText
  _ -> lexNonTagged xmlText
  where
    nextS = T.dropWhile (`elem` whiteSpaceOrNewlineChars) xmlText
    nextC = T.head $ nextS <> " "

lexNonTagged :: XmlText -> (XmlTag, XmlText)
lexNonTagged xmlText = (XmlTag tagContent tagType, remaining)
  where
    xmlTextWithoutWhitespaceOrNewLines = T.dropWhile (`elem` whiteSpaceOrNewlineChars) xmlText
    (tagContent, remaining) = T.break (== '<') xmlTextWithoutWhitespaceOrNewLines
    tagType = StandaloneTagType

whiteSpaceOrNewlineChars :: [Char]
whiteSpaceOrNewlineChars = " \t\r\n"

lexOneTag :: XmlText -> (XmlTag, XmlText)
lexOneTag xmlText = (XmlTag tagContent tagType, res)
  where
    afterTagStart = T.dropWhile (/= '<') xmlText
    (tagContent', remaining) = T.span (/= '>') afterTagStart
    tagContent = tagContent' <> (T.singleton . T.head) remaining
    res = T.tail remaining
    tagType = case (T.index tagContent 1, T.index (T.reverse tagContent) 1) of
      ('/', _) -> DecTagType
      (_, '/') -> StandaloneTagType
      ('!', _) -> StandaloneTagType
      ('?', _) -> StandaloneTagType
      (_, _) -> IncTagType

printTag :: Int -> XmlTag -> (XmlText, Int)
printTag tagIdent tag = (outtext, nextTagIdent)
  where
    (contentIdent, nextTagIdent) = case tagtype tag of
      IncTagType -> (tagIdent, tagIdent + 1)
      DecTagType -> (tagIdent - 1, tagIdent - 1)
      _ -> (tagIdent, tagIdent)
    outtext = T.replicate contentIdent "  " <> content tag

printAllTags :: [XmlTag] -> XmlText
printAllTags = printTags 0

printTags :: Int -> [XmlTag] -> XmlText
printTags _ [] = ""
printTags ident (tag:tags) = T.intercalate "\n" [tagText, remainingTagText]
  where
    (tagText, nextTagIdent) = printTag ident tag
    remainingTagText = printTags nextTagIdent tags
