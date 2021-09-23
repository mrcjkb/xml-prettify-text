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
import Data.Char (isSpace)

prettyPrintXml :: XmlText -> XmlText
prettyPrintXml xmlText = printAllTags tags
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
    tagContent =  tagContent' <> (T.singleton . T.head) remaining
    res = T.tail remaining
    tagType = case (T.index tagContent 1, T.index tagContent (T.length tagContent - 2)) of
      ('/', _) -> DecTagType
      (_, '/') -> StandaloneTagType
      ('!', _) -> StandaloneTagType
      ('?', _) -> StandaloneTagType
      (_, _) -> IncTagType

printTag :: Int -> XmlTag -> (Int, XmlText)
printTag tagIdent (XmlTag tagContent tagType) = (nextTagIdent, outText)
  where
    (contentIdent, nextTagIdent) = case tagType of
      IncTagType -> (tagIdent, tagIdent + 1)
      DecTagType -> (tagIdent - 1, tagIdent - 1)
      _ -> (tagIdent, tagIdent)
    outText = T.replicate contentIdent "  " <> tagContent

printAllTags :: [XmlTag] -> XmlText
printAllTags = printTags 0

printTags :: Int -> [XmlTag] -> XmlText
printTags _ [] = ""
printTags ident (tag:tags) = T.intercalate "\n" [tagText, remainingTagText]
  where
    (nextTagIdent, tagText) = printTag ident tag
    remainingTagText = printTags nextTagIdent tags
