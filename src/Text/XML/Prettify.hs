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
{-# LANGUAGE OverloadedStrings #-}

module Text.XML.Prettify
  ( XmlText,
    prettyPrintXml,
  )
where

import qualified Data.Text as T

prettyPrintXml :: XmlText -> XmlText
prettyPrintXml input = printAllTags tags
  where
    tags = inputToTags input

data TagType = Inc | Dec | Standalone
  deriving (Read, Ord, Show, Eq, Enum)

type XmlText = T.Text

data XmlTag = XmlTag
  { content :: XmlText,
    tagtype :: TagType
  }
  deriving (Read, Ord, Eq, Show)

inputToTags :: XmlText -> [XmlTag]
inputToTags "" = []
inputToTags st = xtag : inputToTags st'
  where
    (xtag, st') = lexOne st

lexOne :: XmlText -> (XmlTag, XmlText)
lexOne inp = case (nextC == '<', nextC == ' ') of
  (_, True) -> (XmlTag "" Standalone, "")
  (True, _) -> lexOneTag inp
  (False, _) -> lexNonTagged inp
  where
    nextS = T.dropWhile (`elem` newLineChars) inp
    nextC = T.head $ nextS <> " "

lexNonTagged :: XmlText -> (XmlTag, XmlText)
lexNonTagged inp = (XmlTag con xtag, rem)
  where
    inp' = T.dropWhile (`elem` newLineChars) inp
    (con, rem) = T.span (`notElem` taggedChars) inp'
    xtag = Standalone
    taggedChars = " \n\r<" :: String

newLineChars :: String
newLineChars = " \t\r\n"

lexOneTag :: XmlText -> (XmlTag, XmlText)
lexOneTag inp = (XmlTag contnt xtag, res)
  where
    inp' = T.dropWhile (/= '<') inp
    (con, rem) = T.span (/= '>') inp'
    contnt = con <> (T.singleton . T.head) rem
    res = T.tail rem
    xtag = case (T.index contnt 1, T.index (T.reverse contnt) 1) of
      ('/', _) -> Dec
      (_, '/') -> Standalone
      ('!', _) -> Standalone
      ('?', _) -> Standalone
      (_, _) -> Inc

printTag :: XmlTag -> Int -> (XmlText, Int)
printTag tag ident = (outtext, ident2)
  where
    ident1 = case tagtype tag of
      Dec -> ident - 1
      _ -> ident
    outtext = T.replicate (ident1 * 2) " " <> content tag
    ident2 = case tagtype tag of
      Inc -> ident + 1
      Dec -> ident - 1
      _ -> ident

printAllTags :: [XmlTag] -> XmlText
printAllTags = printTags 0

printTags :: Int -> [XmlTag] -> XmlText
printTags _ [] = ""
printTags ident (tag:tags) = mconcat [txt, "\n", printTags ident' tags]
  where
    (txt, ident') = printTag tag ident
