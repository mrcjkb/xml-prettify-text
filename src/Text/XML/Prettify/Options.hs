{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Text.XML.Prettify.Options where

import TextShow
import Prelude

-- | The indent size
type IndentSize = Int

-- | The indent style, either `Tab` or `Space` with a given indent size
data IndentStyle = Tab | Space IndentSize
  deriving stock (Eq)

-- | The line break style:
-- Line Feed (LF), Carriage Return (CR),
-- or both (CRLF)
data EndOfLine = LF | CR | CRLF
  deriving stock (Eq)

-- | The options for the Prettify module
data PrettifyOpts = PrettifyOpts
  { -- | The indent style, either `Tab` or `Space` with a given indent size
    indentStyle :: IndentStyle,
    -- | The line break style:
    -- Line Feed (LF), Carriage Return (CR),
    -- or both (CRLF)
    endOfLine :: EndOfLine
  }

instance TextShow EndOfLine where
  showb LF = "\n"
  showb CR = "\r"
  showb _ = foldMap showb [CR, LF]

instance TextShow IndentStyle where
  showb Tab = "\t"
  showb (Space indentSize) = mconcat $ replicate indentSize showbSpace