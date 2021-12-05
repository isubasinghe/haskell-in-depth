{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module CH5 where

import Control.Monad.Writer
import qualified Data.Text as T

type SQL = T.Text

data ErrorMsg = WrongFormat Int T.Text
  deriving (Show)

genSQL :: T.Text -> Writer [ErrorMsg] SQL
genSQL txt = T.concat <$> traverse processLine (zip [1 ..] $ T.lines txt)

genInsert :: T.Text -> T.Text -> SQL
genInsert s1 s2 =
  "INSERT INTO items VALUES ('" <> s1 <> "','" <> s2 <> ",);\n"

processLine :: (Int, T.Text) -> Writer [ErrorMsg] SQL
processLine (_, T.splitOn ":" -> [s1, s2]) = pure $ genInsert s1 s2
processLine (i, s) = tell [WrongFormat i s] >> pure ""
