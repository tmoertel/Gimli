{-# OPTIONS -fglasgow-exts #-}

module LoadData (
    loadCsvTable, loadWsvTable, loadTsvTable
)
where

import Control.Monad.Error
import Data.Either
import Data.List (transpose)
import Text.ParserCombinators.Parsec (parseFromFile)

import CSV.Parser
import WSV.Parser
import TSV.Parser
import Parser
import Table

loadCsvTable path =
    loadTable csvFile path

loadWsvTable path =
    loadTable wsvFile path

loadTsvTable path =
    loadTable tsvFile path

loadTable parser path header tr = do
    matrix <- liftM (if tr then transpose else id) $ loadFile parser path
    gimlParseTable $ (if header then id else addHeader) matrix

loadFile parser path =
    liftIO (parseFromFile parser path) >>=
    either (throwError . show) return

addHeader []          = []
addHeader rows@(xs:_) = header : rows
  where
    header = [ "C" ++ show n | n <- [1 .. length xs] ]
