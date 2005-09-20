{-# OPTIONS -fglasgow-exts #-}

module LoadData (
    loadCsvTable, loadWsvTable, loadTsvTable
)
where

import Control.Monad.Error
import Data.Either
import Text.ParserCombinators.Parsec (parseFromFile)

import CSV.Parser
import WSV.Parser
import TSV.Parser
import Parser
import Table

loadCsvTable :: (MonadIO m, MonadError String m) => String -> m Table
loadCsvTable path =
    loadTable csvFile path

loadWsvTable :: (MonadIO m, MonadError String m) => String -> m Table
loadWsvTable path =
    loadTable wsvFile path

loadTsvTable :: (MonadIO m, MonadError String m) => String -> m Table
loadTsvTable path =
    loadTable tsvFile path

loadTable parser path =
    loadFile parser path >>= gimlParseTable

loadFile parser path =
    liftIO (parseFromFile parser path) >>=
    either (throwError . show) return
