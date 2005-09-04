{-# OPTIONS -fglasgow-exts #-}

module LoadData (
    loadCsvTable, loadWsvTable
)
where

import Control.Monad.Error
import Data.Either
import Text.ParserCombinators.Parsec (parseFromFile)

import CSV.Parser
import WSV.Parser
import Parser
import Value

loadCsvTable :: (MonadIO m, MonadError String m) => String -> m Table
loadCsvTable path =
    loadTable csvFile path

loadWsvTable :: (MonadIO m, MonadError String m) => String -> m Table
loadWsvTable path =
    loadTable wsvFile path

loadTable parser path =
    loadFile parser path >>= gimlParseTable

loadFile parser path =
    liftIO (parseFromFile parser path) >>=
    either (throwError . show) return
