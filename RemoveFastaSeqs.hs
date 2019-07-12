#!/usr/bin/env stack
-- stack --resolver lts-13.5 script --optimize
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
import qualified Data.ByteString as B
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit as C
import           Data.Conduit ((.|))
import qualified Data.Set as S
import           Data.List (foldl')
import           Safe (atDef)
import           Control.DeepSeq ()
import           Control.Monad (when)
import           System.IO.SafeWrite (withOutputFile)
import System.Environment
import System.Console.GetOpt

import Data.BioConduit

readCopies :: FilePath -> IO (S.Set B.ByteString)
readCopies cfname =
    C.runConduitRes $
        C.sourceFile cfname
        .| C.lines
        .| CL.fold (flip S.insert) S.empty


data CmdFlags = OutputFile FilePath
                | InputFile FilePath
                | DeletionFile FilePath
                | Verbose
                deriving (Eq, Show)

options :: [OptDescr CmdFlags]
options =
    [ Option ['v'] ["verbose"] (NoArg Verbose)  "verbose mode"
    , Option ['i'] ["input"] (ReqArg InputFile "FILE") "Input file to check"
    , Option ['o'] ["output"] (ReqArg OutputFile "FILE") "Output file"
    , Option ['d'] ["to-delete"] (ReqArg DeletionFile "FILE") "Sequences to remove"
    ]


data CmdArgs = CmdArgs
                    { ifile :: FilePath
                    , ofile :: FilePath
                    , delfile :: FilePath
                    , verboseArg :: Bool
                    } deriving (Eq, Show)

parseArgs :: [String] -> CmdArgs
parseArgs argv = foldl' p (CmdArgs iarg oarg delarg False) flags
    where
        (flags, args, _extraOpts) = getOpt Permute options argv
        iarg = atDef "" args 0
        oarg = atDef "" args 1
        delarg = atDef "" args 2
        p c Verbose = c { verboseArg = True }
        p c (OutputFile o) = c { ofile = o }
        p c (InputFile i) = c { ifile = i }
        p c (DeletionFile i) = c { delfile = i }

main :: IO ()
main = do
    CmdArgs fna ofna del verbose <- parseArgs <$> getArgs
    toRemove <- readCopies del
    when verbose $
        putStrLn ("Will remove " ++ show (S.size toRemove) ++ " sequences.")
    withOutputFile ofna $ \hout ->
        C.runConduitRes $
            C.sourceFile fna
                .| faConduit
                .| CL.filter (flip S.notMember toRemove . seqheader)
                .| faWriteC
                .| CC.sinkHandleBuilder hout
