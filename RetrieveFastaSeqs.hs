#!/usr/bin/env stack
-- stack --resolver lts-13.5 script --optimize
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit as C
import           Data.Conduit ((.|))
import qualified Data.Set as S
import           Data.List (foldl')
import           System.Console.GetOpt
import           System.IO.SafeWrite (withOutputFile)
import Control.Monad
import System.Environment


import Data.BioConduit

data CmdArgs = CmdArgs
    { ifilesArg :: [FilePath]
    , fileListArg :: FilePath
    , keepArg :: FilePath
    , ofileArg :: FilePath
    , verboseArg :: Bool
    } deriving (Show)

parseArgs :: [String] -> CmdArgs
parseArgs argv = foldl' (flip ($)) (CmdArgs [] "" "" "" False) flags
    where
        flags :: [CmdArgs -> CmdArgs]
        (flags, _, _extraOpts) = getOpt Permute options argv
        options =
            [ Option ['o'] ["output"] (ReqArg (\f c -> c { ofileArg = f }) "FILE") "Output file"
            , Option ['k'] ["keep"] (ReqArg (\f c -> c { keepArg = f }) "FILE") "Input file to check"
            , Option ['F'] ["file-list"] (ReqArg (\f c -> c { fileListArg = f }) "FILE") "List of input files"
            , Option ['i'] ["input"] (ReqArg (\f c -> c { ifilesArg = (f:ifilesArg c) }) "FILE") "Input file to check"
            , Option ['v'] ["verbose"] (NoArg (\c -> c {verboseArg = True }))  "Verbose"
            ]


readHeaderList :: FilePath -> IO (S.Set B.ByteString)
readHeaderList cfname =
    C.runConduitRes $
        C.sourceFile cfname
        .| C.lines
        .| CL.fold (flip S.insert) S.empty

main :: IO ()
main = do
    opts <- parseArgs <$> getArgs
    fileList <- case (ifilesArg opts, fileListArg opts) of
        ([], "") -> error "Both -i and -F are empty"
        ([], fl) -> C.runConduitRes $
                        C.sourceFile fl .| C.lines .| CL.map B8.unpack .| CL.consume
        (fs, "") -> return fs
        (_, _) -> error "Both -i and -F contain values"
    toKeep <- readHeaderList (keepArg opts)
    when (verboseArg opts) $
        putStrLn ("Will keep " ++ show (S.size toKeep) ++ " sequences.")

    withOutputFile (ofileArg opts) $ \hout ->
        C.runConduitRes $
            (sequence_ [C.sourceFile f | f <- fileList])
                .| faConduit
                .| C.filter (flip S.member toKeep . seqheader)
                .| faWriteC
                .| CC.sinkHandleBuilder hout
