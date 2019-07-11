{-# LANGUAGE LambdaCase, OverloadedStrings, BangPatterns #-}
module Data.BioConduit
    ( Fasta(..)
    , faseqLength
    , faConduit
    , faWriteC
    , faWriteConduit
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.Conduit as C
import           Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as CL
import           Data.Word
import           Control.Monad.IO.Class
import           Control.Exception
import           Control.DeepSeq


data Fasta = Fasta
                { seqheader :: !B.ByteString
                , seqdata :: !B.ByteString
                } deriving (Eq, Show)

instance NFData Fasta where
    rnf !_ = ()

faseqLength :: Fasta -> Int
faseqLength = B.length . seqdata

greaterThanSign :: Word8
greaterThanSign = 62

faConduit :: (MonadIO m) => C.ConduitT B.ByteString Fasta m ()
faConduit = C.lines .| faConduit'

faConduit' :: (MonadIO m) => C.ConduitT B.ByteString Fasta m ()
faConduit' = C.await >>= \case
                Nothing -> return ()
                Just header
                  | B.null header -> liftIO $ throwIO (AssertionFailed "Unexpected empty string at line 1")
                  | B.head header == greaterThanSign -> getdata 1 (B.drop 1 header) []
                  | otherwise -> liftIO $ throwIO (AssertionFailed "Unexpected data")
  where
    getdata :: MonadIO m' => Int -> B.ByteString -> [B.ByteString] -> C.ConduitT B.ByteString Fasta m' ()
    getdata !n header toks = C.await >>= \case
                                Nothing -> C.yield $ Fasta header (B.concat $ reverse toks)
                                Just next
                                    | B.null next -> liftIO $ throwIO (AssertionFailed $ "Unexpected empty string at line " ++ show (n+1) ++ " (expected header line).")
                                    | B.head next == greaterThanSign -> do
                                            C.yield $ Fasta header (B.concat $ reverse toks)
                                            getdata (n+1) (B.drop 1 next) []
                                    | otherwise -> getdata (n+1) header (next:toks)

faWriteC :: (Monad m) => C.ConduitT Fasta BB.Builder m ()
faWriteC = CL.map $ \(Fasta h s) -> mconcat [BB.char7 '>', BB.byteString h, BB.char7 '\n', BB.byteString s, BB.char7 '\n']

faWriteConduit :: (Monad m) => [Fasta] -> C.ConduitT () BB.Builder m ()
faWriteConduit fas = C.yieldMany fas .| faWriteC
