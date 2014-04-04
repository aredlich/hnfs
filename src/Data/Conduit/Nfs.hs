{-|
Module      : Data.Conduit.Nfs
Description : Helpers for using hnfs with conduits
Copyright   : (c) 2014 Arne Redlich <arne.redlich@googlemail.com>
License     : LGPL v2.1
Maintainer  : Arne Redlich <arne.redlich@googlemail.com>
Stability   : experimental
Portability : POSIX

Conduit helpers for hnfs. Heavily inspired by Data.Conduit.Binary.
-}

{-# LANGUAGE RankNTypes #-}

module Data.Conduit.Nfs ( sinkFh
                        , sinkFile
                        , sourceDirectory
                        , sourceFh
                        , sourceFile
                        , sourceFileRange
                        ) where

import Control.Exception (assert)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS

import Data.Conduit

import Foreign.C.Types (CSize)

import System.FilePath ((</>))
import qualified System.Nfs as Nfs
import System.Posix.Types (FileOffset)
import System.IO (SeekMode (..))
import System.Posix.IO (OpenMode (..))


-- | Stream the contents of the given directory on an NFS export,
-- without entering subdirectories.
--
-- The generated paths are absolute paths (rooted at the NFS export).
sourceDirectory :: MonadResource m =>
                   Nfs.Context ->
                   FilePath ->
                   Producer m FilePath
sourceDirectory ctx path = bracketP (opendir ctx path) Nfs.closeDir readdir
  where
    opendir :: Nfs.Context -> FilePath -> IO Nfs.Dir
    opendir ctx path = do
      ret <- Nfs.openDir ctx path
      case ret of
        Left s -> fail s
        Right dir -> return dir
    readdir :: MonadResource m => Nfs.Dir -> Producer m FilePath
    readdir dir = do
      mdent <- liftIO $ Nfs.readDir dir
      case mdent of
        Nothing -> return ()
        Just dent -> do
          unless (is_dot_dir dent) $
            yield $ path </> Nfs.direntName dent
          readdir dir
    is_dot_dir :: Nfs.Dirent -> Bool
    is_dot_dir dent' = (Nfs.direntName dent' == "." ||
                        Nfs.direntName dent' == "..") &&
                       Nfs.direntFType3 dent' == Nfs.NF3Dir

open_file :: Nfs.Context -> FilePath -> OpenMode -> IO Nfs.Fh
open_file ctx path mode = do
  ret <- Nfs.open ctx path mode
  case ret of
    Left s -> fail s
    Right fh -> return fh

default_read_chunk_size :: CSize
default_read_chunk_size = 4096

read_fh :: Nfs.Fh -> IO BS.ByteString
read_fh fh = do
  ret <- Nfs.read fh default_read_chunk_size
  case ret of
    Left s -> fail s
    Right bs -> return bs

-- | Stream the contents of an @Fh@ (NFS file handle) as binary data.
-- The @Fh@ handle will not be closed.
sourceFh :: MonadResource m => Nfs.Fh -> Producer m BS.ByteString
sourceFh fh = do
  bs <- liftIO $ read_fh fh
  unless (BS.null bs) $ do
    yield bs
    sourceFh fh

-- | Stream the contents of a file on an NFS export as binary data.
sourceFile :: MonadResource m => Nfs.Context -> FilePath -> Producer m BS.ByteString
sourceFile ctx path = bracketP (open_file ctx path ReadOnly) Nfs.closeFh sourceFh

lseek :: Nfs.Fh -> FileOffset -> IO ()
lseek fh off = do
  res <- Nfs.lseek fh off AbsoluteSeek
  case res of
    Left s -> fail s
    Right pos -> assert (off == pos) $ return ()

-- | Stream the contents of an @Fh@ (NFS file handle) as binary data, optionally
-- specifying the starting offset and the number of bytes to read.
-- The @Fh@ handle will not be closed.
sourceFhRange :: MonadResource m =>
                 Nfs.Fh ->
                 Maybe FileOffset ->
                 Maybe Integer ->
                 Producer m BS.ByteString
sourceFhRange fh moff msize = do
  case moff of
    Nothing -> return ()
    Just off -> liftIO $ lseek fh off
  case msize of
    Nothing -> sourceFh fh
    Just size -> read_fh' fh size
  where
    read_fh' :: MonadResource m => Nfs.Fh -> Integer -> Producer m BS.ByteString
    read_fh' fh' sz = do
      ret <- liftIO $ Nfs.read fh (min default_read_chunk_size $ fromIntegral sz)
      case ret of
        Left s -> fail s
        Right bs -> unless (BS.null bs) $ do
          yield bs
          read_fh' fh $ sz - (fromIntegral . BS.length) bs
      return ()

-- | Stream the contents of a file on an NFS export as binary data, optionally
-- specifying the starting offset and the number of bytes to read.
sourceFileRange :: MonadResource m =>
                   Nfs.Context ->
                   FilePath ->
                   Maybe FileOffset ->
                   Maybe Integer ->
                   Producer m BS.ByteString
sourceFileRange ctx path off size = bracketP
                                    (open_file ctx path ReadOnly)
                                    Nfs.closeFh
                                    (\fh -> sourceFhRange fh off size)

write_fh :: Nfs.Fh -> BS.ByteString -> IO ()
write_fh fh bs = do
  ret <- Nfs.write fh bs
  case ret of
    Left s -> fail s
    Right size -> do
      let sz = (fromIntegral.BS.length) bs
      assert (sz == size) $ return ()

-- | Stream incoming binary data to @Fh@ (NFS file handle).
-- The @Fh@ handle will not be closed.
sinkFh :: MonadResource m => Nfs.Fh -> Consumer BS.ByteString m ()
sinkFh fh = awaitForever $ liftIO . write_fh fh

-- | Stream incoming binary data to a file on an NFS export.
sinkFile :: MonadResource m => Nfs.Context -> FilePath -> Consumer BS.ByteString m ()
sinkFile ctx path = bracketP (open_file ctx path WriteOnly) Nfs.closeFh sinkFh

-- Local Variables: **
-- mode: haskell **
-- compile-command: "cd ../../.. && cabal install -v" **
-- End: **
