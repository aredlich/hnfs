{-|
Module      : AdvancedTests
Description : hnfs-tester - Nfs (client library) test tool
Copyright   : (c) 2014 Arne Redlich <arne.redlich@googlemail.com>
License     : LGPL v2.1
Maintainer  : Arne Redlich <arne.redlich@googlemail.com>
Stability   : experimental
Portability : POSIX

Advanced hnfs tests.
-}

module AdvancedTests ( tests ) where

import Base

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.UUID hiding (null)
import Data.UUID.V4

import System.IO (SeekMode (..))
import qualified System.Nfs as Nfs
import System.Posix.IO (OpenMode (..))

import Test.Tasty.HUnit as HU

list_directory :: FilePath -> ReaderT TestContext IO (Either String [Nfs.Dirent])
list_directory path = do
  tctx <- ask
  let ctx = ctxContext tctx
      nfs = ctxSyncNfs tctx
  runEitherT $ do
    ret <- liftIO $ syncOpenDir nfs ctx path
    dir <- case ret of
      Left s -> left s
      Right d -> return d
    dents <- liftIO $ readdir dir []
    liftIO $ Nfs.closeDir dir
    right dents
  where
    readdir :: Nfs.Dir -> [Nfs.Dirent] -> IO [Nfs.Dirent]
    readdir dir' dirents = do
      mdirent <- Nfs.readDir dir'
      case mdirent of
        Just dirent -> readdir dir' $ dirent : dirents
        Nothing -> return dirents

test_list_empty_directory :: ReaderT TestContext IO ()
test_list_empty_directory = withDirectory "/" $ \path -> do
  ret <- list_directory path
  case ret of
    Left s -> liftIO $ HU.assertFailure $
              "failed to read dir " ++ path ++ ": " ++ s
    Right dents -> do
      liftIO $ HU.assertEqual "empty dir must yield 2 dirents" 2 $ length dents
      liftIO $ HU.assertBool "dirents must be '.' and '..'" $ null $ filter (not.is_dot_dir) dents
  where
    is_dot_dir dent = (Nfs.direntName dent == "." ||
                       Nfs.direntName dent == "..") &&
                      Nfs.direntFType3 dent == Nfs.NF3Dir

test_create_and_remove_directory :: ReaderT TestContext IO ()
test_create_and_remove_directory = withDirectory "/" $ \path -> do
  tctx <- ask
  let nfs = ctxSyncNfs tctx
      ctx = ctxContext tctx
  ret <- liftIO $ syncStat nfs ctx path
  case ret of
    Left s -> liftIO $ HU.assertFailure $ "failed to stat " ++ path ++ ": " ++ s
    Right stat -> liftIO $
                  HU.assertBool "stat should indicate it's a directory" $
                  Nfs.isDirectory stat

test_create_and_remove_file :: ReaderT TestContext IO ()
test_create_and_remove_file = withFile $ \_ fpath -> do
  tctx <- ask
  let nfs = ctxSyncNfs tctx
      ctx = ctxContext tctx
  ret <- liftIO $ syncStat nfs ctx fpath
  case ret of
    Left s -> liftIO $ HU.assertFailure $
              "failed to stat " ++ fpath ++ ": " ++ s
    Right st -> do
      liftIO $ HU.assertBool "stat should indicate it's a file" $
        Nfs.isRegularFile st
      liftIO $ HU.assertEqual "file size should be 0" 0 $ Nfs.statSize st

test_write_and_read_file :: ReaderT TestContext IO ()
test_write_and_read_file =
  let pattern = BSC8.pack "of no particular importance"
  in withFile $ \_ fpath -> do
    withFh fpath WriteOnly $ \fh -> do
      checkPos fh 0 "before write"
      checkWrite fh pattern
    withFh fpath ReadOnly $ \fh -> do
      checkPos fh 0 "before read"
      checkRead fh pattern

test_truncate_and_stat :: ReaderT TestContext IO ()
test_truncate_and_stat =
  let tsize = 12345
      assertion nfs ctx fpath = do
        ret <- liftIO $ syncTruncate nfs ctx fpath $ fromIntegral tsize
        case ret of
          Left s -> left $ "truncate failed: " ++ s
          Right _ -> right ()
        stret <- liftIO $ syncStat nfs ctx fpath
        st <- case stret of
          Left s -> left $ "fstat failed: " ++ s
          Right st' -> right st'
        liftIO $ HU.assertBool "stat should indicate it's a file" $
          Nfs.isRegularFile st
        liftIO $ HU.assertEqual "file size should match truncated size"
          tsize $ Nfs.statSize st
        right ()
  in withFile $ \_ fpath -> do
      tctx <- ask
      let nfs = ctxSyncNfs tctx
          ctx = ctxContext tctx
      res <- runEitherT $ assertion nfs ctx fpath
      case res of
        Left s -> liftIO $ HU.assertFailure s
        Right () -> return ()

test_ftruncate_and_fstat :: ReaderT TestContext IO ()
test_ftruncate_and_fstat =
  let tsize = 67890
      assertion nfs ctx fh = do
        ret <- liftIO $ syncFTruncate nfs ctx fh $ fromIntegral tsize
        case ret of
          Left s -> left $ "ftruncate failed: " ++ s
          Right _ -> right ()
        stret <- liftIO $ syncFStat nfs ctx fh
        st <- case stret of
          Left s -> left $ "fstat failed: " ++ s
          Right st' -> right st'
        liftIO $ HU.assertBool "stat should indicate it's a file" $
          Nfs.isRegularFile st
        liftIO $ HU.assertEqual "file size should match truncated size"
          tsize $ Nfs.statSize st
        right ()
  in withFile $ \ _ fpath ->
    withFh fpath WriteOnly $ \fh -> do
      tctx <- ask
      let nfs = ctxSyncNfs tctx
          ctx = ctxContext tctx
      res <- runEitherT $ assertion nfs ctx fh
      case res of
        Left s -> liftIO $ HU.assertFailure s
        Right () -> return ()

test_ftruncate_and_lseek :: ReaderT TestContext IO ()
test_ftruncate_and_lseek =
  let tsize = 13579
      assertion nfs ctx fh = do
        ret <- liftIO $ syncFTruncate nfs ctx fh $ fromIntegral tsize
        case ret of
          Left s -> left $ "ftruncate failed: " ++ s
          Right _ -> right ()
        lift $ checkPos fh 0 "figure out file pos"
        sret <- liftIO $ syncLSeek nfs ctx fh 0 SeekFromEnd
        case sret of
          Left s -> left $ "seek 0 from end failed: " ++ s
          Right pos -> liftIO $ HU.assertEqual "expect file pos end"
                       tsize $ fromIntegral pos
        sret <- liftIO $ syncLSeek nfs ctx fh 1 AbsoluteSeek
        case sret of
          Left s -> left $ "absolute seek to pos 1 failed: " ++ s
          Right pos -> liftIO $ HU.assertEqual "expect file pos 1"
                       1 $ fromIntegral pos
        sret <- liftIO $ syncLSeek nfs ctx fh (-1) RelativeSeek
        case sret of
          Left s -> left $ "relative seek -1 failed: " ++ s
          Right pos -> liftIO $ HU.assertEqual "expect file pos 0"
                       0 $ fromIntegral pos
        sret <- liftIO $ syncLSeek nfs ctx fh (-1) RelativeSeek
        case sret of
          Left _ -> right ()
          Right pos -> left $ "could do a relative seek to pos " ++ show pos
        sret <- liftIO $ syncLSeek nfs ctx fh (-(tsize + 1)) SeekFromEnd
        case sret of
          Left _ -> right ()
          Right pos -> left $ "could seek to -(tsize + 1) from end to pos " ++ show pos
  in withFile $ \_ fpath ->
    withFh fpath WriteOnly $ \fh -> do
      tctx <- ask
      let nfs = ctxSyncNfs tctx
          ctx = ctxContext tctx
      res <- runEitherT $ assertion nfs ctx fh
      case res of
        Left s -> liftIO $ HU.assertFailure s
        Right () -> return ()

test_ftruncate_pwrite_and_pread_file :: ReaderT TestContext IO ()
test_ftruncate_pwrite_and_pread_file =
  let pattern = BSC8.pack "not of any importance either"
      offset = 42
      tsize = offset + (BS.length pattern)
      assertion nfs ctx fh = do
        ret <- liftIO $ syncFTruncate nfs ctx fh $ fromIntegral tsize
        case ret of
          Left s -> left $ "ftruncate failed: " ++ s
          Right _ -> right ()
        lift $ checkPWrite fh (fromIntegral offset) pattern
        lift $ checkPRead fh (fromIntegral offset) pattern
        right ()
  in withFile $ \_ fpath ->
    withFh fpath ReadWrite $ \fh -> do
      tctx <- ask
      let nfs = ctxSyncNfs tctx
          ctx = ctxContext tctx
      res <- runEitherT $ assertion nfs ctx fh
      case res of
        Left s -> liftIO $ HU.assertFailure s
        Right _ -> return ()

test_mount_wrong_server :: ReaderT TestContext IO ()
test_mount_wrong_server = do
  tctx <- ask
  let ctx = ctxContext tctx
      nfs = ctxSyncNfs tctx
  uuid <- return.toString =<< liftIO nextRandom
  ret <- local (\_ -> TestContext nfs ctx uuid uuid) mount
  case ret of
    Left _ -> return ()
    Right _ -> liftIO $ HU.assertFailure $
               "mounting " ++ uuid ++ ":" ++ uuid ++ " succeeded unexpectedly"

test_mount_wrong_export :: ReaderT TestContext IO ()
test_mount_wrong_export = do
  tctx <- ask
  let ctx = ctxContext tctx
      srv = ctxServer tctx
      nfs = ctxSyncNfs tctx
  uuid <- return.toString =<< liftIO nextRandom
  ret <- local (\_ -> TestContext nfs ctx srv uuid) mount
  case ret of
    Left _ -> return ()
    Right _ -> liftIO $ HU.assertFailure $
               "mounting " ++ srv ++ ":" ++ uuid ++ " succeeded unexpectedly"

test_mount_ok :: ReaderT TestContext IO ()
test_mount_ok = do
  tctx <- ask
  ret <- mount
  case ret of
    Left s -> liftIO $ HU.assertFailure $
              "mounting " ++ ctxServer tctx ++ ":" ++ ctxExport tctx ++
              " failed: " ++ s
    Right _ -> return ()

test_get_fd_mounted :: ReaderT TestContext IO ()
test_get_fd_mounted = withMount $ do
  tctx <- ask
  let ctx = ctxContext tctx
  fd <- liftIO $ Nfs.getFd ctx
  liftIO $ HU.assertBool "didn't get an FD back" $ fd >= 0

tests :: [ (ReaderT TestContext IO (), String) ]
tests = [ (test_get_fd_mounted, "get fd from context when mounted")
        , (test_mount_ok, "mount correct server and export")
        , (test_mount_wrong_export, "mount correct server, wrong export")
        , (test_mount_wrong_server, "mount wrong server")
        , (test_create_and_remove_directory, "create and remove directory")
        , (test_list_empty_directory, "list empty directory")
        , (test_create_and_remove_file, "create and remove file")
        , (test_write_and_read_file, "write and read file")
        , (test_truncate_and_stat, "truncate and stat")
        , (test_ftruncate_and_fstat, "ftruncate and fstat")
        , (test_ftruncate_and_lseek, "ftruncate and lseek")
        , (test_ftruncate_pwrite_and_pread_file, "pwrite and pread") ]
