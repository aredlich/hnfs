-- Main module of nfs-tester
--
-- Copyright (C) 2014 Arne Redlich <arne.redlich@googlemail.com>
--
-- Licensed under the LGPL v2.1 - see the LICENSE file for details.

{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Concurrent.MVar
import Control.Exception
import qualified Control.Exception.Lifted as LE
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.Conduit
import qualified Data.Conduit.List as ListC
import qualified Data.Conduit.Nfs as NfsC
import Data.Monoid
import Data.Typeable (Typeable)
import Data.Proxy
import Data.UUID hiding (null)
import Data.UUID.V4

import Foreign.C.Types

import qualified GHC.Event as Ev

import System.FilePath.Posix
import System.IO
import qualified System.Nfs as Nfs
import System.Posix.IO (OpenMode (..))
import System.Posix.Types

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.HUnit as HU

nfs_to_ev_event :: Nfs.Event -> Ev.Event
nfs_to_ev_event ev
  | ev == Nfs.eventRead `mappend` Nfs.eventWrite = Ev.evtRead `mappend` Ev.evtWrite
  | ev == Nfs.eventRead = Ev.evtRead
  | ev == Nfs.eventWrite = Ev.evtWrite
  | otherwise = undefined

ev_to_nfs_event :: Ev.Event -> Nfs.Event
ev_to_nfs_event ev
  | ev == Ev.evtRead `mappend` Ev.evtWrite = Nfs.eventRead `mappend` Nfs.eventWrite
  | ev == Ev.evtRead = Nfs.eventRead
  | ev == Ev.evtWrite = Nfs.eventWrite
  | otherwise = undefined

-- Retrieving and registering the context's fd once is *not* sufficient -
-- we need to constantly re-register it as libnfs closes / reopens it e.g.
-- during mount.
ev_callback :: Nfs.Context -> Ev.EventManager-> Ev.FdKey -> Ev.Event -> IO ()
ev_callback ctx mgr fdkey ev = do
  Ev.unregisterFd mgr fdkey
  Nfs.service ctx $ ev_to_nfs_event ev
  register_fd ctx mgr

register_fd :: Nfs.Context -> Ev.EventManager -> IO ()
register_fd ctx mgr = do
  fd <- Nfs.getFd ctx
  evts <- Nfs.whichEvents ctx
  _ <- Ev.registerFd mgr (ev_callback ctx mgr) fd $ nfs_to_ev_event evts
  return ()

-- This spews "ioManagerDie: write: Bad file descriptor" -
-- https://ghc.haskell.org/trac/ghc/ticket/5443
sync_wrap :: Nfs.Context ->
             (Nfs.Callback a -> IO (Either String ())) ->
             IO (Either String a)
sync_wrap ctx async_action = runEitherT $ do
  mgr <- liftIO Ev.new
  mv <- liftIO newEmptyMVar
  ret <- liftIO $ async_action $ callback mv mgr
  case ret of
    Left s -> left $ "failed to invoke async action: " ++ s
    Right () -> right ()
  liftIO $ register_fd ctx mgr
  liftIO $ Ev.loop mgr
  (liftIO $ takeMVar mv) >>= hoistEither
  where
    callback :: MVar (Either String a) ->
                Ev.EventManager ->
                Either String a -> IO ()
    callback mv' mgr' ret' = do
      putMVar mv' ret'
      Ev.shutdown mgr'

data SyncNfs = SyncNfs { syncMount :: Nfs.Context ->
                                      Nfs.ServerAddress ->
                                      Nfs.ExportName ->
                                      IO (Either String ())
                       , syncOpenDir :: Nfs.Context ->
                                        FilePath ->
                                        IO (Either String Nfs.Dir)
                       , syncMkDir :: Nfs.Context ->
                                      FilePath ->
                                      IO (Either String ())
                       , syncRmDir :: Nfs.Context ->
                                      FilePath ->
                                      IO (Either String ())
                       , syncStat :: Nfs.Context ->
                                     FilePath ->
                                     IO (Either String Nfs.Stat)
                       , syncFStat :: Nfs.Context ->
                                     Nfs.Fh ->
                                     IO (Either String Nfs.Stat)
                       , syncOpen :: Nfs.Context ->
                                     FilePath ->
                                     OpenMode ->
                                     IO (Either String Nfs.Fh)
                       , syncWrite :: Nfs.Context ->
                                      Nfs.Fh ->
                                      BS.ByteString ->
                                      IO (Either String CSize)
                       , syncPWrite :: Nfs.Context ->
                                       Nfs.Fh ->
                                       BS.ByteString ->
                                       FileOffset ->
                                       IO (Either String CSize)
                       , syncRead :: Nfs.Context ->
                                     Nfs.Fh ->
                                     CSize ->
                                     IO (Either String BS.ByteString)
                       , syncPRead :: Nfs.Context ->
                                      Nfs.Fh ->
                                      CSize ->
                                      FileOffset ->
                                      IO (Either String BS.ByteString)
                       , syncCreat :: Nfs.Context ->
                                      FilePath ->
                                      OpenMode ->
                                      IO (Either String Nfs.Fh)
                       , syncUnlink :: Nfs.Context ->
                                       FilePath ->
                                       IO (Either String ())
                       , syncTruncate :: Nfs.Context ->
                                         FilePath ->
                                         FileOffset ->
                                         IO (Either String ())
                       , syncFTruncate :: Nfs.Context ->
                                          Nfs.Fh ->
                                          FileOffset ->
                                          IO (Either String ())
                       , syncLSeek :: Nfs.Context ->
                                      Nfs.Fh ->
                                      FileOffset ->
                                      SeekMode ->
                                      IO (Either String FileOffset)
                       }

data TestContext = TestContext { ctxSyncNfs :: SyncNfs
                               , ctxContext :: Nfs.Context
                               , ctxServer :: Nfs.ServerAddress
                               , ctxExport :: Nfs.ExportName }

with_context :: (Nfs.Context -> IO ()) -> IO ()
with_context = bracket Nfs.initContext Nfs.destroyContext

mount :: ReaderT TestContext IO (Either String ())
mount = do
  tctx <- ask
  let ctx = ctxContext tctx
  liftIO $ Nfs.setUid ctx 0
  liftIO $ Nfs.setUid ctx 0
  liftIO $ syncMount (ctxSyncNfs tctx) ctx (ctxServer tctx) (ctxExport tctx)

with_mount :: ReaderT TestContext IO () -> ReaderT TestContext IO ()
with_mount act = do
  tctx <- ask
  ret <- mount
  case ret of
    Left s -> liftIO $ HU.assertFailure $
              "mounting " ++ (ctxServer tctx) ++ ":" ++ (ctxExport tctx) ++
              " failed: " ++ s
    Right () -> act

mk_random_path :: FilePath -> IO FilePath
mk_random_path parent = do
  uuid <- return.toString =<< nextRandom
  return $ joinPath [ parent, uuid ]

with_directory :: FilePath ->
                  (FilePath -> ReaderT TestContext IO ()) ->
                  ReaderT TestContext IO ()
with_directory parent action = LE.bracket mkdir rmdir action
  where
    mkdir = do
      tctx <- ask
      let ctx = ctxContext tctx
          nfs = ctxSyncNfs tctx
      path <- liftIO $ mk_random_path parent
      ret <- liftIO $ syncMkDir nfs ctx path
      case ret of
        Left s -> fail $ "failed to create directory: " ++ s
        Right () -> return path
    rmdir path = do
      tctx <- ask
      let ctx = ctxContext tctx
          nfs = ctxSyncNfs tctx
      ret <- liftIO $ syncRmDir nfs ctx path
      case ret of
        -- XXX: does this override the original assertion?
        Left s -> liftIO $ HU.assertFailure $ "failed to remove path: " ++ s
        Right () -> return ()

with_directory' :: FilePath ->
                  (FilePath -> ReaderT TestContext IO ()) ->
                  ReaderT TestContext IO ()
with_directory' parent action = with_mount $ with_directory parent action

with_file :: FilePath ->
             (FilePath -> ReaderT TestContext IO ()) ->
             ReaderT TestContext IO ()
with_file parent action = LE.bracket mkfile rmfile action
  where
    mkfile = do
      tctx <- ask
      let ctx = ctxContext tctx
          nfs = ctxSyncNfs tctx
      path <- liftIO $ mk_random_path parent
      ret <- liftIO $ syncCreat nfs ctx path WriteOnly
      case ret of
        Left s -> fail $ "failed to create file: " ++ s
        Right fh -> liftIO $ Nfs.closeFh fh >> return path
    rmfile path = do
      tctx <- ask
      let ctx = ctxContext tctx
          nfs = ctxSyncNfs tctx
      ret <- liftIO $ syncUnlink nfs ctx path
      case ret of
        Left s -> fail $ "failed to unlink file " ++ path ++ ": " ++ s
        Right () -> return ()

with_file' :: (FilePath -> FilePath -> ReaderT TestContext IO ()) ->
              ReaderT TestContext IO ()
with_file' act =
  with_directory' "/" $ \dpath -> with_file dpath $ \fpath -> act dpath fpath

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
test_list_empty_directory = with_directory' "/" $ \path -> do
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
test_create_and_remove_directory = with_directory' "/" $ \path -> do
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
test_create_and_remove_file = with_file' $ \_ fpath -> do
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

with_fh :: FilePath ->
           OpenMode ->
           (Nfs.Fh -> ReaderT TestContext IO ()) ->
           ReaderT TestContext IO ()
with_fh path mode action = LE.bracket open close action
  where
    open = do
      tctx <- ask
      let nfs = ctxSyncNfs tctx
          ctx = ctxContext tctx
      ret <- liftIO $ syncOpen nfs ctx path mode
      case ret of
        Left s -> fail $ "failed to open " ++ path ++ ": " ++ s
        Right fh -> return fh
    close = liftIO.Nfs.closeFh

check_pos :: Nfs.Fh -> FileOffset -> String -> ReaderT TestContext IO ()
check_pos fh expected desc = do
  tctx <- ask
  let nfs = ctxSyncNfs tctx
      ctx = ctxContext tctx
  sret <- liftIO $ syncLSeek nfs ctx fh 0 RelativeSeek
  case sret of
    Left s -> liftIO $ HU.assertFailure $
              desc ++ ": failed to lseek to relative pos 0: " ++ s
    Right pos -> liftIO $ HU.assertEqual
                 (desc ++ ": expected file position " ++ show expected)
                 expected pos

test_write_and_read_file :: ReaderT TestContext IO ()
test_write_and_read_file =
  let pattern = BSC8.pack "of no particular importance"
  in with_file' $ \_ fpath -> do
    with_fh fpath WriteOnly $ \fh -> do
      check_pos fh 0 "before write"
      check_write fh pattern
    with_fh fpath ReadOnly $ \fh -> do
      check_pos fh 0 "before read"
      check_read fh pattern

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
  in with_file' $ \_ fpath -> do
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
  in with_file' $ \ _ fpath ->
    with_fh fpath WriteOnly $ \fh -> do
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
        lift $ check_pos fh 0 "figure out file pos"
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
  in with_file' $ \_ fpath ->
    with_fh fpath WriteOnly $ \fh -> do
      tctx <- ask
      let nfs = ctxSyncNfs tctx
          ctx = ctxContext tctx
      res <- runEitherT $ assertion nfs ctx fh
      case res of
        Left s -> liftIO $ HU.assertFailure s
        Right () -> return ()

check_pread :: Nfs.Fh ->
               FileOffset ->
               BS.ByteString ->
               ReaderT TestContext IO ()
check_pread fh off pattern = do
  tctx <- ask
  let nfs = ctxSyncNfs tctx
      ctx = ctxContext tctx
  pos <- liftIO $ Nfs.getCurrentOffset fh
  rret <- liftIO $ syncPRead nfs ctx fh (fromIntegral $ BS.length pattern) (fromIntegral off)
  case rret of
    Left s -> liftIO $ HU.assertFailure $ "pread failed: " ++ s
    Right bs -> liftIO $ HU.assertEqual "read data should match expectation" pattern bs
  pos' <- liftIO $ Nfs.getCurrentOffset fh
  liftIO $ HU.assertEqual "file position must not have changed" pos pos'

check_read :: Nfs.Fh ->
              BS.ByteString ->
              ReaderT TestContext IO ()
check_read fh pattern = do
  tctx <- ask
  let nfs = ctxSyncNfs tctx
      ctx = ctxContext tctx
      size = (fromIntegral $ BS.length pattern)
  pos <- liftIO $ Nfs.getCurrentOffset fh
  rret <- liftIO $ syncRead nfs ctx fh size
  case rret of
    Left s -> liftIO $ HU.assertFailure $ "read failed: " ++ s
    Right bs -> liftIO $
                HU.assertEqual "read data should match expectation" pattern bs
  pos' <- liftIO $ Nfs.getCurrentOffset fh
  liftIO $ HU.assertEqual
    "file position must have changed"
    (pos + fromIntegral size) pos'

check_pwrite :: Nfs.Fh ->
                FileOffset ->
                BS.ByteString ->
                ReaderT TestContext IO ()
check_pwrite fh off pattern = do
  tctx <- ask
  let nfs = ctxSyncNfs tctx
      ctx = ctxContext tctx
  pos <- liftIO $ Nfs.getCurrentOffset fh
  wret <- liftIO $ syncPWrite nfs ctx fh pattern (fromIntegral off)
  case wret of
    Left s -> liftIO $ HU.assertFailure $ "pwrite failed: " ++ s
    Right size -> liftIO $ HU.assertEqual
                  "write size should match expectation"
                  (BS.length pattern) (fromIntegral size)
  pos' <- liftIO $ Nfs.getCurrentOffset fh
  liftIO $ HU.assertEqual "file position must not have changed" pos pos'

check_write :: Nfs.Fh ->
               BS.ByteString ->
               ReaderT TestContext IO ()
check_write fh pattern = do
  tctx <- ask
  let nfs = ctxSyncNfs tctx
      ctx = ctxContext tctx
      size = (fromIntegral $ BS.length pattern)
  pos <- liftIO $ Nfs.getCurrentOffset fh
  wret <- liftIO $ syncWrite nfs ctx fh pattern
  case wret of
    Left s -> liftIO $ HU.assertFailure $ "write failed: " ++ s
    Right size' -> liftIO $ HU.assertEqual
                  "write size should match expectation"
                  size size'
  pos' <- liftIO $ Nfs.getCurrentOffset fh
  liftIO $ HU.assertEqual
    "file position must have changed"
    (pos + fromIntegral size) pos'

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
        lift $ check_pwrite fh (fromIntegral offset) pattern
        lift $ check_pread fh (fromIntegral offset) pattern
        right ()
  in with_file' $ \_ fpath ->
    with_fh fpath ReadWrite $ \fh -> do
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

test_init_and_destroy_context :: IO ()
test_init_and_destroy_context = with_context $ \ctx -> do
  err <- Nfs.getError ctx
  HU.assertEqual "initContext failed" Nothing err

-- Does not really verify yet that the context is garbage collected.
test_garbage_collect_context :: IO ()
test_garbage_collect_context = Nfs.initContext >> return ()

test_destroy_context_twice :: IO ()
test_destroy_context_twice = do
  ctx <- Nfs.initContext
  Nfs.destroyContext ctx
  Nfs.destroyContext ctx

test_get_fd :: IO ()
test_get_fd = with_context $ \ctx -> do
  fd <- Nfs.getFd ctx
  HU.assertBool "got an fd without mounting" (fd < 0)

test_get_fd_mounted :: ReaderT TestContext IO ()
test_get_fd_mounted = with_mount $ do
  tctx <- ask
  let ctx = ctxContext tctx
  fd <- liftIO $ Nfs.getFd ctx
  liftIO $ HU.assertBool "didn't get an FD back" $ fd >= 0

test_queue_length :: IO ()
test_queue_length = with_context $ \ctx -> do
  l <- Nfs.queueLength ctx
  HU.assertEqual "unexpected queue length" 0 l

test_get_read_max :: IO ()
test_get_read_max = with_context $ \ctx -> do
  l <- Nfs.getReadMax ctx
  HU.assertEqual "unexpected read max" 0 l

test_get_write_max :: IO ()
test_get_write_max = with_context $ \ctx -> do
  l <- Nfs.getWriteMax ctx
  HU.assertEqual "unexpected write max" 0 l

test_directory_source :: ReaderT TestContext IO ()
test_directory_source =
  with_directory' "/" $ \parent ->
    with_directory parent $ \child_dir ->
      with_file parent $ \child_file -> do
        tctx <- ask
        let ctx = ctxContext tctx
        l <- runResourceT $ NfsC.sourceDirectory ctx parent $$ ListC.consume
        liftIO $ HU.assertEqual "unexpected directory contents" 2 $ length l
        liftIO $ HU.assertBool "child file must be present in output" $
          any (\e -> e == parent </> child_file) l
        liftIO $ HU.assertBool "child dir must be present in output" $
          any (\e -> e == parent </> child_dir) l

test_fh_source_and_sink :: ReaderT TestContext IO ()
test_fh_source_and_sink =
  let pattern = BSC8.pack "some arbitrary, utterly unremarkable text"
  in with_file' $ \dir source ->
    with_fh source ReadWrite $ \fh -> do
      check_pwrite fh 0 pattern
      with_file dir $ \sink ->
        with_fh sink ReadWrite $ \fh2 -> do
          runResourceT $ NfsC.sourceFh fh $$ NfsC.sinkFh fh2
          check_pread fh2 0 pattern

test_file_path_source_and_sink :: ReaderT TestContext IO ()
test_file_path_source_and_sink =
  let pattern = BSC8.pack "some dull string"
  in with_file' $ \dir source ->
    with_fh source ReadWrite $ \fh -> do
      check_pwrite fh 0 pattern
      with_file dir $ \sink -> do
        tctx <- ask
        let ctx = ctxContext tctx
        runResourceT $ NfsC.sourceFile ctx source $$ NfsC.sinkFile ctx sink
        with_fh sink ReadWrite $ \fh2 ->
          check_pread fh2 0 pattern

test_file_path_range_source_and_sink :: ReaderT TestContext IO ()
test_file_path_range_source_and_sink =
  let pattern1 = BSC8.pack "some"
      size1 = BS.length pattern1
      pattern2 = BSC8.pack "pointless"
      size2 = BS.length pattern2
      pattern3 = BSC8.pack "text"
  in with_file' $ \dir source ->
    with_fh source ReadWrite $ \fh -> do
      check_write fh pattern1
      check_write fh pattern2
      check_write fh pattern3
      with_file dir $ \sink1 ->
        with_file dir $ \sink2 ->
          with_file dir $ \sink3 -> do
            tctx <- ask
            let ctx = ctxContext tctx
            runResourceT $
              NfsC.sourceFileRange ctx source Nothing (Just $ fromIntegral size1) $$
              NfsC.sinkFile ctx sink1
            runResourceT $
              NfsC.sourceFileRange ctx source (Just $ fromIntegral size1)
              (Just $ fromIntegral size2) $$ NfsC.sinkFile ctx sink2
            runResourceT $
              NfsC.sourceFileRange ctx source (Just $ fromIntegral (size1 + size2))
              Nothing $$ NfsC.sinkFile ctx sink3
            with_fh sink1 ReadOnly $ \fh' -> check_read fh' pattern1
            with_fh sink2 ReadOnly $ \fh' -> check_read fh' pattern2
            with_fh sink3 ReadOnly $ \fh' -> check_read fh' pattern3

sync_nfs :: SyncNfs
sync_nfs = SyncNfs { syncMount = Nfs.mount
                   , syncOpenDir = Nfs.openDir
                   , syncMkDir = Nfs.mkDir
                   , syncRmDir = Nfs.rmDir
                   , syncStat = Nfs.stat
                   , syncCreat = Nfs.creat
                   , syncTruncate = Nfs.truncate
                   , syncUnlink = Nfs.unlink
                   , syncOpen = Nfs.open
                   , syncRead = \_ fh size -> Nfs.read fh size
                   , syncPRead = \_ fh size off -> Nfs.pread fh size off
                   , syncWrite = \_ fh bs -> Nfs.write fh bs
                   , syncPWrite = \_ fh bs off -> Nfs.pwrite fh bs off
                   , syncFTruncate = \_ fh off -> Nfs.ftruncate fh off
                   , syncFStat = \_ fh -> Nfs.fstat fh
                   , syncLSeek = \_ fh off mode -> Nfs.lseek fh off mode }

async_nfs :: SyncNfs
async_nfs = SyncNfs { syncMount = \ctx addr xprt ->
                       sync_wrap ctx $ Nfs.mountAsync ctx addr xprt
                    , syncOpenDir = \ctx path ->
                       sync_wrap ctx $ Nfs.openDirAsync ctx path
                    , syncMkDir = \ctx path ->
                       sync_wrap ctx $ Nfs.mkDirAsync ctx path
                    , syncRmDir = \ctx path ->
                       sync_wrap ctx $ Nfs.rmDirAsync ctx path
                    , syncStat = \ctx path ->
                       sync_wrap ctx $ Nfs.statAsync ctx path
                    , syncCreat = \ctx path mode ->
                       sync_wrap ctx $ Nfs.creatAsync ctx path mode
                    , syncUnlink = \ctx path ->
                       sync_wrap ctx $ Nfs.unlinkAsync ctx path
                    , syncOpen = \ctx path mode ->
                       sync_wrap ctx $ Nfs.openAsync ctx path mode
                    , syncRead = \ctx fh size ->
                       sync_wrap ctx $ Nfs.readAsync fh size
                    , syncPRead = \ctx fh size off ->
                       sync_wrap ctx $ Nfs.preadAsync fh size off
                    , syncWrite = \ctx fh bs ->
                       sync_wrap ctx $ Nfs.writeAsync fh bs
                    , syncPWrite = \ctx fh bs off ->
                       sync_wrap ctx $ Nfs.pwriteAsync fh bs off
                    , syncTruncate = \ctx path off ->
                       sync_wrap ctx $ Nfs.truncateAsync ctx path off
                    , syncFTruncate = \ctx fh off ->
                       sync_wrap ctx $ Nfs.ftruncateAsync fh off
                    , syncFStat = \ctx fh ->
                       sync_wrap ctx $ Nfs.fstatAsync fh
                    , syncLSeek = \ctx fh off mode ->
                       sync_wrap ctx $ Nfs.lseekAsync fh off mode }

mk_test :: Nfs.ServerAddress ->
           Nfs.ExportName ->
           SyncNfs ->
           (ReaderT TestContext IO (), String) ->
           TestTree
mk_test addr xprt nfs (assertion, desc) =
  HU.testCase desc (with_context $ \ctx ->
                     runReaderT assertion $ TestContext nfs ctx addr xprt)

basic_tests :: TestTree
basic_tests =
  let tests = [ (test_init_and_destroy_context, "init and destroy context")
              , (test_destroy_context_twice, "destroy context twice")
                -- disabled as the test is too weak
                -- , (test_garbage_collect_context, "garbage collect context")
              , (test_get_fd, "get fd from context")
              , (test_queue_length, "get queue length from context")
              , (test_get_read_max, "get read max from context")
              , (test_get_write_max, "get write max from context") ]
  in
   testGroup "Basic tests" $
   fmap (\(assertion, desc) -> HU.testCase desc assertion) $ tests

advanced_tests :: [ (ReaderT TestContext IO (), String) ]
advanced_tests = [ (test_get_fd_mounted, "get fd from context when mounted")
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

sync_tests :: Nfs.ServerAddress -> Nfs.ExportName -> TestTree
sync_tests srv xprt = testGroup "Synchronous interface tests" $
             fmap (\test -> mk_test srv xprt sync_nfs test) $ advanced_tests

async_tests :: Nfs.ServerAddress -> Nfs.ExportName -> TestTree
async_tests srv xprt = testGroup "Asynchronous interface tests" $
              fmap (\test -> mk_test srv xprt async_nfs test) $ advanced_tests

conduit_tests' :: [ (ReaderT TestContext IO (), String) ]
conduit_tests' = [ (test_directory_source, "conduit directory source")
                 , (test_fh_source_and_sink, "conduit, fh source and sink")
                 , (test_file_path_source_and_sink, "conduit file source and sink")
                 , (test_file_path_range_source_and_sink,
                    "conduit file range source and sink") ]

conduit_tests :: Nfs.ServerAddress -> Nfs.ExportName -> TestTree
conduit_tests srv xprt = testGroup "Nfs conduit tests" $
                         fmap (\test -> mk_test srv xprt sync_nfs test) $
                         conduit_tests'

-- TODO: make this fail with a nicer error message if server / export are not
-- specified
newtype ServerAddressOpt = ServerAddressOpt Nfs.ServerAddress
                           deriving (Eq, Show, Ord, Typeable)

instance IsOption ServerAddressOpt where
  parseValue s = Just $ ServerAddressOpt s
  optionName = return "server"
  optionHelp = return "NFS server to connect to"

newtype ExportNameOpt = ExportNameOpt Nfs.ExportName
                      deriving (Eq, Show, Ord, Typeable)

instance IsOption ExportNameOpt where
  parseValue s = Just $ ExportNameOpt s
  optionName = return "export"
  optionHelp = return "NFS export to mount"

main :: IO ()
main = let ings = includingOptions [ Option (Proxy :: Proxy ServerAddressOpt)
                                   , Option (Proxy :: Proxy ExportNameOpt)
                                   ] : defaultIngredients
       in
        defaultMainWithIngredients ings $
        askOption $ \(ServerAddressOpt server) ->
        askOption $ \(ExportNameOpt export) ->
        testGroup "HNfs tests" $ [ basic_tests
                                 , sync_tests server export
                                 , async_tests server export
                                 , conduit_tests server export ]

-- Local Variables: **
-- mode: haskell **
-- compile-command: "cd .. && cabal install -v" **
-- End: **
