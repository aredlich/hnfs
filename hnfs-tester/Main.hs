-- Main module of nfs-tester
--
-- Copyright (C) 2014 Arne Redlich <arne.redlich@googlemail.com>
--
-- Licensed under the LGPL v2.1 - see the LICENSE file for details.

{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
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

with_context :: (Nfs.Context -> IO ()) -> IO ()
with_context = bracket Nfs.initContext Nfs.destroyContext

mount :: SyncNfs ->
         Nfs.Context ->
         Nfs.ServerAddress ->
         Nfs.ExportName ->
         IO (Either String ())
mount nfs ctx srv xprt = do
  Nfs.setUid ctx 0
  Nfs.setGid ctx 0
  syncMount nfs ctx srv xprt

with_mount :: SyncNfs ->
              Nfs.ServerAddress ->
              Nfs.ExportName ->
              (Nfs.Context -> IO ()) ->
              IO ()
with_mount nfs srv xprt action = with_context $ \ctx -> do
  ret <- mount nfs ctx srv xprt
  case ret of
    Left s -> HU.assertFailure $ "mounting " ++ srv ++ ":" ++ xprt ++ " failed: " ++ s
    Right () -> action ctx

mk_random_path :: FilePath -> IO FilePath
mk_random_path parent = do
  uuid <- return.toString =<< nextRandom
  return $ joinPath [ parent, uuid ]

with_directory :: SyncNfs ->
                  Nfs.Context ->
                  FilePath ->
                  (FilePath -> IO ()) ->
                  IO ()
with_directory nfs ctx parent action = bracket mkdir rmdir action
  where
    mkdir :: IO FilePath
    mkdir = do
      path <- mk_random_path parent
      ret <- syncMkDir nfs ctx path
      case ret of
        Left s -> fail $ "failed to create directory: " ++ s
        Right () -> return path

    rmdir :: FilePath -> IO ()
    rmdir path = do
      ret <- syncRmDir nfs ctx path
      case ret of
        -- XXX: does this override the original assertion?
        Left s -> HU.assertFailure $ "failed to remove path: " ++ s
        Right () -> return ()

with_directory' :: SyncNfs ->
                   Nfs.ServerAddress ->
                   Nfs.ExportName ->
                   FilePath ->
                   (Nfs.Context -> FilePath -> IO ()) ->
                   IO ()
with_directory' nfs addr xprt parent action =
  with_mount nfs addr xprt $ \ctx ->
    with_directory nfs ctx parent $ \path ->
      action ctx path

with_file :: SyncNfs ->
             Nfs.Context ->
             FilePath ->
             (FilePath -> IO ()) ->
             IO ()
with_file nfs ctx parent action = bracket mkfile rmfile action
  where
    mkfile :: IO FilePath
    mkfile = do
      path <- mk_random_path parent
      ret <- syncCreat nfs ctx path WriteOnly
      case ret of
        Left s -> fail $ "failed to create file: " ++ s
        Right fh -> Nfs.closeFh fh >> return path

    rmfile :: FilePath -> IO ()
    rmfile path = do
      ret <- syncUnlink nfs ctx path
      case ret of
        Left s -> fail $ "failed to unlink file " ++ path ++ ": " ++ s
        Right () -> return ()

with_file' :: SyncNfs ->
              Nfs.ServerAddress ->
              Nfs.ExportName ->
              (Nfs.Context -> FilePath -> FilePath -> IO ()) ->
              IO ()
with_file' nfs addr xprt act =
  with_directory' nfs addr xprt "/" $ \ctx dpath ->
    with_file nfs ctx dpath $ \fpath -> act ctx dpath fpath

list_directory :: SyncNfs ->
                  Nfs.Context ->
                  FilePath ->
                  IO (Either String [Nfs.Dirent])
list_directory nfs ctx path = runEitherT $ do
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

test_list_empty_directory :: Nfs.ServerAddress ->
                             Nfs.ExportName ->
                             SyncNfs ->
                             TestTree
test_list_empty_directory srv xprt nfs =
  HU.testCase "List contents of empty directory" assertion
    where
      assertion = with_directory' nfs srv xprt "/" $ \ctx path -> do
        ret <- list_directory nfs ctx path
        case ret of
          Left s -> HU.assertFailure $ "failed to read dir " ++ path ++ ": " ++ s
          Right dents -> do
            HU.assertEqual "empty dir must yield 2 dirents" 2 $ length dents
            HU.assertBool "dirents must be '.' and '..'" $ null $
              filter (not.is_dot_dir) dents

      is_dot_dir dent = (Nfs.direntName dent == "." ||
                         Nfs.direntName dent == "..") &&
                        Nfs.direntFType3 dent == Nfs.NF3Dir

test_create_and_remove_directory :: Nfs.ServerAddress ->
                                    Nfs.ExportName ->
                                    SyncNfs ->
                                    TestTree
test_create_and_remove_directory srv xprt nfs =
  let assertion = with_directory' nfs srv xprt "/" $ \ctx path -> do
        ret <- syncStat nfs ctx path
        case ret of
          Left s -> HU.assertFailure $ "failed to stat " ++ path ++ ": " ++ s
          Right stat -> HU.assertBool "stat should indicate it's a directory" $
                        Nfs.isDirectory stat
  in
   HU.testCase "Create and remove directory" assertion

test_create_and_remove_file srv xprt nfs =
  let assertion = with_file' nfs srv xprt $ \ctx _ fpath -> do
          ret <- syncStat nfs ctx fpath
          case ret of
            Left s -> HU.assertFailure $ "failed to stat " ++ fpath ++ ": " ++ s
            Right st -> do
              HU.assertBool "stat should indicate it's a file" $
                Nfs.isRegularFile st
              HU.assertEqual "file size should be 0" 0 $ Nfs.statSize st
  in
   HU.testCase "Create and remove file" assertion

with_fh :: SyncNfs ->
           Nfs.Context ->
           FilePath ->
           OpenMode ->
           (Nfs.Fh -> IO ()) ->
           IO ()
with_fh nfs ctx path mode action = bracket open Nfs.closeFh action
  where
    open :: IO Nfs.Fh
    open = do
      ret <- syncOpen nfs ctx path mode
      case ret of
        Left s -> fail $ "failed to open " ++ path ++ ": " ++ s
        Right fh -> return fh

check_pos :: SyncNfs -> Nfs.Context -> Nfs.Fh -> FileOffset -> String -> IO ()
check_pos nfs ctx fh exp desc = do
  sret <- syncLSeek nfs ctx fh 0 RelativeSeek
  case sret of
    Left s -> HU.assertFailure $ desc ++ ": failed to lseek to relative pos 0: " ++ s
    Right pos -> HU.assertEqual (desc ++ ": expected file position " ++ show exp)
                 exp pos

test_write_and_read_file :: Nfs.ServerAddress ->
                            Nfs.ExportName ->
                            SyncNfs ->
                            TestTree
test_write_and_read_file srv xprt nfs =
  let pattern = BSC8.pack "of no particular importance"
      assertion = with_file' nfs srv xprt $ \ctx _ fpath -> do
        with_fh nfs ctx fpath WriteOnly $ \fh -> do
          check_pos nfs ctx fh 0 "before write"
          check_write nfs ctx fh pattern
        with_fh nfs ctx fpath ReadOnly $ \fh -> do
          check_pos nfs ctx fh 0 "before read"
          check_read nfs ctx fh pattern
  in
   HU.testCase "Write to and read from file" assertion

test_truncate_and_stat :: Nfs.ServerAddress ->
                          Nfs.ExportName ->
                          SyncNfs ->
                          TestTree
test_truncate_and_stat srv xprt nfs =
  let tsize = 12345
      assertion' ctx fpath = do
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
      assertion = with_file' nfs srv xprt $ \ctx _ fpath -> do
        res <- runEitherT $ assertion' ctx fpath
        case res of
          Left s -> HU.assertFailure s
          Right () -> return ()
  in
   HU.testCase "Truncate and stat file" assertion

test_ftruncate_and_fstat :: Nfs.ServerAddress ->
                            Nfs.ExportName ->
                            SyncNfs ->
                            TestTree
test_ftruncate_and_fstat srv xprt nfs =
  let tsize = 67890
      assertion' ctx fh = do
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
      assertion = with_file' nfs srv xprt $ \ctx _ fpath ->
        with_fh nfs ctx fpath WriteOnly $ \fh -> do
          res <- runEitherT $ assertion' ctx fh
          case res of
            Left s -> HU.assertFailure s
            Right () -> return ()
  in
   HU.testCase "Ftruncate and fstat file" assertion

test_ftruncate_and_lseek :: Nfs.ServerAddress ->
                            Nfs.ExportName ->
                            SyncNfs ->
                            TestTree
test_ftruncate_and_lseek srv xprt nfs =
  let tsize = 13579
      assertion' ctx fh = do
        ret <- liftIO $ syncFTruncate nfs ctx fh $ fromIntegral tsize
        case ret of
          Left s -> left $ "ftruncate failed: " ++ s
          Right _ -> right ()
        liftIO $ check_pos nfs ctx fh 0 "figure out file pos"
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
          Left s -> right ()
          Right pos -> left $ "could do a relative seek to pos " ++ show pos
        sret <- liftIO $ syncLSeek nfs ctx fh (-(tsize + 1)) SeekFromEnd
        case sret of
          Left s -> right ()
          Right pos -> left $ "could seek to -(tsize + 1) from end to pos " ++ show pos
      assertion = with_file' nfs srv xprt $ \ctx _ fpath ->
          with_fh nfs ctx fpath WriteOnly $ \fh -> do
            res <- runEitherT $ assertion' ctx fh
            case res of
              Left s -> HU.assertFailure s
              Right () -> return ()
  in
   HU.testCase "Ftruncate and lseek in file" assertion

check_pread :: SyncNfs ->
               Nfs.Context ->
               Nfs.Fh ->
               FileOffset ->
               BS.ByteString ->
               IO ()
check_pread nfs ctx fh off pattern = do
  pos <- Nfs.getCurrentOffset fh
  rret <- syncPRead nfs ctx fh (fromIntegral $ BS.length pattern) (fromIntegral off)
  case rret of
    Left s -> HU.assertFailure $ "pread failed: " ++ s
    Right bs -> HU.assertEqual "read data should match expectation" pattern bs
  pos' <- Nfs.getCurrentOffset fh
  HU.assertEqual "file position must not have changed" pos pos'

check_read :: SyncNfs ->
              Nfs.Context ->
              Nfs.Fh ->
              BS.ByteString ->
              IO ()
check_read nfs ctx fh pattern = do
  let size = (fromIntegral $ BS.length pattern)
  pos <- Nfs.getCurrentOffset fh
  rret <- syncRead nfs ctx fh size
  case rret of
    Left s -> HU.assertFailure $ "read failed: " ++ s
    Right bs -> HU.assertEqual "read data should match expectation" pattern bs
  pos' <- Nfs.getCurrentOffset fh
  HU.assertEqual "file position must have changed" (pos + fromIntegral size) pos'

check_pwrite :: SyncNfs ->
                Nfs.Context ->
                Nfs.Fh ->
                FileOffset ->
                BS.ByteString ->
                IO ()
check_pwrite nfs ctx fh off pattern = do
  pos <- Nfs.getCurrentOffset fh
  wret <- syncPWrite nfs ctx fh pattern (fromIntegral off)
  case wret of
    Left s -> HU.assertFailure $ "pwrite failed: " ++ s
    Right size -> HU.assertEqual
                  "write size should match expectation"
                  (BS.length pattern) (fromIntegral size)
  pos' <- Nfs.getCurrentOffset fh
  HU.assertEqual "file position must not have changed" pos pos'

check_write :: SyncNfs ->
               Nfs.Context ->
               Nfs.Fh ->
               BS.ByteString ->
               IO ()
check_write nfs ctx fh pattern = do
  let size = (fromIntegral $ BS.length pattern)
  pos <- Nfs.getCurrentOffset fh
  wret <- syncWrite nfs ctx fh pattern
  case wret of
    Left s -> HU.assertFailure $ "write failed: " ++ s
    Right size' -> HU.assertEqual
                  "write size should match expectation"
                  size size'
  pos' <- Nfs.getCurrentOffset fh
  HU.assertEqual "file position must have changed" (pos + fromIntegral size) pos'

test_ftruncate_pwrite_and_pread_file :: Nfs.ServerAddress ->
                                        Nfs.ExportName ->
                                        SyncNfs ->
                                        TestTree
test_ftruncate_pwrite_and_pread_file srv xprt nfs =
  let pattern = BSC8.pack "not of any importance either"
      offset = 42
      tsize = offset + (BS.length pattern)
      assertion' ctx fh = do
        ret <- liftIO $ syncFTruncate nfs ctx fh $ fromIntegral tsize
        case ret of
          Left s -> left $ "ftruncate failed: " ++ s
          Right _ -> right ()
        liftIO $ check_pwrite nfs ctx fh (fromIntegral offset) pattern
        liftIO $ check_pread nfs ctx fh (fromIntegral offset) pattern
        right ()
      assertion = with_file' nfs srv xprt $ \ctx _ fpath ->
          with_fh nfs ctx fpath ReadWrite $ \fh -> do
            res <- runEitherT $ assertion' ctx fh
            case res of
              Left s -> HU.assertFailure s
              Right _ -> return ()
  in
   HU.testCase "FTruncate, pwrite to and pread from file" assertion

test_mount_wrong_server :: Nfs.ServerAddress -> Nfs.ExportName -> SyncNfs -> TestTree
test_mount_wrong_server _ _ nfs =
  let assertion = with_context $ \ctx -> do
        uuid <- return.toString =<< nextRandom
        ret <- mount nfs ctx uuid uuid
        case ret of
          Left _ -> return ()
          Right _ -> HU.assertFailure $ "mounting " ++ uuid ++ ":" ++ uuid ++
                     " succeeded unexpectedly"
  in
   HU.testCase "Mount wrong server" assertion

test_mount_wrong_export :: Nfs.ServerAddress -> Nfs.ExportName -> SyncNfs -> TestTree
test_mount_wrong_export srv _ nfs =
  let assertion = with_context $ \ctx -> do
        uuid <- return.toString =<< nextRandom
        ret <- mount nfs ctx srv uuid
        case ret of
          Left _ -> return ()
          Right _ -> HU.assertFailure $ "mounting " ++ srv ++ ":" ++ uuid ++
                     " succeeded unexpectedly"
  in
   HU.testCase "Mount wrong export" assertion

test_mount_ok :: Nfs.ServerAddress -> Nfs.ExportName -> SyncNfs -> TestTree
test_mount_ok srv xprt nfs =
  let assertion = with_context $ \ctx -> do
        ret <- mount nfs ctx srv xprt
        case ret of
          Left s -> HU.assertFailure $ "mounting " ++ srv ++ ":" ++ xprt ++
                    " failed: " ++ s
          Right _ -> return ()
  in
   HU.testCase "Mount correct server and export" assertion

test_init_and_destroy_context :: TestTree
test_init_and_destroy_context =
  let assertion = with_context $ \ctx -> do
        err <- Nfs.getError ctx
        HU.assertEqual "initContext failed" Nothing err
  in
   HU.testCase "Init and destroy context" assertion

test_garbage_collect_context :: TestTree
test_garbage_collect_context =
  let
    assertion = Nfs.initContext >> return ()
  in
   HU.testCase "Garbage collect context" assertion

test_destroy_context_twice :: TestTree
test_destroy_context_twice =
  let assertion = do
        ctx <- Nfs.initContext
        Nfs.destroyContext ctx
        Nfs.destroyContext ctx
  in
   HU.testCase "Destroy context twice in a row" assertion

test_get_fd :: TestTree
test_get_fd =
  let assertion = with_context $ \ctx -> do
        fd <- Nfs.getFd ctx
        HU.assertBool "got an fd without mounting" (fd < 0)
  in
   HU.testCase "Get fd from context" assertion

test_get_fd_mounted :: Nfs.ServerAddress -> Nfs.ExportName -> SyncNfs -> TestTree
test_get_fd_mounted srv xprt nfs =
  let assertion = with_mount nfs srv xprt $ \ctx -> do
        fd <- Nfs.getFd ctx
        HU.assertBool "didn't get an FD back" $ fd >= 0
  in
   HU.testCase "Get fd from mounted context" assertion

test_queue_length :: TestTree
test_queue_length =
  let assertion = with_context $ \ctx -> do
        l <- Nfs.queueLength ctx
        HU.assertEqual "unexpected queue length" 0 l
  in
   HU.testCase "Get queue length from context" assertion

test_get_read_max :: TestTree
test_get_read_max =
  let assertion = with_context $ \ctx -> do
        l <- Nfs.getReadMax ctx
        HU.assertEqual "unexpected read max" 0 l
  in
   HU.testCase "Get read max from context" assertion

test_get_write_max :: TestTree
test_get_write_max =
  let assertion = with_context $ \ctx -> do
        l <- Nfs.getWriteMax ctx
        HU.assertEqual "unexpected write max" 0 l
  in
   HU.testCase "Get write max from context" assertion

test_directory_source :: Nfs.ServerAddress -> Nfs.ExportName -> SyncNfs -> TestTree
test_directory_source addr xprt nfs =
  let assertion = with_directory' nfs addr xprt "/" $ \ctx parent ->
        with_directory' nfs addr xprt parent $ \ctx child_dir ->
          with_file nfs ctx parent $ \child_file -> do
            l <- runResourceT $ NfsC.sourceDirectory ctx parent $$ ListC.consume
            HU.assertEqual "unexpected directory contents" 2 $ length l
            HU.assertBool "child file must be present in output" $
              any (\e -> e == parent </> child_file) l
            HU.assertBool "child dir must be present in output" $
              any (\e -> e == parent </> child_dir) l
  in
   HU.testCase "SourceDirectory conduit" assertion

test_fh_source_and_sink :: Nfs.ServerAddress ->
                           Nfs.ExportName ->
                           SyncNfs ->
                           TestTree
test_fh_source_and_sink addr xprt nfs =
  let pattern = BSC8.pack "some arbitrary, utterly unremarkable text"
      assertion = with_file' nfs addr xprt $ \ctx dir source ->
        with_fh nfs ctx source ReadWrite $ \fh -> do
          check_pwrite nfs ctx fh 0 pattern
          with_file nfs ctx dir $ \sink ->
            with_fh nfs ctx sink ReadWrite $ \fh2 -> do
              runResourceT $ NfsC.sourceFh fh $$ NfsC.sinkFh fh2
              check_pread nfs ctx fh2 0 pattern
  in
   HU.testCase "Conduit source and sink - Fh" assertion

test_file_path_source_and_sink :: Nfs.ServerAddress ->
                                  Nfs.ExportName ->
                                  SyncNfs ->
                                  TestTree
test_file_path_source_and_sink addr xprt nfs =
  let pattern = BSC8.pack "some dull string"
      assertion = with_file' nfs addr xprt $ \ctx dir source ->
        with_fh nfs ctx source ReadWrite $ \fh -> do
          check_pwrite nfs ctx fh 0 pattern
          with_file nfs ctx dir $ \sink -> do
            runResourceT $ NfsC.sourceFile ctx source $$ NfsC.sinkFile ctx sink
            with_fh nfs ctx sink ReadWrite $ \fh2 ->
              check_pread nfs ctx fh2 0 pattern
  in
   HU.testCase "Conduit source and sink - FilePath" assertion

test_file_path_range_source_and_sink :: Nfs.ServerAddress ->
                                        Nfs.ExportName ->
                                        SyncNfs ->
                                        TestTree
test_file_path_range_source_and_sink addr xprt nfs =
  let pattern1 = BSC8.pack "some"
      size1 = BS.length pattern1
      pattern2 = BSC8.pack "pointless"
      size2 = BS.length pattern2
      pattern3 = BSC8.pack "text"
      size3 = BS.length pattern3
      assertion = with_file' nfs addr xprt $ \ctx dir source ->
        with_fh nfs ctx source ReadWrite $ \fh -> do
          check_write nfs ctx fh pattern1
          check_write nfs ctx fh pattern2
          check_write nfs ctx fh pattern3
          with_file nfs ctx dir $ \sink1 ->
            with_file nfs ctx dir $ \sink2 ->
              with_file nfs ctx dir $ \sink3 -> do
                runResourceT $
                  NfsC.sourceFileRange ctx source Nothing (Just $ fromIntegral size1) $$
                  NfsC.sinkFile ctx sink1
                runResourceT $
                  NfsC.sourceFileRange ctx source (Just $ fromIntegral size1) (Just $ fromIntegral size2) $$
                  NfsC.sinkFile ctx sink2
                runResourceT $
                  NfsC.sourceFileRange ctx source (Just $ fromIntegral (size1 + size2)) Nothing $$
                  NfsC.sinkFile ctx sink3
                with_fh nfs ctx sink1 ReadOnly $ \fh' ->
                  check_read nfs ctx fh' pattern1
                with_fh nfs ctx sink2 ReadOnly $ \fh' ->
                  check_read nfs ctx fh' pattern2
                with_fh nfs ctx sink3 ReadOnly $ \fh' ->
                  check_read nfs ctx fh' pattern3
  in
   HU.testCase "Conduit source and sink - FilePath" assertion

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

basic_tests :: TestTree
basic_tests = testGroup "Basic tests" [ test_init_and_destroy_context
                                      , test_destroy_context_twice
                                      , test_get_fd
                                      , test_queue_length
                                      , test_get_read_max
                                      , test_get_write_max ]

advanced_tests :: [ (Nfs.ServerAddress -> Nfs.ExportName -> SyncNfs -> TestTree) ]
advanced_tests = [ test_get_fd_mounted
                 , test_mount_ok
                 , test_mount_wrong_export
                 , test_mount_wrong_server
                 , test_create_and_remove_directory
                 , test_list_empty_directory
                 , test_create_and_remove_file
                 , test_write_and_read_file
                 , test_truncate_and_stat
                 , test_ftruncate_and_fstat
                 , test_ftruncate_and_lseek
                 , test_ftruncate_pwrite_and_pread_file ]

sync_tests :: Nfs.ServerAddress -> Nfs.ExportName -> TestTree
sync_tests srv xprt = testGroup "Synchronous interface tests" $
             fmap (\test -> test srv xprt sync_nfs) $ advanced_tests

async_tests :: Nfs.ServerAddress -> Nfs.ExportName -> TestTree
async_tests srv xprt = testGroup "Asynchronous interface tests" $
              fmap (\test -> test srv xprt async_nfs) $ advanced_tests

conduit_tests' :: [ (Nfs.ServerAddress -> Nfs.ExportName -> SyncNfs -> TestTree) ]
conduit_tests' = [ test_directory_source
                 , test_fh_source_and_sink
                 , test_file_path_source_and_sink
                 , test_file_path_range_source_and_sink ]

conduit_tests :: Nfs.ServerAddress -> Nfs.ExportName -> TestTree
conduit_tests srv xprt = testGroup "Nfs conduit tests" $
                         fmap (\test -> test srv xprt sync_nfs) $ conduit_tests'

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
