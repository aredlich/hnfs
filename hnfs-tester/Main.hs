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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.IORef
import Data.Monoid
import Data.Typeable (Typeable)
import Data.Proxy
import Data.UUID hiding (null)
import Data.UUID.V4

import Foreign.C.Types

import qualified GHC.Event as Ev

import System.FilePath.Posix
import qualified System.Nfs as Nfs
import System.Posix.IO (OpenMode (..))

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
-- TODO: get rid of the IORef
ev_callback :: Nfs.Context -> Ev.EventManager -> IORef (Maybe Ev.FdKey) -> Ev.FdKey -> Ev.Event -> IO ()
ev_callback ctx mgr ref fdkey ev = do
  Ev.unregisterFd mgr fdkey
  Nfs.service ctx $ ev_to_nfs_event ev
  register_fd ctx mgr ref

register_fd :: Nfs.Context -> Ev.EventManager -> IORef (Maybe Ev.FdKey) -> IO ()
register_fd ctx mgr ref = do
  fd <- Nfs.getFd ctx
  evts <- Nfs.whichEvents ctx
  key <- Ev.registerFd mgr (ev_callback ctx mgr ref) fd $ nfs_to_ev_event evts
  writeIORef ref $ Just key

sync_wrap :: Nfs.Context ->
             (Nfs.Callback a -> IO (Either String ())) ->
             IO (Either String a)
sync_wrap ctx async_action = runEitherT $ do
  mmgr <- liftIO Ev.getSystemEventManager
  mgr <- case mmgr of
    Nothing -> left "failed to get system event manager"
    Just m -> right m
  mv <- liftIO newEmptyMVar
  ret <- liftIO $ async_action $ \ret' -> do
    putMVar mv ret'
  case ret of
    Left s -> left $ "failed to invoke async action: " ++ s
    Right () -> right ()
  ref <- liftIO $ newIORef Nothing
  liftIO $ register_fd ctx mgr ref
  ret'' <- liftIO $ takeMVar mv
  mkey <- liftIO $ readIORef ref
  key <- case mkey of
    Nothing -> left "failed to get fdkey to unregister"
    Just k -> right k
  liftIO $ Ev.unregisterFd mgr key
  hoistEither ret''

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
                       , syncOpen :: Nfs.Context ->
                                     FilePath ->
                                     OpenMode ->
                                     IO (Either String Nfs.Fh)
                       , syncWrite :: Nfs.Context ->
                                      Nfs.Fh ->
                                      BS.ByteString ->
                                      IO (Either String CSize)
                       , syncRead :: Nfs.Context ->
                                     Nfs.Fh ->
                                     CSize ->
                                     IO (Either String BS.ByteString)
                       , syncCreat :: Nfs.Context ->
                                      FilePath ->
                                      OpenMode ->
                                      IO (Either String Nfs.Fh)
                       , syncUnlink :: Nfs.Context ->
                                       FilePath ->
                                       IO (Either String ())
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

      is_dot_dir dent = (Nfs.direntName dent == "." || Nfs.direntName dent == "..") &&
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
  let assertion = with_directory' nfs srv xprt "/" $ \ctx dir ->
        with_file nfs ctx dir $ \fpath -> do
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

test_write_and_read_file srv xprt nfs =
  let pattern = BSC8.pack "of no particular importance"
      assertion = with_directory' nfs srv xprt "/" $ \ctx dir ->
        with_file nfs ctx dir $ \fpath -> do
          with_fh nfs ctx fpath WriteOnly $ \fh -> do
            wret <- liftIO $ syncWrite nfs ctx fh pattern
            case wret of
              Left s -> HU.assertFailure $ "write failed: " ++ s
              Right size -> HU.assertEqual
                            "pattern size bytes should've been written"
                            (BS.length pattern) (fromIntegral size)
          with_fh nfs ctx fpath ReadOnly $ \fh -> do
            rret <- syncRead nfs ctx fh (fromIntegral $ BS.length pattern)
            case rret of
              Left s -> HU.assertFailure $ "read failed: " ++ s
              Right bs -> HU.assertEqual
                          "read pattern should match written pattern"
                          pattern bs
  in
   HU.testCase "Write to and read from file" assertion

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

sync_nfs :: SyncNfs
sync_nfs = SyncNfs { syncMount = Nfs.mount
                   , syncOpenDir = Nfs.openDir
                   , syncMkDir = Nfs.mkDir
                   , syncRmDir = Nfs.rmDir
                   , syncStat = Nfs.stat
                   , syncCreat = Nfs.creat
                   , syncUnlink = Nfs.unlink
                   , syncOpen = Nfs.open
                   , syncRead = \_ fh size -> Nfs.read fh size
                   , syncWrite = \_ fh bs -> Nfs.write fh bs }

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
                    , syncWrite = \ctx fh bs ->
                       sync_wrap ctx $ Nfs.writeAsync fh bs
                    }

basic_tests :: TestTree
basic_tests = testGroup "Basic tests" [ test_init_and_destroy_context
                                      , test_destroy_context_twice
                                      , test_get_fd
                                      , test_queue_length
                                      , test_get_read_max
                                      , test_get_write_max
                                      ]

advanced_tests :: [ (Nfs.ServerAddress -> Nfs.ExportName -> SyncNfs -> TestTree) ]
advanced_tests = [ test_get_fd_mounted
                 , test_mount_ok
                 , test_mount_wrong_export
                 , test_mount_wrong_server
                 , test_create_and_remove_directory
                 , test_list_empty_directory
                 , test_create_and_remove_file
                 , test_write_and_read_file ]

sync_tests :: Nfs.ServerAddress -> Nfs.ExportName -> TestTree
sync_tests srv xprt = testGroup "Synchronous interface tests" $
             fmap (\test -> test srv xprt sync_nfs) $ advanced_tests

async_tests :: Nfs.ServerAddress -> Nfs.ExportName -> TestTree
async_tests srv xprt = testGroup "Asynchronous interface tests" $
              fmap (\test -> test srv xprt async_nfs) $ advanced_tests

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
                                 ]

-- Local Variables: **
-- mode: haskell **
-- compile-command: "cd .. && cabal install -v" **
-- End: **
