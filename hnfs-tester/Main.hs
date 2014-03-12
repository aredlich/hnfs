-- Main module of nfs-tester
--
-- Copyright (C) 2014 Arne Redlich <arne.redlich@googlemail.com>
--
-- Licensed under the LGPL v2.1 - see the LICENSE file for details.

-- CmdArgs wants this
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Data.IORef
import Data.Monoid
import Data.Typeable (Typeable)
import Data.Proxy
import Data.UUID
import Data.UUID.V4

import qualified GHC.Event as Ev

import System.FilePath.Posix
import qualified System.Nfs as Nfs

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

sync_wrap :: Show a => Nfs.Context ->
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

mount_and_list :: Nfs.ServerAddress -> Nfs.ExportName -> FilePath -> IO ()
mount_and_list srv xprt path = do
  ctx <- Nfs.initContext
  ret <- sync_wrap ctx $ Nfs.mountAsync ctx srv xprt
  case ret of
    Left s -> HU.assertFailure $ "failed to mount " ++ srv ++ ":" ++ xprt ++ ": " ++ s
    Right _ -> do
      ret' <- sync_wrap ctx $ Nfs.openDirAsync ctx path
      case ret' of
        Left s -> HU.assertFailure $ "failed to open dir \"/\": " ++ s
        Right dir -> print_entries ctx dir
    where
      print_entries :: Nfs.Context -> Nfs.Dir -> IO ()
      print_entries ctx' dir' = do
        maybe_entry <- Nfs.readDir ctx' dir'
        case maybe_entry of
          Nothing -> return ()
          Just e -> do
            putStrLn $
              "{ name = " ++ (Nfs.direntName e) ++
              ", type = " ++ (show $ Nfs.direntFType3 e) ++
              ", size = " ++ (show $ Nfs.direntSize e) ++
              ", mode = " ++ (show $ Nfs.direntMode e) ++
              ", uid = " ++ (show $ Nfs.direntUid e) ++
              ", gid = " ++ (show $ Nfs.direntGid e) ++
              ", atime = " ++ (show $ Nfs.direntATime e) ++
              ", ctime = " ++ (show $ Nfs.direntCTime e) ++
              ", mtime = " ++ (show $ Nfs.direntMTime e) ++
              ", inode = " ++ (show $ Nfs.direntInode e) ++
              "}"
            print_entries ctx' dir'

with_mount :: Nfs.ServerAddress ->
              Nfs.ExportName ->
              (Nfs.Context -> IO ()) ->
              IO ()
with_mount srv xprt action = do
  ret <- mount srv xprt
  case ret of
    Left s -> HU.assertFailure $ "mounting " ++ srv ++ ":" ++ xprt ++ " failed: " ++ s
    Right ctx -> action ctx

mount :: Nfs.ServerAddress -> Nfs.ExportName -> IO (Either String Nfs.Context)
mount srv xprt = runEitherT $ do
  ctx <- liftIO Nfs.initContext
  ret <- liftIO $ sync_wrap ctx $ Nfs.mountAsync ctx srv xprt
  case ret of
    Left s -> left s
    Right () -> right ctx

with_directory :: Nfs.Context ->
                  FilePath ->
                  (FilePath -> IO ()) ->
                  IO ()
with_directory ctx parent action = bracket mkdir rmdir action
  where
    mkdir :: IO FilePath
    mkdir = do
      uuid <- return . toString =<< nextRandom
      path <- return $ joinPath [ parent, uuid ]
      ret <- sync_wrap ctx $ Nfs.mkDirAsync ctx path
      case ret of
        Left s -> fail $ "failed to create path: " ++ s
        Right () -> return path

    rmdir :: FilePath -> IO ()
    rmdir path = do
      ret <- sync_wrap ctx $ Nfs.rmDirAsync ctx path
      case ret of
        -- XXX: does this override the original assertion?
        Left s -> HU.assertFailure $ "failed to remove path: " ++ s
        Right () -> return ()

test_create_and_remove_directory :: Nfs.ServerAddress ->
                                    Nfs.ExportName ->
                                    TestTree
test_create_and_remove_directory srv xprt =
  let assertion = with_mount srv xprt $ \ctx ->
        with_directory ctx "/" $ \path -> do
          ret <- sync_wrap ctx $ Nfs.statAsync ctx path
          case ret of
            Left s -> HU.assertFailure $ "failed to stat " ++ path ++ ": " ++ s
            -- TODO: verify that type is directory
            Right _ -> return ()
  in
   HU.testCase "create and remove directory" assertion

test_mount_wrong_server :: TestTree
test_mount_wrong_server = let assertion = do
                                uuid <- nextRandom >>= \u -> return $ toString u
                                ret <- mount uuid uuid
                                case ret of
                                  Left s -> return ()
                                  Right _ -> HU.assertFailure $
                                             "mounting " ++ uuid ++ ":" ++ uuid ++
                                             " succeeded unexpectedly"
                          in
                           HU.testCase "mount test, wrong server" assertion

test_mount_wrong_export :: Nfs.ServerAddress -> TestTree
test_mount_wrong_export srv = let assertion = do
                                    uuid <- nextRandom >>= \u -> return $ toString u
                                    ret <- mount srv uuid
                                    case ret of
                                      Left s -> return ()
                                      Right _ -> HU.assertFailure $
                                                 "mounting " ++ srv ++ ":" ++ uuid ++
                                                 " succeeded unexpectedly"
                              in
                               HU.testCase "mount test, wrong export" assertion

test_mount_ok :: Nfs.ServerAddress -> Nfs.ExportName -> TestTree
test_mount_ok srv xprt = let assertion = do
                               ret <- mount srv xprt
                               case ret of
                                 Left s -> HU.assertFailure $
                                           "mounting " ++ srv ++ ":" ++ xprt ++
                                           " failed: " ++ s
                                 Right _ -> return ()
                         in
                          HU.testCase "mount test" assertion

test_init_context :: TestTree
test_init_context = let assertion = do
                          ctx <- Nfs.initContext
                          err <- Nfs.getError ctx
                          HU.assertEqual "initContext failed" Nothing err
                    in
                     HU.testCase "test initContext" assertion

test_destroy_context :: TestTree
test_destroy_context = let assertion = do
                             ctx <- Nfs.initContext
                             Nfs.destroyContext ctx
                       in HU.testCase "test destroyContext" assertion

test_destroy_context_twice :: TestTree
test_destroy_context_twice = let assertion = do
                                   ctx <- Nfs.initContext
                                   Nfs.destroyContext ctx
                                   Nfs.destroyContext ctx
                             in HU.testCase "test destroyContext 2x" assertion

test_get_fd :: TestTree
test_get_fd = let assertion = do
                    ctx <- Nfs.initContext
                    fd <- Nfs.getFd ctx
                    HU.assertBool "got an fd without mounting" (fd < 0)
              in
               HU.testCase "test getFd" assertion

test_get_fd_mounted :: Nfs.ServerAddress -> Nfs.ExportName -> TestTree
test_get_fd_mounted srv xprt = let assertion =
                                     with_mount srv xprt $ \ctx -> do
                                       fd <- Nfs.getFd ctx
                                       HU.assertBool "didn't get an FD back" $ fd >= 0
                               in
                                HU.testCase "test getFd mounted" assertion

test_queue_length :: TestTree
test_queue_length = let assertion = do
                          ctx <- Nfs.initContext
                          l <- Nfs.queueLength ctx
                          HU.assertEqual "unexpected queue length" 0 l
                    in
                     HU.testCase "test queueLength" assertion

test_get_read_max :: TestTree
test_get_read_max = let assertion = do
                          ctx <- Nfs.initContext
                          l <- Nfs.getReadMax ctx
                          HU.assertEqual "unexpected read max" 0 l
                    in
                     HU.testCase "test getReadMax" assertion

test_get_write_max :: TestTree
test_get_write_max = let assertion = do
                           ctx <- Nfs.initContext
                           l <- Nfs.getWriteMax ctx
                           HU.assertEqual "unexpected write max" 0 l
                     in
                      HU.testCase "test getWriteMax" assertion

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
        testGroup "Tests" $
       [ test_init_context
       , test_destroy_context
       , test_destroy_context_twice
       , test_get_fd
       , test_get_fd_mounted server export
       , test_queue_length
       , test_get_read_max
       , test_get_write_max
       , test_mount_ok server export
       , test_mount_wrong_export server
       , test_mount_wrong_server
       , test_create_and_remove_directory server export
       ]

-- Local Variables: **
-- mode: haskell **
-- compile-command: "cd ../.. && cabal install -v" **
-- End: **
