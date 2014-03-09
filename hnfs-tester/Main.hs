-- Main module of nfs-tester
--
-- Copyright (C) 2014 Arne Redlich <arne.redlich@googlemail.com>
--
-- Licensed under the LGPL v2.1 - see the LICENSE file for details.

-- CmdArgs wants this
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Concurrent.MVar

import Data.IORef
import Data.Monoid

import qualified GHC.Event as Ev

import qualified System.Console.CmdArgs as CA
import qualified System.Nfs as Nfs

import qualified Test.Tasty.HUnit as HU

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

-- We need to constantly re-register the context's fd as libnfs closes / reopens
-- it.
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
sync_wrap ctx async_action = do
  mmgr <- Ev.getSystemEventManager
  case mmgr of
    Nothing -> return $ Left "failed to get system event manager"
    Just mgr -> do
      mv <- newEmptyMVar
      ret <- async_action $ \ret' -> do
        putMVar mv ret'
      case ret of
        Left s -> return $ Left $ "failed to invoke async action: " ++ s
        Right () -> do
          ref <- newIORef Nothing
          register_fd ctx mgr ref
          ret'' <- takeMVar mv
          mkey <- readIORef ref
          case mkey of
            Nothing -> return $ Left "failed to get fdkey to unregister"
            Just key -> do
              Ev.unregisterFd mgr key
              return ret''

-- TODO: use Either + IO monad transformer stack
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

data Args = Args { server :: Nfs.ServerAddress
                 , export :: Nfs.ExportName
                 } deriving (CA.Data, CA.Typeable, Show)

prog :: String
prog = "hnfs-tester"

help :: String
help = "Test program to both exercise and demonstrate the hnfs library"

version :: String
version = "0.0.1"

copyright :: String
copyright = "Copyright (C) 2014 Arne Redlich <arne.redlich@googlemail.com>"

license :: String
license = "Licensed under the LGPL v2.1. See the LICENSE file for details."

args :: CA.Mode (CA.CmdArgs Args)
args = CA.cmdArgsMode $  Args { server = CA.def CA.&= CA.help "NFS server address"
                              , export = CA.def CA.&= CA.help "NFS export"
                              }
       CA.&= CA.help help
       CA.&= CA.summary (prog ++ ", Version " ++ version ++ "\n\n" ++ copyright ++ "\n" ++ license)
       CA.&= CA.program prog

main :: IO ()
main = do
  a <- CA.cmdArgsRun args
  mount_and_list (server a) (export a) "/"

-- Local Variables: **
-- mode: haskell **
-- compile-command: "cd ../.. && cabal install -v" **
-- End: **
