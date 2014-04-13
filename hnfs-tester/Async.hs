{-|
Module      : Async
Description : hnfs-tester - Nfs (client library) test tool
Copyright   : (c) 2014 Arne Redlich <arne.redlich@googlemail.com>
License     : LGPL v2.1
Maintainer  : Arne Redlich <arne.redlich@googlemail.com>
Stability   : experimental
Portability : POSIX

hnfs-tester tests using hnfs' async interface.
-}

module Async ( nfs ) where

import Base

import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either

import Data.Maybe (fromJust)
import Data.Monoid

import qualified System.Linux.Epoll as Ep
import qualified System.Nfs as Nfs

-- epoll and poll events are (mostly) the same. However, we don't have any
-- means to get to the underlying event codes. The below should hopefully get
-- us far enough.
nfs_to_ep_event :: Nfs.Event -> Ep.EventType
nfs_to_ep_event ev
  | ev == Nfs.eventRead = Ep.inEvent
  | ev == Nfs.eventWrite = Ep.outEvent
  | ev == Nfs.eventRead `mappend` Nfs.eventWrite = Ep.combineEvents [ Ep.inEvent
                                                                    , Ep.outEvent ]
  | otherwise = undefined

ep_to_nfs_event :: Ep.EventType -> Nfs.Event
ep_to_nfs_event ev
  | ev Ep.=~ Ep.inEvent && ev Ep.=~ Ep.outEvent =
    Nfs.eventRead `mappend` Nfs.eventWrite
  | ev Ep.=~ Ep.inEvent = Nfs.eventRead
  | ev Ep.=~ Ep.outEvent = Nfs.eventWrite
  | ev Ep.=~ Ep.hangupEvent = Nfs.eventHup
  | ev Ep.=~ Ep.urgentEvent = Nfs.eventPri
  | ev Ep.=~ Ep.errorEvent = Nfs.eventErr
  | otherwise = undefined

event_loop :: Nfs.Context -> MVar (Either String a) -> IO (Either String a)
event_loop ctx mv = do
  mret <- tryTakeMVar mv
  case mret of
    Just ret -> return ret
    Nothing -> do
      bracket open Ep.close poll
      event_loop ctx mv
  where
    -- Arbitrarily chosen size.
    open = Ep.create (fromJust $ Ep.toSize 32)

    -- The Context's fd can (and does!) change, so we need to get it out each time.
    poll dev = do
      fd <- Nfs.getFd ctx
      evts <- Nfs.whichEvents ctx
      bracket
        (Ep.add dev () [ nfs_to_ep_event evts, Ep.oneShotEvent ] fd)
        Ep.freeDesc
        $ \_ -> do
          -- Arbitrarily chosen timeout. epoll_wait allows -1 to wait indefinitely
          -- but that does not seem to be supported by the haskell bindings.
          evts' <- Ep.wait (fromJust $ Ep.toDuration 10) dev
          let etypes = map Ep.eventType evts'
              etype = Ep.combineEvents etypes
          Nfs.service ctx $ ep_to_nfs_event etype

sync_wrap :: Nfs.Context ->
             (Nfs.Callback a -> IO (Either String ())) ->
             IO (Either String a)
sync_wrap ctx async_action = runEitherT $ do
  mv <- liftIO $ newEmptyMVar
  ret <- liftIO $ async_action $ putMVar mv
  case ret of
    Left s -> left $ "failed to invoke async action: " ++ s
    Right () -> (liftIO $ event_loop ctx mv) >>= hoistEither

nfs :: SyncNfs
nfs = SyncNfs { syncMount = \ctx addr xprt ->
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
