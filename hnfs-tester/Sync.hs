{-|
Module      : Sync
Description : hnfs-tester - Nfs (client library) test tool
Copyright   : (c) 2014 Arne Redlich <arne.redlich@googlemail.com>
License     : LGPL v2.1
Maintainer  : Arne Redlich <arne.redlich@googlemail.com>
Stability   : experimental
Portability : POSIX

hnfs-tester tests using hnfs' sync interface.
-}

module Sync ( nfs ) where

import Base
import qualified System.Nfs as Nfs

nfs :: SyncNfs
nfs = SyncNfs { syncMount = Nfs.mount
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
