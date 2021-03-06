{-|
Module      : System.Nfs
Description : hnfs - Nfs client library
Copyright   : (c) 2014 Arne Redlich <arne.redlich@googlemail.com>
License     : LGPL v2.1
Maintainer  : Arne Redlich <arne.redlich@googlemail.com>
Stability   : experimental
Portability : POSIX

Haskell bindings for <https://github.com/sahlberg/libnfs libnfs>).
-}

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module System.Nfs ( AccessCallback
                  , AccessMode
                  , AccessTimeVal
                  , BlockCount
                  , BlockSize
                  , Callback
                  , ChdirCallback
                  , ChmodCallback
                  , ChownCallback
                  , Context
                  , Dir
                  , Dirent
                  , Event
                  , ExportName
                  , Fh
                  , FType3 (..)
                  , FSyncCallback
                  , LinkCallback
                  , LSeekCallback
                  , MkDirCallback
                  , ModificationTimeVal
                  , MountCallback
                  , NoDataCallback
                  , OpenDirCallback
                  , OpenFileCallback
                  , ReadCallback
                  , ReadLinkCallback
                  , RenameCallback
                  , RmDirCallback
                  , ServerAddress
                  , Stat
                  , StatCallback
                  , StatVFSCallback
                  , SymlinkCallback
                  , TimeVal
                  , TruncateCallback
                  , UnlinkCallback
                  , UTimesCallback
                  , WriteCallback
                  , access
                  , accessAsync
                  , accessModeExec
                  , accessModeExists
                  , accessModeRead
                  , accessModeWrite
                  , chdir
                  , chdirAsync
                  , chmod
                  , chmodAsync
                  , chown
                  , chownAsync
                  , closeDir
                  , closeFh
                  , creat
                  , creatAsync
                  , destroyContext
                  , direntATime
                  , direntCTime
                  , direntFType3
                  , direntGid
                  , direntInode
                  , direntMode
                  , direntMTime
                  , direntName
                  , direntSize
                  , direntUid
                  , eventErr
                  , eventHup
                  , eventNone
                  , eventNval
                  , eventPri
                  , eventRead
                  , eventWrite
                  , fchmod
                  , fchmodAsync
                  , fchown
                  , fchownAsync
                  , fstat
                  , fstatAsync
                  , fsync
                  , fsyncAsync
                  , ftruncate
                  , ftruncateAsync
                  , initContext
                  , getCurrentOffset
                  , getCwd
                  , getError
                  , getFd
                  , getReadMax
                  , getWriteMax
                  , isBlockDevice
                  , isCharacterDevice
                  , isDirectory
                  , isNamedPipe
                  , isRegularFile
                  , isSocket
                  , isSymbolicLink
                  , link
                  , linkAsync
                  , lseek
                  , lseekAsync
                  , mkDir
                  , mkDirAsync
                  , mount
                  , mountAsync
                  , open
                  , openAsync
                  , openDir
                  , openDirAsync
                  , pread
                  , preadAsync
                  , pwrite
                  , pwriteAsync
                  , queueLength
                  , read
                  , readAsync
                  , readDir
                  , readlink
                  , readlinkAsync
                  , rename
                  , renameAsync
                  , rmDir
                  , rmDirAsync
                  , service
                  , setGid
                  , setUid
                  , setTcpSynCount
                  , statATime
                  , stat
                  , statAsync
                  , statBlkSize
                  , statBlocks
                  , statCTime
                  , statDev
                  , statGid
                  , statIno
                  , statMTime
                  , statNLink
                  , statRDev
                  , statSize
                  , statUid
                  , statvfs
                  , statvfsAsync
                  , symlink
                  , symlinkAsync
                  , truncate
                  , truncateAsync
                  , unlink
                  , unlinkAsync
                  , utimes
                  , utimesAsync
                  , whichEvents
                  , write
                  , writeAsync
                  ) where

import Control.Concurrent.MVar
import Control.Monad

import Data.Bits
import qualified Data.ByteString as BS
--import Data.Either
--import Data.Maybe
import Data.Monoid
import Data.Word (Word64)

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

-- Really only to hide read and truncate' since we provide eponymous calls.
import Prelude hiding (read, truncate)

import System.IO (SeekMode)

-- provides TimeSpec, albeit based on Int for both sec and nsec
import qualified System.Clock as Clock
import System.Posix.IO (OpenMode (..))
import System.Posix.StatVFS
import System.Posix.Types

#include <poll.h>

-- access mode definitions: ${R,W,F,X}_OK
#include <unistd.h>

-- stat and statvfs
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <sys/types.h>
#include <unistd.h>

-- I hope this is the fixed version that includes <sys/time.h>
-- (a deficiency in libnfs.h: it makes use of struct timeval but lacks this
-- include).
-- For some reason including it explicitly here before the libnfs.h is not enough.
#include <nfsc/libnfs.h>

-- No "prefix" for now to avoid confusion.
{# context lib="nfs" #}

-- struct nfs_context - an opaque handle passed around with pointers
data Context_
{# pointer* nfs_context as ContextPtr -> Context_ #}

{# fun nfs_destroy_context as destroy_context { id `ContextPtr' } -> `()' #}

-- Wrap the pointer into an MVar (nfs_context is not thread safe!) and a Maybe
-- so we can have our cake and eat it too: if the Context isn't explicitly destroyed
-- the pointer will be disposed by the garbage collection.
-- The MVar *must not* be left empty otherwise the finalizer will choke.

-- | Opaque handle that represents an NFS export. Several Contexts to the same
-- export can be used concurrently. A Context should be released with
-- @destroyContext@ to free its resources. If not released explicitly the garbage
-- collection will finalize it at an unspecified point in time.
data Context = Context !(MVar (Maybe ContextPtr))

finalize_context_mvar :: MVar (Maybe ContextPtr) -> IO ()
finalize_context_mvar mv = modifyMVar_ mv $ \mctx -> case mctx of
        Just ptr -> destroy_context ptr >> return Nothing
        Nothing -> return Nothing

mk_context :: ContextPtr -> IO Context
mk_context ptr = do
  mv <- newMVar $ Just ptr
  _ <- mkWeakMVar mv $ finalize_context_mvar mv
  return $ Context mv

-- TODO: revisit the error handling: we might want to throw instead of using
-- Either String a to report errors as that allows a nicer integration with
-- errors due to an attempt to use a closed handle.
with_context :: Context -> (ContextPtr -> IO a) -> IO a
with_context (Context mv) act = withMVar mv $ \mctx -> case mctx of
  Just ctx -> act ctx
  Nothing -> fail "context was destroyed"

-- | Create a new @Context@.
{# fun nfs_init_context as initContext {} -> `Context' mk_context* #}

-- | Explicitly release a @Context@. After being released the @Context@ becomes
-- unusable, i.e. any further action using it will raise an error.
destroyContext :: Context -> IO ()
destroyContext (Context mv) = finalize_context_mvar mv

maybe_error :: CString -> IO (Maybe String)
maybe_error ptr
  | ptr == nullPtr = return Nothing
  | otherwise = liftM Just $ peekCString ptr

-- | Get a string representation of the last error that happened in the @Context@.
{# fun nfs_get_error as getError { with_context* `Context' } ->
 `Maybe String' maybe_error* #}

-- | Get the maximum READ3 size supported by the server.
{# fun nfs_get_readmax as getReadMax { with_context* `Context' } ->
 `Integer' fromIntegral #}

-- | Get the maximum WRITE3 size supported by the server.
{# fun nfs_get_writemax as getWriteMax { with_context* `Context' } ->
 `Integer' fromIntegral #}

-- | Modify the TCP SYN count.
{# fun nfs_set_tcp_syncnt as setTcpSynCount { with_context* `Context',
                                              fromIntegral `Integer'} -> `()' #}

-- | Set the @UserID@ the client sends to the server.
{# fun nfs_set_uid as setUid { with_context* `Context',
                               fromIntegral `UserID' } -> `()' #}

-- | Set the @GroupID@ the client sends to the server.
{# fun nfs_set_gid as setGid { with_context* `Context',
                               fromIntegral `GroupID' } -> `()' #}

-- Not provided by <poll.h> but will be handy for our purposes.
#c
#define POLLNONE 0
#endc

{# enum define PollEvent { POLLNONE as PollNone,
                           POLLIN as PollIn,
                           POLLPRI as PollPri,
                           POLLOUT as PollOut,
                           POLLERR as PollErr,
                           POLLHUP as PollHup,
                           POLLNVAL as PollNval } deriving (Eq, Show) #}

-- Push to a higher level API?
-- | Poll events the async interface might be interested in.
newtype Event = Event Int deriving (Eq, Show)

-- | The neutral element for the Monoid instance.
eventNone :: Event
eventNone = Event $ fromEnum PollNone

-- | Read event.
eventRead :: Event
eventRead = Event $ fromEnum PollIn

-- | Write event.
eventWrite :: Event
eventWrite = Event $ fromEnum PollOut

-- | Error event.
eventErr :: Event
eventErr = Event $ fromEnum PollErr

-- | HUP event.
eventHup :: Event
eventHup = Event $ fromEnum PollHup

-- | Urgent read data.
eventPri :: Event
eventPri = Event $ fromEnum PollPri

-- | Invalid event.
eventNval :: Event
eventNval = Event $ fromEnum PollNval

instance Monoid Event where
  mempty = eventNone
  mappend (Event a) (Event b) = Event (a .|. b)

to_event :: CInt -> Event
to_event e = Event $ fromIntegral e

from_event :: Event -> CInt
from_event (Event e) = fromIntegral e

-- | Query the context which event(s) to poll for.
{# fun nfs_which_events as whichEvents { with_context* `Context' } ->
 `Event' to_event #}

-- | Handle the @Event@ detected on the @Fd@.
{# fun nfs_service as service { with_context* `Context'
                              , from_event `Event'
                              } -> `()' #}

-- | Get the @Fd@ (file descriptor) to poll for async events.
{# fun nfs_get_fd as getFd { with_context* `Context' } -> `Fd' fromIntegral #}

-- | Queue length of this @Context@.
{# fun nfs_queue_length as queueLength { with_context* `Context' } -> `Integer' fromIntegral #}

errmsg :: Maybe String -> String
errmsg (Just s) = s
errmsg Nothing = "unspecified error"

-- For now - while I'm still in exploratory mode - errors will be treated
-- with this (odd?) Either. At a later point exceptions could turn out to be a
-- better idea.
handle_ret_error'' :: Context -> (a -> IO b) -> (CInt, a) -> IO (Either String b)
handle_ret_error'' ctx act (err, ret)
  | err >= 0 = act ret >>= \x -> return $ Right x
  | otherwise = do
    mmsg <- getError ctx
    return $ Left $ errmsg mmsg

handle_ret_error' :: Context -> (CInt, a) -> IO (Either String a)
handle_ret_error' ctx pair = handle_ret_error'' ctx (return) pair

handle_ret_error :: Context -> CInt -> IO (Either String ())
handle_ret_error ctx err = handle_ret_error' ctx (err, ())

-- There's not much point in threading the Context or any private data through
-- libnfs calls as we can use partial application.
type CCallback = CInt -> -- err
                 ContextPtr -> -- we have no need for it
                 Ptr () -> -- data - to be cast
                 Ptr () -> -- private_data - we have no need for it
                 IO ()

foreign import ccall "wrapper"
  wrap_cb :: CCallback -> IO (FunPtr (CCallback))

type DataExtractor a = CInt -> Ptr () -> IO a

handle_cb_error :: CInt -> Ptr () -> DataExtractor a -> IO (Either String a)
handle_cb_error status ptr extract
  | status >= 0 = liftM Right $ extract status ptr
  | otherwise = liftM Left $ peekCString (castPtr ptr)

-- | General type of callbacks.
type Callback a = (Either String a) -> IO ()

to_callback_ptr :: Callback a ->
                   DataExtractor a ->
                   IO (FunPtr CCallback)
to_callback_ptr cb extractor = wrap_cb =<< to_c_callback cb extractor
  where
    to_c_callback :: Callback a ->
                     DataExtractor a ->
                     IO CCallback
    to_c_callback cb' extractor' = return $ \err _ ptr _ ->
      handle_cb_error err ptr extractor' >>= cb'

-- | Type of callbacks that do not expect inbound data.
type NoDataCallback = Callback ()

-- | Type of the callback for mountAsync.
type MountCallback = NoDataCallback

type WrappedAction = FunPtr CCallback -> Ptr () -> IO CInt

wrap_action :: Context ->
               WrappedAction ->
               Callback a ->
               DataExtractor a ->
               IO (Either String ())
wrap_action ctx action cb extract = do
  ccb <- to_callback_ptr cb extract
  handle_ret_error ctx =<< action ccb nullPtr

-- | An NFS server's (IP) address.
type ServerAddress = String

-- | Name of an NFS export.
type ExportName = String

extract_nothing :: DataExtractor ()
extract_nothing _ _ = return ()

-- C2HS can't go all the way for this one:
-- - I cannot seem to get the marshalling of the callback right
-- - the private_data ptr of the C call won't be used by us
-- - converting the Integer error code to our Either String () requires access
--   to the Context
{# fun nfs_mount_async as mount_async { with_context* `Context'
                                      , withCString* `ServerAddress'
                                      , withCString* `ExportName'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

-- | Invoke an asynchronous mount of the NFS export of the given server.
mountAsync :: Context ->
              ServerAddress ->
              ExportName ->
              MountCallback ->
              IO (Either String ())
mountAsync ctx addr export cb =
  wrap_action ctx (mount_async ctx addr export) cb extract_nothing

{# fun nfs_mount as mount_sync { with_context* `Context'
                               , withCString* `ServerAddress'
                               , withCString* `ExportName'
                               } -> `CInt' id #}

-- | Synchronously mount the specified NFS export of the given server.
mount :: Context -> ServerAddress -> ExportName -> IO (Either String ())
mount ctx addr xprt = handle_ret_error ctx =<< mount_sync ctx addr xprt

data Dir_

{# pointer* nfsdir as DirPtr -> Dir_ #}

{# fun nfs_closedir as close_dir { with_context* `Context'
                                 , id `DirPtr' } -> `()' #}

-- | Opaque handle representing an open directory. Should be closed after use
-- with @closeDir@ to release the associated resources. If not released explicitly
-- the garbage collector will dispose it at an unspecified point in time.
data Dir = Dir !(MVar (Maybe (Context, DirPtr)))

mk_dir :: Context -> DirPtr -> IO Dir
mk_dir ctx ptr = do
  mv <- newMVar $ Just (ctx, ptr)
  _ <- mkWeakMVar mv $ finalize_dir_mvar mv
  return $ Dir mv

finalize_dir_mvar :: MVar (Maybe (Context, DirPtr)) -> IO ()
finalize_dir_mvar mv = modifyMVar_ mv $ \mpair -> case mpair of
  Just (ctx, ptr) -> close_dir ctx ptr >> return Nothing
  Nothing -> return Nothing

-- | Explicitly release a @Dir@ handle.
closeDir :: Dir -> IO ()
closeDir (Dir mv) = finalize_dir_mvar mv

-- | Type of the callback for openDirAsync
type OpenDirCallback = Callback Dir

{# fun nfs_opendir_async as opendir_async { with_context* `Context'
                                          , withCString* `FilePath'
                                          , id `FunPtr CCallback'
                                          , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously open a directory.
openDirAsync :: Context ->
                FilePath ->
                OpenDirCallback ->
                IO (Either String ())
openDirAsync ctx path cb =
  wrap_action ctx (opendir_async ctx path) cb extract_dir
    where
      extract_dir :: DataExtractor Dir
      extract_dir _ ptr = mk_dir ctx $ castPtr ptr

{# fun nfs_opendir as opendir_sync { with_context* `Context'
                                   , withCString* `FilePath'
                                   , alloca- `DirPtr' peek* } -> `CInt' id #}

-- | Synchronously open a directory.
openDir :: Context -> FilePath -> IO (Either String Dir)
openDir ctx path = handle_ret_error'' ctx (mk_dir ctx) =<< opendir_sync ctx path

-- These ones are only available in linux/nfs3.h which isn't exactly portable.
-- So let's define a C enum here and have c2hs generate accessors / converters.
#c
enum ftype3 {
  NF3None = 0,
  NF3Reg  = 1,
  NF3Dir  = 2,
  NF3Blk  = 3,
  NF3Chr  = 4,
  NF3Link = 5,
  NF3Sock = 6,
  NF3Fifo = 7,
};
#endc

{# enum ftype3 as FType3 {upcaseFirstLetter} deriving (Eq, Show) #}

data TimeVal = TimeVal { tvSec :: EpochTime
                       , tvUSec :: CSUSeconds
                       } deriving (Eq, Show)

#c
typedef struct timeval timeval;
#endc

{# pointer* timeval as TimeValPtr -> TimeVal #}

peek_timeval_ptr :: TimeValPtr -> IO TimeVal
peek_timeval_ptr ptr = do
  sec <- {# get timeval->tv_sec #} ptr
  usec <- {# get timeval->tv_usec #} ptr
  return $ TimeVal { tvSec = fromIntegral sec
                   , tvUSec = fromIntegral usec }

poke_timeval_ptr :: TimeValPtr -> TimeVal -> IO ()
poke_timeval_ptr ptr tv = do
  {# set timeval->tv_sec #} ptr $ to_clong $ tvSec tv
  {# set timeval->tv_usec #} ptr $ to_clong $ tvUSec tv
    where
      -- Why are EpochTime (CTime) and CSUSeconds not instances of the
      -- Integral class? This is ... yucky:
      to_clong :: Real a => a -> CLong
      to_clong = fromIntegral.round.realToFrac

instance Storable TimeVal where
  sizeOf _ = {# sizeof timeval #}
  alignment _ = {# alignof timeval #}
  peek = peek_timeval_ptr
  poke = poke_timeval_ptr

-- | Directory entry.
data Dirent = Dirent { direntName :: String
                     , direntInode :: FileID
                     , direntFType3 :: FType3
                     , direntMode :: FileMode
                     , direntSize :: FileOffset
                     , direntUid :: UserID
                     , direntGid :: GroupID
                     , direntATime :: TimeVal
                     , direntMTime :: TimeVal
                     , direntCTime :: TimeVal
                     } deriving (Eq, Show)

#c
typedef struct nfsdirent nfsdirent;
#endc

{# pointer* nfsdirent as DirentPtr -> Dirent #}

peek_dirent_ptr :: DirentPtr -> IO Dirent
peek_dirent_ptr ptr = do
  name <- peekCString =<< {# get nfsdirent->name #} ptr
  inode <- {# get nfsdirent->inode #} ptr
  typ <- {# get nfsdirent->type #} ptr
  mode <- {# get nfsdirent->mode #} ptr
  size <- {# get nfsdirent->size #} ptr
  uid <- {# get nfsdirent->uid #} ptr
  gid <- {# get nfsdirent->gid #} ptr
  atime <- peek $ ptr `plusPtr` {# offsetof nfsdirent->atime #}
  mtime <- peek $ ptr `plusPtr` {# offsetof nfsdirent->mtime #}
  ctime <- peek $ ptr `plusPtr` {# offsetof nfsdirent->ctime #}
  return Dirent { direntName = name
                , direntInode = fromIntegral inode
                , direntFType3 = toEnum $ fromIntegral typ
                , direntMode = fromIntegral mode
                , direntSize = fromIntegral size
                , direntUid = fromIntegral uid
                , direntGid = fromIntegral gid
                , direntATime = atime
                , direntMTime = mtime
                , direntCTime = ctime }

poke_dirent_ptr :: DirentPtr -> Dirent -> IO ()
poke_dirent_ptr _ _ = fail "We don't write to a DirentPtr. Ever."

instance Storable Dirent where
  sizeOf _ = {# sizeof nfsdirent #}
  alignment _ = {# alignof nfsdirent #}
  peek = peek_dirent_ptr
  poke = poke_dirent_ptr

extract_maybe_dirent :: DirentPtr -> IO (Maybe Dirent)
extract_maybe_dirent ptr
  | ptr == nullPtr = return Nothing
  | otherwise = liftM Just $ peek ptr

with_dir :: Dir -> (Context -> DirPtr -> IO a) -> IO a
with_dir (Dir mv) act = withMVar mv $ \mpair -> case mpair of
  Just (ctx, dir) -> act ctx dir
  Nothing -> fail "dir was closed"

{# fun nfs_readdir as readdir { with_context* `Context'
                              , id `DirPtr' } ->
 `Maybe Dirent' extract_maybe_dirent* #}

readDir :: Dir -> IO (Maybe Dirent)
readDir dir = with_dir dir readdir

{# fun nfs_mkdir_async as mkdir_async { with_context* `Context'
                                      , withCString* `FilePath'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

-- | Type of the mkDirAsync callback.
type MkDirCallback = NoDataCallback

-- | Asynchronously create a directory.
mkDirAsync :: Context -> FilePath -> MkDirCallback -> IO (Either String ())
mkDirAsync ctx path cb =
  wrap_action ctx (mkdir_async ctx path) cb extract_nothing

{# fun nfs_mkdir as mkdir_sync { with_context* `Context'
                               , withCString* `FilePath'
                               } -> `CInt' id #}

-- | Synchronously create a directory.
mkDir :: Context -> FilePath -> IO (Either String ())
mkDir ctx path = handle_ret_error ctx =<< mkdir_sync ctx path

{# fun nfs_rmdir_async as rmdir_async { with_context* `Context'
                                      , withCString* `FilePath'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

-- | Type of the rmDirAsync callback.
type RmDirCallback = NoDataCallback

-- | Asynchronously remove a directory.
rmDirAsync :: Context -> FilePath -> RmDirCallback -> IO (Either String ())
rmDirAsync ctx path cb =
  wrap_action ctx (rmdir_async ctx path) cb extract_nothing

{# fun nfs_rmdir as rmdir_sync { with_context* `Context'
                               , withCString* `FilePath' } -> `CInt' id #}

-- | Synchronously remove a directory.
rmDir :: Context -> FilePath -> IO (Either String ())
rmDir ctx path = handle_ret_error ctx =<< rmdir_sync ctx path

-- | Type of the truncateAsync callback.
type TruncateCallback = NoDataCallback

{#fun nfs_truncate_async as truncate_async { with_context* `Context'
                                           , withCString* `FilePath'
                                           , fromIntegral `FileOffset'
                                           , id `FunPtr CCallback'
                                           , id `Ptr ()'} -> `CInt' id #}

-- | Asynchronously truncate a file.
truncateAsync :: Context ->
                 FilePath ->
                 FileOffset ->
                 TruncateCallback ->
                 IO (Either String ())
truncateAsync ctx path len cb =
  wrap_action ctx (truncate_async ctx path len) cb extract_nothing

{#fun nfs_truncate as truncate_sync { with_context* `Context'
                                    , withCString* `FilePath'
                                    , fromIntegral `FileOffset' } -> `CInt' id #}

-- | Synchronously truncate a file.
truncate :: Context -> FilePath -> FileOffset -> IO (Either String ())
truncate ctx path off = handle_ret_error ctx =<< truncate_sync ctx path off

-- | Type of the renameAsync callback.
type RenameCallback = NoDataCallback

{# fun nfs_rename_async as rename_async { with_context* `Context'
                                        , withCString* `FilePath'
                                        , withCString* `FilePath'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously rename a file or directory.
renameAsync :: Context ->
               FilePath ->
               FilePath ->
               RenameCallback ->
               IO (Either String ())
renameAsync ctx from to cb =
  wrap_action ctx (rename_async ctx from to) cb extract_nothing

{# fun nfs_rename as rename_sync { with_context* `Context'
                                 , withCString* `FilePath'
                                 , withCString* `FilePath' } -> `CInt' id #}

-- | Synchronously rename a file or directory.
rename :: Context -> FilePath -> FilePath -> IO (Either String ())
rename ctx from to = handle_ret_error ctx =<< rename_sync ctx from to

-- | Type of the symlinkAsync callback.
type SymlinkCallback = NoDataCallback

{# fun nfs_symlink_async as symlink_async { with_context* `Context'
                                          , withCString* `FilePath'
                                          , withCString* `FilePath'
                                          , id `FunPtr CCallback'
                                          , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously create a symbolic link.
symlinkAsync :: Context ->
                FilePath ->
                FilePath ->
                SymlinkCallback ->
                IO (Either String ())
symlinkAsync ctx from to cb =
  wrap_action ctx (symlink_async ctx from to) cb extract_nothing

{# fun nfs_symlink as symlink_sync { with_context* `Context'
                                   , withCString* `FilePath'
                                   , withCString* `FilePath' } -> `CInt' id #}

-- | Synchronously create a symbolic link.
symlink :: Context -> FilePath -> FilePath -> IO (Either String ())
symlink ctx from to = handle_ret_error ctx =<< symlink_sync ctx from to

-- | Type of the linkAsync callback.
type LinkCallback = NoDataCallback

{# fun nfs_link_async as link_async { with_context* `Context'
                                    , withCString* `FilePath'
                                    , withCString* `FilePath'
                                    , id `FunPtr CCallback'
                                    , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously create a hard link.
linkAsync :: Context ->
             FilePath ->
             FilePath ->
             LinkCallback ->
             IO (Either String ())
linkAsync ctx from to cb =
  wrap_action ctx (link_async ctx from to) cb extract_nothing

{# fun nfs_link as link_sync { with_context* `Context'
                                   , withCString* `FilePath'
                                   , withCString* `FilePath' } -> `CInt' id #}

-- | Synchronously create a hard link.
link :: Context -> FilePath -> FilePath -> IO (Either String ())
link ctx from to = handle_ret_error ctx =<< link_sync ctx from to

data Fh_

{# pointer* nfsfh as FhPtr -> Fh_ #}

-- Even though nfs_close is declared to return an int (< 0 indicating an error),
-- the implementation of it always returns 0 so we can simply ignore the return value.
-- We also won't bother with nfs_close_async as that does nothing more than invoking
-- the callback immediately.
{# fun nfs_close as close_fh { with_context* `Context'
                             , id `FhPtr' } -> `()' #}

-- | Opaque handle representing an open file. Should be released explicitly with
-- @closeFh@ so its resources can be reclaimed. Otherwise it will be finalized by
-- the garbage collector at an unspecified point in time.
data Fh = Fh !(MVar (Maybe (Context, FhPtr)))

mk_fh :: Context -> FhPtr -> IO Fh
mk_fh ctx ptr = do
  mv <- newMVar $ Just (ctx, ptr)
  _ <- mkWeakMVar mv $ finalize_fh_mvar mv
  return $ Fh mv

finalize_fh_mvar :: MVar (Maybe (Context, FhPtr)) -> IO ()
finalize_fh_mvar mv = modifyMVar_ mv $ \mpair -> case mpair of
  Just (ctx, ptr) -> close_fh ctx ptr >> return Nothing
  Nothing -> return Nothing

-- | Explicitly close a @Fh@.
closeFh :: Fh -> IO ()
closeFh (Fh mv) = finalize_fh_mvar mv

-- | Type of the openAsync and creatAsync callback.
type OpenFileCallback = Callback Fh

extract_fh :: Context -> DataExtractor Fh
extract_fh ctx _ ptr = mk_fh ctx $ castPtr ptr

open_mode_to_cint :: OpenMode -> CInt
open_mode_to_cint ReadOnly = 0
open_mode_to_cint WriteOnly = 1
open_mode_to_cint ReadWrite = 2

{# fun nfs_creat_async as creat_async { with_context* `Context'
                                      , withCString* `FilePath'
                                      , open_mode_to_cint `OpenMode'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously create and open a new file.
creatAsync :: Context ->
              FilePath ->
              OpenMode ->
              OpenFileCallback ->
              IO (Either String ())
creatAsync ctx path mode cb =
  wrap_action ctx (creat_async ctx path mode) cb $ extract_fh ctx

{# fun nfs_creat as creat_sync { with_context* `Context'
                               , withCString* `FilePath'
                               , open_mode_to_cint `OpenMode'
                               , alloca- `FhPtr' peek* } -> `CInt' id #}

-- | Synchronously create and open a new file.
creat :: Context -> FilePath -> OpenMode -> IO (Either String Fh)
creat ctx path mode = handle_ret_error'' ctx (mk_fh ctx) =<< creat_sync ctx path mode

{# fun nfs_open_async as open_async { with_context* `Context'
                                    , withCString* `FilePath'
                                    , open_mode_to_cint `OpenMode'
                                    , id `FunPtr CCallback'
                                    , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously open a file.
openAsync :: Context ->
             FilePath ->
             OpenMode ->
             OpenFileCallback ->
             IO (Either String ())
openAsync ctx path mode cb =
  wrap_action ctx (open_async ctx path mode) cb $ extract_fh ctx

{# fun nfs_open as open_sync { with_context* `Context'
                               , withCString* `FilePath'
                               , open_mode_to_cint `OpenMode'
                               , alloca- `FhPtr' peek* } -> `CInt' id #}

-- | Synchronously open a file.
open :: Context -> FilePath -> OpenMode -> IO (Either String Fh)
open ctx path mode = handle_ret_error'' ctx (mk_fh ctx) =<< open_sync ctx path mode

-- | Type of the writeAsync and pwriteAsync callback.
type WriteCallback = Callback CSize

extract_write_size :: DataExtractor CSize
extract_write_size size _ = return $ CSize $ fromIntegral size

-- c2hs won't allow the "BS." prefix. Oh well.
bs_as_cstring :: BS.ByteString -> (CString -> IO a) -> IO a
bs_as_cstring = BS.useAsCString

with_fh :: Fh -> (Context -> FhPtr -> IO a) -> IO a
with_fh (Fh mv) act = withMVar mv $ \mpair -> case mpair of
  Just (ctx, fh) -> act ctx fh
  Nothing -> fail "fh was closed"

{# fun nfs_write_async as write_async { with_context* `Context'
                                      , id `FhPtr'
                                      , fromIntegral `Int'
                                      , bs_as_cstring* `BS.ByteString'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously write binary data to the open file represented by @Fh@ starting
-- at the current file position. The file position is advanced by the number of
-- bytes written.
writeAsync :: Fh ->
              BS.ByteString ->
              WriteCallback ->
              IO (Either String ())
writeAsync fh bs cb = with_fh fh $ \ctx -> \fhp ->
  wrap_action ctx (write_async ctx fhp (BS.length bs) bs) cb extract_write_size

handle_write_error :: Context -> CInt -> IO (Either String CSize)
handle_write_error ctx ret = handle_ret_error' ctx (ret, fromIntegral ret)

{# fun nfs_write as write_sync { with_context* `Context'
                               , id `FhPtr'
                               , fromIntegral `Int'
                               , bs_as_cstring* `BS.ByteString' } -> `CInt' id #}

-- | Synchronously write binary data to the open file represented by @Fh@ starting
-- at the current file position. The file position is advanced by the number of
-- bytes written.
write :: Fh -> BS.ByteString -> IO (Either String CSize)
write fh bs = with_fh fh $ \ctx -> \fhp ->
  handle_write_error ctx =<< write_sync ctx fhp (BS.length bs) bs

{# fun nfs_pwrite_async as pwrite_async { with_context* `Context'
                                        , id `FhPtr'
                                        , fromIntegral `FileOffset'
                                        , fromIntegral `Int'
                                        , bs_as_cstring* `BS.ByteString'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously write binary data to the open file represented by @Fh@ starting
-- at the specified offset. The file position is *not* modified.
pwriteAsync :: Fh ->
               BS.ByteString ->
               FileOffset ->
               WriteCallback ->
               IO (Either String ())
pwriteAsync fh bs off cb = with_fh fh $ \ctx -> \fhp ->
  wrap_action ctx (pwrite_async ctx fhp off (BS.length bs) bs) cb extract_write_size

{# fun nfs_pwrite as pwrite_sync { with_context* `Context'
                                 , id `FhPtr'
                                 , fromIntegral `FileOffset'
                                 , fromIntegral `Int'
                                 , bs_as_cstring* `BS.ByteString' } -> `CInt' id #}

-- | Synchronously write binary data to the open file represented by @Fh@ starting
-- at the specified offset. The file position is *not* modified.
pwrite :: Fh -> BS.ByteString -> FileOffset -> IO (Either String CSize)
pwrite fh bs off = with_fh fh $ \ctx -> \fhp ->
  handle_write_error ctx =<< pwrite_sync ctx fhp off (BS.length bs) bs

{# fun nfs_read_async as read_async { with_context* `Context'
                                    , id `FhPtr'
                                    , fromIntegral `CSize'
                                    , id `FunPtr CCallback'
                                    , id `Ptr ()' } -> `CInt' id #}

-- | Type of the readAsync and preadAsync callback.
type ReadCallback = Callback BS.ByteString

extract_read_data :: DataExtractor BS.ByteString
extract_read_data len ptr = BS.packCStringLen (castPtr ptr, fromIntegral len)

-- | Asynchronously read the specified number of bytes of binary data from the
-- current file position. The file position is advanced by the number of bytes
-- read.
readAsync :: Fh ->
             CSize ->
             ReadCallback ->
             IO (Either String ())
readAsync fh size cb = with_fh fh $ \ctx -> \fhp ->
  wrap_action ctx (read_async ctx fhp size) cb extract_read_data

handle_read_error :: Context -> Ptr CChar -> CInt -> IO (Either String BS.ByteString)
handle_read_error ctx ptr ret = do
  bs <- BS.packCStringLen (castPtr ptr, fromIntegral ret)
  handle_ret_error' ctx (ret, bs)

{# fun nfs_read as read_sync { with_context* `Context'
                             , id `FhPtr'
                             , fromIntegral `CSize'
                             , id `Ptr CChar' } -> `CInt' id #}

-- | Synchronously read the specified number of bytes of binary data from the
-- current file position. The file position is advanced by the number of bytes
-- read.
read :: Fh -> CSize -> IO (Either String BS.ByteString)
read fh size = with_fh fh $ \ctx -> \fhp ->
  allocaBytes (fromIntegral size) $ \buf ->
    handle_read_error ctx buf =<< read_sync ctx fhp size buf

{# fun nfs_pread_async as pread_async { with_context* `Context'
                                      , id `FhPtr'
                                      , fromIntegral `FileOffset'
                                      , fromIntegral `CSize'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously read the specified number of bytes of binary data from the
-- specified offset. The file position is *not* modified.
preadAsync :: Fh ->
              CSize ->
              FileOffset ->
              ReadCallback ->
              IO (Either String ())
preadAsync fh size off cb = with_fh fh $ \ctx -> \fhp ->
  wrap_action ctx (pread_async ctx fhp off size) cb extract_read_data

{# fun nfs_pread as pread_sync { with_context* `Context'
                               , id `FhPtr'
                               , fromIntegral `FileOffset'
                               , fromIntegral `CSize'
                               , id `Ptr CChar' } -> `CInt' id #}

-- | Synchronously read the specified number of bytes of binary data from the
-- specified offset. The file position is *not* modified.
pread :: Fh -> CSize -> FileOffset -> IO (Either String BS.ByteString)
pread fh size off = with_fh fh $ \ctx -> \fhp ->
  allocaBytes (fromIntegral size) $ \buf ->
    handle_read_error ctx buf =<< pread_sync ctx fhp off size buf

-- | Type of the syncAsync callback.
type FSyncCallback = NoDataCallback

{# fun nfs_fsync_async as fsync_async { with_context* `Context'
                                      , id `FhPtr'
                                      , id `FunPtr CCallback'
                                      , id `Ptr () ' } -> `CInt' id #}

-- | Asynchronously write out the file's dirty data cached at the server.
fsyncAsync :: Fh ->
              FSyncCallback ->
              IO (Either String ())
fsyncAsync fh cb = with_fh fh $ \ctx -> \fhp ->
  wrap_action ctx (fsync_async ctx fhp) cb extract_nothing

{# fun nfs_fsync as fsync_sync { with_context* `Context'
                               , id `FhPtr' } -> `CInt' id #}

-- | Synchronously write out the file's dirty data cached at the server.
fsync :: Fh -> IO (Either String ())
fsync fh = with_fh fh $ \ctx -> \fhp ->
  handle_ret_error ctx =<< fsync_sync ctx fhp

-- | Type of the lseekAsync callback.
type LSeekCallback = Callback FileOffset

extract_file_pos :: DataExtractor FileOffset
extract_file_pos _ ptr = peek $ castPtr ptr

{#fun nfs_lseek_async as lseek_async { with_context* `Context'
                                     , id `FhPtr'
                                     , fromIntegral `FileOffset'
                                     , fromIntegral `Int'
                                     , id `FunPtr CCallback'
                                     , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously update the file position. The new position is returned.
lseekAsync :: Fh ->
              FileOffset ->
              SeekMode ->
              LSeekCallback ->
              IO (Either String ())
lseekAsync fh off mode cb = with_fh fh $ \ctx -> \fhp ->
  wrap_action ctx (lseek_async ctx fhp off $ fromEnum mode) cb extract_file_pos

{# fun nfs_lseek as lseek_sync { with_context* `Context'
                               , id `FhPtr'
                               , fromIntegral `FileOffset'
                               , fromIntegral `Int'
                               , alloca- `CULong' peek* } -> `CInt' id #}

-- | Synchronously update the file position. The new position is returned.
lseek :: Fh -> FileOffset -> SeekMode -> IO (Either String FileOffset)
lseek fh off mode = with_fh fh $ \ctx -> \fhp -> do
  (ret, pos) <- lseek_sync ctx fhp off $ fromEnum mode
  handle_ret_error' ctx (ret, fromIntegral pos)

{# fun nfs_ftruncate_async as ftruncate_async { with_context* `Context'
                                              , id `FhPtr'
                                              , fromIntegral `FileOffset'
                                              , id `FunPtr CCallback'
                                              , id `Ptr ()' } -> `CInt' id #}

{# fun nfs_get_current_offset as get_current_offset { id `FhPtr' } ->
 `FileOffset' fromIntegral #}

-- | Return the current file position.
getCurrentOffset :: Fh -> IO FileOffset
getCurrentOffset fh = with_fh fh $ \_ -> \fhp -> get_current_offset fhp

-- | Asynchronously truncate the file.
ftruncateAsync :: Fh ->
                  FileOffset ->
                  TruncateCallback ->
                  IO (Either String ())
ftruncateAsync fh len cb = with_fh fh $ \ctx -> \fhp ->
  wrap_action ctx (ftruncate_async ctx fhp len) cb extract_nothing

{# fun nfs_ftruncate as ftruncate_sync { with_context* `Context'
                                       , id `FhPtr'
                                       , fromIntegral `FileOffset' } -> `CInt' id #}

-- | Synchronously truncate the file.
ftruncate :: Fh -> FileOffset -> IO (Either String ())
ftruncate fh len = with_fh fh $ \ctx -> \fhp ->
  handle_ret_error ctx =<< ftruncate_sync ctx fhp len

-- | Type of the asyncChown callback.
type ChownCallback = NoDataCallback

{# fun nfs_chown_async as chown_async { with_context* `Context'
                                      , withCString* `FilePath'
                                      , fromIntegral `UserID'
                                      , fromIntegral `GroupID'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously change the owner and group of the specified file or directory.
chownAsync :: Context ->
              FilePath ->
              UserID ->
              GroupID ->
              ChownCallback ->
              IO (Either String ())
chownAsync ctx path uid gid cb =
  wrap_action ctx (chown_async ctx path uid gid) cb extract_nothing

{# fun nfs_chown as chown_sync { with_context* `Context'
                               , withCString* `FilePath'
                               , fromIntegral `UserID'
                               , fromIntegral `GroupID' } -> `CInt' id #}

-- | Synchronously change the owner and group of the specified file or directory.
chown :: Context -> FilePath -> UserID -> GroupID -> IO (Either String ())
chown ctx path uid gid = handle_ret_error ctx =<< chown_sync ctx path uid gid

{# fun nfs_fchown_async as fchown_async { with_context* `Context'
                                        , id `FhPtr'
                                        , fromIntegral `UserID'
                                        , fromIntegral `GroupID'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously change the owner and group of the file.
fchownAsync :: Fh ->
               UserID ->
               GroupID ->
               ChownCallback ->
               IO (Either String ())
fchownAsync fh uid gid cb = with_fh fh $ \ctx -> \fhp ->
  wrap_action ctx (fchown_async ctx fhp uid gid) cb extract_nothing

{# fun nfs_fchown as fchown_sync { with_context* `Context'
                                 , id `FhPtr'
                                 , fromIntegral `UserID'
                                 , fromIntegral `GroupID' } -> `CInt' id #}

-- | Synchronously change the owner and group of the file.
fchown :: Fh -> UserID -> GroupID -> IO (Either String ())
fchown fh uid gid = with_fh fh $ \ctx -> \fhp ->
  handle_ret_error ctx =<< fchown_sync ctx fhp uid gid

-- | Type of the chmodAsync callback.
type ChmodCallback = NoDataCallback

{# fun nfs_chmod_async as chmod_async { with_context* `Context'
                                      , withCString* `FilePath'
                                      , fromIntegral `FileMode'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously change the access permissions of the specified file.
chmodAsync :: Context ->
              FilePath ->
              FileMode ->
              ChmodCallback ->
              IO (Either String ())
chmodAsync ctx path mode cb =
  wrap_action ctx (chmod_async ctx path mode) cb extract_nothing

{# fun nfs_chmod as chmod_sync { with_context* `Context'
                               , withCString* `FilePath'
                               , fromIntegral `FileMode' } -> `CInt' id #}

-- | Synchronously change the access permissions of the specified file.
chmod :: Context -> FilePath -> FileMode -> IO (Either String ())
chmod ctx path mode = handle_ret_error ctx =<< chmod_sync ctx path mode

{# fun nfs_fchmod_async as fchmod_async { with_context* `Context'
                                        , id `FhPtr'
                                        , fromIntegral `FileMode'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously change the access permissions of the file.
fchmodAsync :: Fh ->
               FileMode ->
               ChmodCallback ->
               IO (Either String ())
fchmodAsync fh mode cb = with_fh fh $ \ctx -> \fhp ->
  wrap_action ctx (fchmod_async ctx fhp mode) cb extract_nothing

{# fun nfs_fchmod as fchmod_sync { with_context* `Context'
                                 , id `FhPtr'
                                 , fromIntegral `FileMode' } -> `CInt' id #}

-- | Synchronously change the access permissions of the file.
fchmod :: Fh -> FileMode -> IO (Either String ())
fchmod fh mode = with_fh fh $ \ctx -> \fhp ->
  handle_ret_error ctx =<< fchmod_sync ctx fhp mode

-- | Type of the asyncChdir callback.
type ChdirCallback = NoDataCallback

{# fun nfs_chdir_async as chdir_async { with_context* `Context'
                                      , withCString* `FilePath'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously change the working directory.
chdirAsync :: Context ->
              FilePath ->
              ChdirCallback ->
              IO (Either String ())
chdirAsync ctx path cb =
  wrap_action ctx (chdir_async ctx path) cb extract_nothing

{# fun nfs_chdir as chdir_sync { with_context* `Context'
                               , withCString* `FilePath' } -> `CInt' id #}

-- | Synchronously change the working directory.
chdir :: Context -> FilePath -> IO (Either String ())
chdir ctx path = handle_ret_error ctx =<< chdir_sync ctx path

{# fun nfs_getcwd as get_cwd { with_context* `Context'
                             , id `Ptr CString' } -> `()' #}

-- | Get the current working directory.
getCwd :: Context -> IO FilePath
getCwd ctx = alloca $ \ptr -> do
    get_cwd ctx ptr
    peekCString =<< peek ptr

{#enum define AccessMode' { R_OK as ReadOk
                          , W_OK as WriteOk
                          , X_OK as ExecOk
                          , F_OK as ExistsOk } deriving (Eq, Show) #}

newtype AccessMode = AccessMode Int deriving (Eq, Show)

accessModeRead :: AccessMode
accessModeRead = AccessMode $ fromEnum ReadOk

accessModeWrite :: AccessMode
accessModeWrite = AccessMode $ fromEnum WriteOk

accessModeExec :: AccessMode
accessModeExec = AccessMode $ fromEnum ExecOk

accessModeExists :: AccessMode
accessModeExists = AccessMode $ fromEnum ExistsOk

instance Monoid AccessMode where
  mempty = accessModeExists
  mappend (AccessMode a) (AccessMode b) = AccessMode (a .|. b)

-- | Type of the accessAsync callback.
type AccessCallback = NoDataCallback

from_access_mode :: AccessMode -> CInt
from_access_mode (AccessMode m) = fromIntegral m

{# fun nfs_access_async as access_async { with_context* `Context'
                                        , withCString* `FilePath'
                                        , from_access_mode `AccessMode'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously check the user's permissions for a file.
accessAsync :: Context ->
               FilePath ->
               AccessMode ->
               AccessCallback ->
               IO (Either String ())
accessAsync ctx path mode cb =
  wrap_action ctx (access_async ctx path mode) cb extract_nothing

{# fun nfs_access as access_sync { with_context* `Context'
                                 , withCString* `FilePath'
                                 , from_access_mode `AccessMode' } -> `CInt' id #}

-- | Synchronously check the user's permissions for a file.
access :: Context -> FilePath -> AccessMode -> IO (Either String ())
access ctx path mode = handle_ret_error ctx =<< access_sync ctx path mode

-- | Type of the readlinkAsync callback.
type ReadLinkCallback = Callback FilePath

extract_file_path :: DataExtractor FilePath
extract_file_path _ ptr = peekCString $ castPtr ptr

{# fun nfs_readlink_async as readlink_async { with_context* `Context'
                                            , withCString* `FilePath'
                                            , id `FunPtr CCallback'
                                            , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously resolve a symbolic link.
readlinkAsync :: Context ->
                 FilePath ->
                 ReadLinkCallback ->
                 IO (Either String ())
readlinkAsync ctx path cb =
  wrap_action ctx (readlink_async ctx path) cb extract_file_path

{# fun nfs_readlink as readlink_sync { with_context* `Context'
                                     , withCString* `FilePath'
                                     , id `Ptr CChar'
                                     , fromIntegral `Int' } -> `CInt' id #}

-- | Synchronously resolve a symbolic link.
readlink :: Context -> FilePath -> IO (Either String FilePath)
readlink ctx path =
  let
    len = 4096
  in
   allocaBytes len $ \ptr -> do
     ret <- readlink_sync ctx path ptr len
     str <- peekCStringLen (ptr, len)
     handle_ret_error' ctx (ret, str)
#c
typedef struct statvfs statvfs;
#endc

{# pointer* statvfs as StatVFSPtr -> StatVFS #}

peek_statvfs_ptr :: StatVFSPtr -> IO StatVFS
peek_statvfs_ptr ptr = do
  bsize <- {# get statvfs->f_bsize #} ptr
  frsize <- {# get statvfs->f_frsize #} ptr
  blocks <- {# get statvfs->f_blocks #} ptr
  bfree <- {# get statvfs->f_bfree #} ptr
  bavail <- {# get statvfs->f_bavail #} ptr
  files <- {# get statvfs->f_files #} ptr
  ffree <- {# get statvfs->f_ffree #} ptr
  favail <- {# get statvfs->f_favail #} ptr
  fsid <- {# get statvfs->f_fsid #} ptr
  flag <- {# get statvfs->f_flag #} ptr
  namemax <- {# get statvfs->f_namemax #} ptr
  return $ StatVFS { statVFS_bsize = bsize
                   , statVFS_frsize = frsize
                   , statVFS_blocks = fromIntegral blocks
                   , statVFS_bfree = fromIntegral bfree
                   , statVFS_bavail = fromIntegral bavail
                   , statVFS_files = fromIntegral files
                   , statVFS_ffree = fromIntegral ffree
                   , statVFS_favail = fromIntegral favail
                   , statVFS_fsid = fsid
                   , statVFS_flag = flag
                   , statVFS_namemax = namemax }

poke_statvfs_ptr :: StatVFSPtr -> StatVFS -> IO ()
poke_statvfs_ptr _ _ = fail "We don't write to a StatVFSPtr. Ever."

instance Storable StatVFS where
  sizeOf _ = {# sizeof statvfs #}
  alignment _ = {# alignof statvfs #}
  peek = peek_statvfs_ptr
  poke = poke_statvfs_ptr

-- | Type of the statVfsAsync callback.
type StatVFSCallback = Callback StatVFS

extract_statvfs :: DataExtractor StatVFS
extract_statvfs _ ptr = peek $ castPtr ptr

{#fun nfs_statvfs_async as statvfs_async { with_context* `Context'
                                         , withCString* `FilePath'
                                         , id `FunPtr CCallback'
                                         , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously query information about the NFS export.
statvfsAsync :: Context ->
                FilePath ->
                StatVFSCallback ->
                IO (Either String ())
statvfsAsync ctx path cb =
  wrap_action ctx (statvfs_async ctx path) cb extract_statvfs

{#fun nfs_statvfs as statvfs_sync { with_context* `Context'
                                  , withCString* `FilePath'
                                  , alloca- `StatVFS' peek* } -> `CInt' id #}

-- | Synchronously query information about the NFS export.
statvfs :: Context -> FilePath -> IO (Either String StatVFS)
statvfs ctx path = handle_ret_error' ctx =<< statvfs_sync ctx path

type BlockCount = Word64
type BlockSize = Word64

-- Duplicates System.Posix.Files.FileStatus which doesn't have a constructor
-- exported - bummer.
-- | Information about a file or directory on an NFS export.
data Stat = Stat { statDev :: DeviceID
                 , statIno :: FileID
                 , statMode :: FileMode
                 , statNLink :: LinkCount
                 , statUid :: UserID
                 , statGid :: GroupID
                 , statRDev :: DeviceID
                 , statSize :: FileOffset
                 , statBlkSize :: BlockSize
                 , statBlocks :: BlockCount
                 , statATime :: EpochTime
                 , statMTime :: EpochTime
                 , statCTime :: EpochTime
                 } deriving (Eq, Show)

#c
typedef struct stat stat;
#endc

-- Redoing the FileStatus functions that get the file type out of the st_mode field.
-- S_ISxyz cannot easily be used as these are macros (we could write wrappers around
-- these, but oh well).
-- Did I mention that it's a bummer we cannot reuse SystemPosix.Files.FileStatus?
{# enum define FileType { S_IFMT as TypeMask
                        , S_IFDIR as IfDir
                        , S_IFCHR as IfChr
                        , S_IFBLK as IfBlk
                        , S_IFREG as IfReg
                        , S_IFIFO as IfFifo
                        , S_IFLNK as IfLnk
                        , S_IFSOCK as IfSock
                        } deriving (Eq, Ord, Show) #}

is_type :: FileType -> Stat -> Bool
is_type t s = (fromIntegral $ statMode s) .&. (fromEnum TypeMask) == (fromEnum t)

isDirectory :: Stat -> Bool
isDirectory = is_type IfDir

isRegularFile :: Stat -> Bool
isRegularFile = is_type IfReg

isSocket :: Stat -> Bool
isSocket = is_type IfSock

isSymbolicLink :: Stat -> Bool
isSymbolicLink = is_type IfLnk

isBlockDevice :: Stat -> Bool
isBlockDevice = is_type IfBlk

isCharacterDevice :: Stat -> Bool
isCharacterDevice = is_type IfChr

isNamedPipe :: Stat -> Bool
isNamedPipe = is_type IfFifo

{# pointer* stat as StatPtr -> Stat #}

peek_stat_ptr :: StatPtr -> IO Stat
peek_stat_ptr ptr = do
  dev <- {# get stat->st_dev #} ptr
  ino <- {# get stat->st_ino #} ptr
  mode <- {# get stat->st_mode #} ptr
  nlink <- {# get stat->st_nlink #} ptr
  uid <- {# get stat->st_uid #} ptr
  gid <- {# get stat->st_gid #} ptr
  rdev <- {# get stat->st_rdev #} ptr
  size <- {# get stat->st_size #} ptr
  blksize <- {# get stat->st_blksize #} ptr
  blocks <- {# get stat->st_blocks #} ptr
  atime <- peek $ ptr `plusPtr` {# offsetof stat->st_atim #}
  mtime <- peek $ ptr `plusPtr` {# offsetof stat->st_mtim #}
  ctime <- peek $ ptr `plusPtr` {# offsetof stat->st_ctim #}
  return $ Stat { statDev = fromIntegral dev
                , statIno = fromIntegral ino
                , statMode = fromIntegral mode
                , statNLink = fromIntegral nlink
                , statUid = fromIntegral uid
                , statGid = fromIntegral gid
                , statRDev = fromIntegral rdev
                , statSize = fromIntegral size
                , statBlkSize = fromIntegral blksize
                , statBlocks = fromIntegral blocks
                , statATime = fromIntegral $ Clock.sec atime
                , statMTime = fromIntegral $ Clock.sec mtime
                , statCTime = fromIntegral $ Clock.sec ctime }

poke_stat_ptr :: StatPtr -> Stat -> IO ()
poke_stat_ptr _ _ = fail "We don't write to a StatPtr. Ever."

instance Storable Stat where
  sizeOf _ = {# sizeof stat #}
  alignment _ = {# alignof stat #}
  peek = peek_stat_ptr
  poke = poke_stat_ptr

-- | Type of the statAsync callback.
type StatCallback = Callback Stat

extract_stat :: DataExtractor Stat
extract_stat _ ptr = peek $ castPtr ptr

{# fun nfs_stat_async as stat_async { with_context* `Context'
                                    , withCString* `FilePath'
                                    , id `FunPtr CCallback'
                                    , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously query information about a file or directory.
statAsync :: Context ->
             FilePath ->
             StatCallback ->
             IO (Either String ())
statAsync ctx path cb =
  wrap_action ctx (stat_async ctx path) cb extract_stat

{# fun nfs_stat as stat_sync { with_context* `Context'
                             , withCString* `FilePath'
                             , alloca- `Stat' peek* } -> `CInt' id #}

-- | Synchronously query information about a file or directory.
stat :: Context -> FilePath -> IO (Either String Stat)
stat ctx path = handle_ret_error' ctx =<< stat_sync ctx path

{# fun nfs_fstat_async as fstat_async { with_context* `Context'
                                      , id `FhPtr'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously query information about the file.
fstatAsync :: Fh ->
              StatCallback ->
              IO (Either String ())
fstatAsync fh cb = with_fh fh $ \ctx -> \fhp ->
  wrap_action ctx (fstat_async ctx fhp) cb extract_stat

{# fun nfs_fstat as fstat_sync { with_context* `Context'
                               , id `FhPtr'
                               , alloca- `Stat' peek* } -> `CInt' id #}

-- | Synchronously query information about the file.
fstat :: Fh -> IO (Either String Stat)
fstat fh = with_fh fh $ \ctx -> \fhp ->
  handle_ret_error' ctx =<< fstat_sync ctx fhp

-- | Type of the utimesAsyncCallback.
type UTimesCallback = NoDataCallback

-- | Type of the access timestamp.
type AccessTimeVal = TimeVal

-- | Type of the modification timestamp.
type ModificationTimeVal = TimeVal

utimes_helper :: Maybe (AccessTimeVal, ModificationTimeVal) ->
                 (TimeValPtr -> IO (Either String ())) ->
                 IO (Either String ())
utimes_helper Nothing act = act nullPtr
utimes_helper (Just (atv, mtv)) act =
  let
    tvsize = sizeOf atv
  in
    allocaBytes (2 * tvsize) $ \ptr -> do
      poke ptr atv
      poke (ptr `plusPtr` tvsize) mtv
      act ptr

{# fun nfs_utimes_async as utimes_async { with_context* `Context'
                                        , withCString* `FilePath'
                                        , id `TimeValPtr'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously update the access and modification timestamps of a file.
utimesAsync :: Context ->
               FilePath ->
               Maybe (AccessTimeVal, ModificationTimeVal) ->
               UTimesCallback ->
               IO (Either String ())
utimesAsync ctx path mtvs cb = utimes_helper mtvs $ \ptr ->
  wrap_action ctx (utimes_async ctx path ptr) cb extract_nothing

{# fun nfs_utimes as utimes_sync { with_context* `Context'
                                 , withCString* `FilePath'
                                 , id `TimeValPtr' } -> `CInt' id #}

-- | Synchronously update the access and modification timestamps of a file.
utimes :: Context ->
          FilePath ->
          Maybe (AccessTimeVal, ModificationTimeVal) ->
          IO (Either String ())
utimes ctx path mtvs =
  utimes_helper mtvs $ \ptr -> handle_ret_error ctx =<< utimes_sync ctx path ptr

-- | Type of the unlinkAsync callback.
type UnlinkCallback = NoDataCallback

{# fun nfs_unlink_async as unlink_async { with_context* `Context'
                                        , withCString* `FilePath'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `CInt' id #}

-- | Asynchronously unlink a file.
unlinkAsync :: Context -> FilePath -> UnlinkCallback -> IO (Either String ())
unlinkAsync ctx path cb =
  wrap_action ctx (unlink_async ctx path) cb extract_nothing

{# fun nfs_unlink as unlink_sync { with_context* `Context'
                                 , withCString* `FilePath' } -> `CInt' id #}

-- | Synchronously unlink a file.
unlink :: Context -> FilePath -> IO (Either String ())
unlink ctx path =
  handle_ret_error ctx =<< unlink_sync ctx path

-- Local Variables: **
-- mode: haskell **
-- compile-command: "cd ../.. && cabal install -v" **
-- End: **
