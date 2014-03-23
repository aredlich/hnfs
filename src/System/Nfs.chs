-- System.Nfs
--
-- Copyright (C) 2014 Arne Redlich <arne.redlich@googlemail.com>
--
-- Licensed under the LGPL v2.1 - see the LICENSE file.
--
-- Haskell bindings to libnfs - https://github.com/sahlberg/libnfs

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
                  , utimes
                  , utimesAsync
                  , whichEvents
                  , write
                  , writeAsync
                  ) where

import Control.Monad

import Data.Bits
import qualified Data.ByteString as BS
--import Data.Either
--import Data.Maybe
import Data.Monoid
import Data.Word (Word64)

import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
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
{# pointer* nfs_context as Context -> Context_ #}

{# fun nfs_destroy_context as destroyContext { id `Context' } -> `()' #}

-- init_context :: IO Context
-- init_context = do
--   cptr <- {# call nfs_init_context #}
--   ctx <- newForeignPtr destroy_context cptr
--   return $ Context ctx

{# fun nfs_init_context as initContext {} -> `Context' id #}

maybe_error :: CString -> IO (Maybe String)
maybe_error ptr
  | ptr == nullPtr = return Nothing
  | otherwise = liftM Just $ peekCString ptr

{# fun nfs_get_error as getError { id `Context' } -> `Maybe String' maybe_error* #}

{# fun nfs_get_readmax as getReadMax { id `Context' } -> `Integer' fromIntegral #}

{# fun nfs_get_writemax as getWriteMax { id `Context' } -> `Integer' fromIntegral #}

{# fun nfs_set_tcp_syncnt as setTcpSynCount { id `Context',
                                              fromIntegral `Integer'} -> `()' #}

{# fun nfs_set_uid as setUid { id `Context',
                               fromIntegral `UserID' } -> `()' #}

{# fun nfs_set_gid as setGid { id `Context',
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

-- TODO:
-- * a nicer "show"
-- * push to a higher level API?
newtype Event = Event Int deriving (Eq, Show)

eventNone :: Event
eventNone = Event $ fromEnum PollNone

eventRead :: Event
eventRead = Event $ fromEnum PollIn

eventWrite :: Event
eventWrite = Event $ fromEnum PollOut

eventErr :: Event
eventErr = Event $ fromEnum PollErr

eventHup :: Event
eventHup = Event $ fromEnum PollHup

eventPri :: Event
eventPri = Event $ fromEnum PollPri

eventNval :: Event
eventNval = Event $ fromEnum PollNval

instance Monoid Event where
  mempty = eventNone
  mappend (Event a) (Event b) = Event (a .|. b)

to_event :: CInt -> Event
to_event e = Event $ fromIntegral e

from_event :: Event -> CInt
from_event (Event e) = fromIntegral e

{# fun nfs_which_events as whichEvents { id `Context' } -> `Event' to_event #}

{# fun nfs_service as service { id `Context'
                              , from_event `Event'
                              } -> `()' #}

{# fun nfs_get_fd as getFd { id `Context' } -> `Fd' fromIntegral #}

{# fun nfs_queue_length as queueLength { id `Context' } -> `Integer' fromIntegral #}

errmsg :: Maybe String -> String
errmsg (Just s) = s
errmsg Nothing = "unspecified error"

-- For now - while I'm still in exploratory mode - errors will be treated
-- with this (odd?) Either. At a later point exceptions could turn out to be a
-- better idea.
handle_ret_error' :: Context -> (CInt, a) -> IO (Either String a)
handle_ret_error' ctx (err, ret)
  | err >= 0 = return $ Right ret
  | otherwise = do
    mmsg <- getError ctx
    return $ Left $ errmsg mmsg

handle_ret_error :: Context -> CInt -> IO (Either String ())
handle_ret_error ctx err = handle_ret_error' ctx (err, ())

-- There's not much point in threading the Context or any private data through
-- libnfs calls as we can use partial application.
type CCallback = CInt -> -- err
                 Context -> -- we have no need for it
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

type NoDataCallback = Callback ()

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

type ServerAddress = String
type ExportName = String

extract_nothing :: DataExtractor ()
extract_nothing _ _ = return ()

-- C2HS can't go all the way for this one:
-- - I cannot seem to get the marshalling of the callback right
-- - the private_data ptr of the C call won't be used by us
-- - converting the Integer error code to our Either String () requires access
--   to the Context
{# fun nfs_mount_async as mount_async { id `Context'
                                      , withCString* `ServerAddress'
                                      , withCString* `ExportName'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

mountAsync :: Context ->
              ServerAddress ->
              ExportName ->
              MountCallback ->
              IO (Either String ())
mountAsync ctx addr export cb =
  wrap_action ctx (mount_async ctx addr export) cb extract_nothing

{# fun nfs_mount as mount_sync { id `Context'
                               , withCString* `ServerAddress'
                               , withCString* `ExportName'
                               } -> `CInt' id #}

mount :: Context -> ServerAddress -> ExportName -> IO (Either String ())
mount ctx addr xprt = handle_ret_error ctx =<< mount_sync ctx addr xprt

data Dir_

{# pointer* nfsdir as Dir -> Dir_ #}

{# fun nfs_closedir as closeDir { id `Context'
                                , id `Dir' } -> `()' #}

type OpenDirCallback = Callback Dir

{# fun nfs_opendir_async as opendir_async { id `Context'
                                          , withCString* `FilePath'
                                          , id `FunPtr CCallback'
                                          , id `Ptr ()' } -> `CInt' id #}

openDirAsync :: Context ->
                FilePath ->
                OpenDirCallback ->
                IO (Either String ())
openDirAsync ctx path cb =
  wrap_action ctx (opendir_async ctx path) cb extract_dir
    where
      extract_dir :: DataExtractor Dir
      extract_dir _ ptr = return $ castPtr ptr

{# fun nfs_opendir as opendir_sync { id `Context'
                                   , withCString* `FilePath'
                                   , alloca- `Dir' peek* } -> `CInt' id #}

openDir :: Context -> FilePath -> IO (Either String Dir)
openDir ctx path = handle_ret_error' ctx =<< opendir_sync ctx path

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

-- TODO:
-- * {a,c,m}time
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

{# fun nfs_readdir as readDir { id `Context'
                              , id `Dir' } -> `Maybe Dirent' extract_maybe_dirent* #}


{# fun nfs_mkdir_async as mkdir_async { id `Context'
                                      , withCString* `FilePath'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

type MkDirCallback = NoDataCallback

mkDirAsync :: Context -> FilePath -> MkDirCallback -> IO (Either String ())
mkDirAsync ctx path cb =
  wrap_action ctx (mkdir_async ctx path) cb extract_nothing

{# fun nfs_mkdir as mkdir_sync { id `Context'
                               , withCString* `FilePath'
                               } -> `CInt' id #}

mkDir :: Context -> FilePath -> IO (Either String ())
mkDir ctx path = handle_ret_error ctx =<< mkdir_sync ctx path

{# fun nfs_rmdir_async as rmdir_async { id `Context'
                                      , withCString* `FilePath'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

type RmDirCallback = NoDataCallback

rmDirAsync :: Context -> FilePath -> RmDirCallback -> IO (Either String ())
rmDirAsync ctx path cb =
  wrap_action ctx (rmdir_async ctx path) cb extract_nothing

{# fun nfs_rmdir as rmdir_sync { id `Context'
                               , withCString* `FilePath' } -> `CInt' id #}

rmDir :: Context -> FilePath -> IO (Either String ())
rmDir ctx path = handle_ret_error ctx =<< rmdir_sync ctx path

type TruncateCallback = NoDataCallback

{#fun nfs_truncate_async as truncate_async { id `Context'
                                           , withCString* `FilePath'
                                           , fromIntegral `FileOffset'
                                           , id `FunPtr CCallback'
                                           , id `Ptr ()'} -> `CInt' id #}

truncateAsync :: Context ->
                 FilePath ->
                 FileOffset ->
                 TruncateCallback ->
                 IO (Either String ())
truncateAsync ctx path len cb =
  wrap_action ctx (truncate_async ctx path len) cb extract_nothing

{#fun nfs_truncate as truncate_sync { id `Context'
                                    , withCString* `FilePath'
                                    , fromIntegral `FileOffset' } -> `CInt' id #}

truncate :: Context -> FilePath -> FileOffset -> IO (Either String ())
truncate ctx path off = handle_ret_error ctx =<< truncate_sync ctx path off

type RenameCallback = NoDataCallback

{# fun nfs_rename_async as rename_async { id `Context'
                                        , withCString* `FilePath'
                                        , withCString* `FilePath'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `CInt' id #}

renameAsync :: Context ->
               FilePath ->
               FilePath ->
               RenameCallback ->
               IO (Either String ())
renameAsync ctx from to cb =
  wrap_action ctx (rename_async ctx from to) cb extract_nothing

{# fun nfs_rename as rename_sync { id `Context'
                                 , withCString* `FilePath'
                                 , withCString* `FilePath' } -> `CInt' id #}

rename :: Context -> FilePath -> FilePath -> IO (Either String ())
rename ctx from to = handle_ret_error ctx =<< rename_sync ctx from to

type SymlinkCallback = NoDataCallback

{# fun nfs_symlink_async as symlink_async { id `Context'
                                          , withCString* `FilePath'
                                          , withCString* `FilePath'
                                          , id `FunPtr CCallback'
                                          , id `Ptr ()' } -> `CInt' id #}

symlinkAsync :: Context ->
                FilePath ->
                FilePath ->
                SymlinkCallback ->
                IO (Either String ())
symlinkAsync ctx from to cb =
  wrap_action ctx (symlink_async ctx from to) cb extract_nothing

{# fun nfs_symlink as symlink_sync { id `Context'
                                   , withCString* `FilePath'
                                   , withCString* `FilePath' } -> `CInt' id #}

symlink :: Context -> FilePath -> FilePath -> IO (Either String ())
symlink ctx from to = handle_ret_error ctx =<< symlink_sync ctx from to

type LinkCallback = NoDataCallback

{# fun nfs_link_async as link_async { id `Context'
                                    , withCString* `FilePath'
                                    , withCString* `FilePath'
                                    , id `FunPtr CCallback'
                                    , id `Ptr ()' } -> `CInt' id #}

linkAsync :: Context ->
             FilePath ->
             FilePath ->
             LinkCallback ->
             IO (Either String ())
linkAsync ctx from to cb =
  wrap_action ctx (link_async ctx from to) cb extract_nothing

{# fun nfs_link as link_sync { id `Context'
                                   , withCString* `FilePath'
                                   , withCString* `FilePath' } -> `CInt' id #}

link :: Context -> FilePath -> FilePath -> IO (Either String ())
link ctx from to = handle_ret_error ctx =<< link_sync ctx from to

data Fh_

{# pointer* nfsfh as Fh -> Fh_ #}

-- Even though nfs_close is declared to return an int (< 0 indicating an error),
-- the implementation of it always returns 0 so we can simply ignore the return value.
-- We also won't bother with nfs_close_async as that does nothing more than invoking
-- the callback immediately.
{# fun nfs_close as closeFh { id `Context'
                            , id `Fh' } -> `()' #}

type OpenFileCallback = Callback Fh

extract_fh :: DataExtractor Fh
extract_fh _ ptr = return $ castPtr ptr

open_mode_to_cint :: OpenMode -> CInt
open_mode_to_cint ReadOnly = 0
open_mode_to_cint WriteOnly = 1
open_mode_to_cint ReadWrite = 2

{# fun nfs_creat_async as creat_async { id `Context'
                                      , withCString* `FilePath'
                                      , open_mode_to_cint `OpenMode'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

creatAsync :: Context ->
              FilePath ->
              OpenMode ->
              OpenFileCallback ->
              IO (Either String ())
creatAsync ctx path mode cb =
  wrap_action ctx (creat_async ctx path mode) cb extract_fh

{# fun nfs_creat as creat_sync { id `Context'
                               , withCString* `FilePath'
                               , open_mode_to_cint `OpenMode'
                               , alloca- `Fh' peek* } -> `CInt' id #}

creat :: Context -> FilePath -> OpenMode -> IO (Either String Fh)
creat ctx path mode = handle_ret_error' ctx =<< creat_sync ctx path mode

{# fun nfs_open_async as open_async { id `Context'
                                    , withCString* `FilePath'
                                    , open_mode_to_cint `OpenMode'
                                    , id `FunPtr CCallback'
                                    , id `Ptr ()' } -> `CInt' id #}

openAsync :: Context ->
             FilePath ->
             OpenMode ->
             OpenFileCallback ->
             IO (Either String ())
openAsync ctx path mode cb =
  wrap_action ctx (open_async ctx path mode) cb extract_fh

{# fun nfs_open as open_sync { id `Context'
                               , withCString* `FilePath'
                               , open_mode_to_cint `OpenMode'
                               , alloca- `Fh' peek* } -> `CInt' id #}

open :: Context -> FilePath -> OpenMode -> IO (Either String Fh)
open ctx path mode = handle_ret_error' ctx =<< open_sync ctx path mode

type WriteCallback = Callback CSize

extract_write_size :: DataExtractor CSize
extract_write_size size _ = return $ CSize $ fromIntegral size

-- c2hs won't allow the "BS." prefix. Oh well.
bs_as_cstring :: BS.ByteString -> (CString -> IO a) -> IO a
bs_as_cstring = BS.useAsCString

{# fun nfs_write_async as write_async { id `Context'
                                      , id `Fh'
                                      , fromIntegral `Int'
                                      , bs_as_cstring* `BS.ByteString'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

writeAsync :: Context ->
              Fh ->
              BS.ByteString ->
              WriteCallback ->
              IO (Either String ())
writeAsync ctx fh bs cb =
  wrap_action ctx (write_async ctx fh (BS.length bs) bs) cb extract_write_size

handle_write_error :: Context -> CInt -> IO (Either String CSize)
handle_write_error ctx ret = handle_ret_error' ctx (ret, fromIntegral ret)

{# fun nfs_write as write_sync { id `Context'
                               , id `Fh'
                               , fromIntegral `Int'
                               , bs_as_cstring* `BS.ByteString' } -> `CInt' id #}

write :: Context -> Fh -> BS.ByteString -> IO (Either String CSize)
write ctx fh bs = handle_write_error ctx =<< write_sync ctx fh (BS.length bs) bs

{# fun nfs_pwrite_async as pwrite_async { id `Context'
                                        , id `Fh'
                                        , fromIntegral `FileOffset'
                                        , fromIntegral `Int'
                                        , bs_as_cstring* `BS.ByteString'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `CInt' id #}

pwriteAsync :: Context ->
               Fh ->
               BS.ByteString ->
               FileOffset ->
               WriteCallback ->
               IO (Either String ())
pwriteAsync ctx fh bs off cb =
  wrap_action ctx (pwrite_async ctx fh off (BS.length bs) bs) cb extract_write_size

{# fun nfs_pwrite as pwrite_sync { id `Context'
                                 , id `Fh'
                                 , fromIntegral `FileOffset'
                                 , fromIntegral `Int'
                                 , bs_as_cstring* `BS.ByteString' } -> `CInt' id #}

pwrite :: Context -> Fh -> BS.ByteString -> FileOffset -> IO (Either String CSize)
pwrite ctx fh bs off =
  handle_write_error ctx =<< pwrite_sync ctx fh off (BS.length bs) bs

{# fun nfs_read_async as read_async { id `Context'
                                    , id `Fh'
                                    , fromIntegral `Word64'
                                    , id `FunPtr CCallback'
                                    , id `Ptr ()' } -> `CInt' id #}

type ReadCallback = Callback BS.ByteString

extract_read_data :: DataExtractor BS.ByteString
extract_read_data len ptr = BS.packCStringLen (castPtr ptr, fromIntegral len)

readAsync :: Context ->
             Fh ->
             Word64 ->
             ReadCallback ->
             IO (Either String ())
readAsync ctx fh size cb =
  wrap_action ctx (read_async ctx fh size) cb extract_read_data

handle_read_error :: Context -> Ptr CChar -> CInt -> IO (Either String BS.ByteString)
handle_read_error ctx ptr ret = do
  bs <- BS.packCStringLen (castPtr ptr, fromIntegral ret)
  handle_ret_error' ctx (ret, bs)

{# fun nfs_read as read_sync { id `Context'
                             , id `Fh'
                             , fromIntegral `Word64'
                             , id `Ptr CChar' } -> `CInt' id #}

read :: Context -> Fh -> Word64 -> IO (Either String BS.ByteString)
read ctx fh size = allocaBytes (fromIntegral size) $ \buf -> do
  handle_read_error ctx buf =<< read_sync ctx fh size buf

{# fun nfs_pread_async as pread_async { id `Context'
                                      , id `Fh'
                                      , fromIntegral `FileOffset'
                                      , fromIntegral `CSize'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

preadAsync :: Context ->
              Fh ->
              CSize ->
              FileOffset ->
              ReadCallback ->
              IO (Either String ())
preadAsync ctx fh size off cb =
  wrap_action ctx (pread_async ctx fh off size) cb extract_read_data

{# fun nfs_pread as pread_sync { id `Context'
                               , id `Fh'
                               , fromIntegral `FileOffset'
                               , fromIntegral `CSize'
                               , id `Ptr CChar' } -> `CInt' id #}

pread :: Context -> Fh -> CSize -> FileOffset -> IO (Either String BS.ByteString)
pread ctx fh size off = allocaBytes (fromIntegral size) $ \buf -> do
  handle_read_error ctx buf =<< pread_sync ctx fh off size buf

type FSyncCallback = NoDataCallback

{# fun nfs_fsync_async as fsync_async { id `Context'
                                      , id `Fh'
                                      , id `FunPtr CCallback'
                                      , id `Ptr () ' } -> `CInt' id #}

fsyncAsync :: Context ->
              Fh ->
              FSyncCallback ->
              IO (Either String ())
fsyncAsync ctx fh cb =
  wrap_action ctx (fsync_async ctx fh) cb extract_nothing

{# fun nfs_fsync as fsync_sync { id `Context'
                               , id `Fh' } -> `CInt' id #}

fsync :: Context -> Fh -> IO (Either String ())
fsync ctx fh = handle_ret_error ctx =<< fsync_sync ctx fh

type LSeekCallback = Callback FileOffset

extract_file_pos :: DataExtractor FileOffset
extract_file_pos _ ptr = peek $ castPtr ptr

{#fun nfs_lseek_async as lseek_async { id `Context'
                                     , id `Fh'
                                     , fromIntegral `FileOffset'
                                     , fromIntegral `Int'
                                     , id `FunPtr CCallback'
                                     , id `Ptr ()' } -> `CInt' id #}

lseekAsync :: Context ->
              Fh ->
              FileOffset ->
              SeekMode ->
              LSeekCallback ->
              IO (Either String ())
lseekAsync ctx fh off mode cb =
  wrap_action ctx (lseek_async ctx fh off $ fromEnum mode) cb extract_file_pos

{# fun nfs_lseek as lseek_sync { id `Context'
                               , id `Fh'
                               , fromIntegral `FileOffset'
                               , fromIntegral `Int'
                               , alloca- `CULong' peek* } -> `CInt' id #}

lseek :: Context -> Fh -> FileOffset -> SeekMode -> IO (Either String FileOffset)
lseek ctx fh off mode = do
  (ret, pos) <- lseek_sync ctx fh off $ fromEnum mode
  handle_ret_error' ctx (ret, fromIntegral pos)

{# fun nfs_ftruncate_async as ftruncate_async { id `Context'
                                              , id `Fh'
                                              , fromIntegral `FileOffset'
                                              , id `FunPtr CCallback'
                                              , id `Ptr ()' } -> `CInt' id #}

ftruncateAsync :: Context ->
                  Fh ->
                  FileOffset ->
                  TruncateCallback ->
                  IO (Either String ())
ftruncateAsync ctx fh len cb =
  wrap_action ctx (ftruncate_async ctx fh len) cb extract_nothing

{# fun nfs_get_current_offset as getCurrentOffset { id `Fh' } -> `FileOffset' fromIntegral #}

{# fun nfs_ftruncate as ftruncate_sync { id `Context'
                                       , id `Fh'
                                       , fromIntegral `FileOffset' } -> `CInt' id #}

ftruncate :: Context -> Fh -> FileOffset -> IO (Either String ())
ftruncate ctx fh len = handle_ret_error ctx =<< ftruncate_sync ctx fh len

type ChownCallback = NoDataCallback

{# fun nfs_chown_async as chown_async { id `Context'
                                      , withCString* `FilePath'
                                      , fromIntegral `UserID'
                                      , fromIntegral `GroupID'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

chownAsync :: Context ->
              FilePath ->
              UserID ->
              GroupID ->
              ChownCallback ->
              IO (Either String ())
chownAsync ctx path uid gid cb =
  wrap_action ctx (chown_async ctx path uid gid) cb extract_nothing

{# fun nfs_chown as chown_sync { id `Context'
                               , withCString* `FilePath'
                               , fromIntegral `UserID'
                               , fromIntegral `GroupID' } -> `CInt' id #}

chown :: Context -> FilePath -> UserID -> GroupID -> IO (Either String ())
chown ctx path uid gid = handle_ret_error ctx =<< chown_sync ctx path uid gid

{# fun nfs_fchown_async as fchown_async { id `Context'
                                        , id `Fh'
                                        , fromIntegral `UserID'
                                        , fromIntegral `GroupID'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `CInt' id #}

fchownAsync :: Context ->
               Fh ->
               UserID ->
               GroupID ->
               ChownCallback ->
               IO (Either String ())
fchownAsync ctx fh uid gid cb =
  wrap_action ctx (fchown_async ctx fh uid gid) cb extract_nothing

{# fun nfs_fchown as fchown_sync { id `Context'
                                 , id `Fh'
                                 , fromIntegral `UserID'
                                 , fromIntegral `GroupID' } -> `CInt' id #}

fchown :: Context -> Fh -> UserID -> GroupID -> IO (Either String ())
fchown ctx fh uid gid = handle_ret_error ctx =<< fchown_sync ctx fh uid gid

type ChmodCallback = NoDataCallback

{# fun nfs_chmod_async as chmod_async { id `Context'
                                      , withCString* `FilePath'
                                      , fromIntegral `FileMode'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

chmodAsync :: Context ->
              FilePath ->
              FileMode ->
              ChmodCallback ->
              IO (Either String ())
chmodAsync ctx path mode cb =
  wrap_action ctx (chmod_async ctx path mode) cb extract_nothing

{# fun nfs_chmod as chmod_sync { id `Context'
                               , withCString* `FilePath'
                               , fromIntegral `FileMode' } -> `CInt' id #}

chmod :: Context -> FilePath -> FileMode -> IO (Either String ())
chmod ctx path mode = handle_ret_error ctx =<< chmod_sync ctx path mode

{# fun nfs_fchmod_async as fchmod_async { id `Context'
                                        , id `Fh'
                                        , fromIntegral `FileMode'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `CInt' id #}

fchmodAsync :: Context ->
               Fh ->
               FileMode ->
               ChmodCallback ->
               IO (Either String ())
fchmodAsync ctx fh mode cb =
  wrap_action ctx (fchmod_async ctx fh mode) cb extract_nothing

{# fun nfs_fchmod as fchmod_sync { id `Context'
                                 , id `Fh'
                                 , fromIntegral `FileMode' } -> `CInt' id #}

fchmod :: Context -> Fh -> FileMode -> IO (Either String ())
fchmod ctx fh mode = handle_ret_error ctx =<< fchmod_sync ctx fh mode

type ChdirCallback = NoDataCallback

{# fun nfs_chdir_async as chdir_async { id `Context'
                                      , withCString* `FilePath'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

chdirAsync :: Context ->
              FilePath ->
              ChdirCallback ->
              IO (Either String ())
chdirAsync ctx path cb =
  wrap_action ctx (chdir_async ctx path) cb extract_nothing

{# fun nfs_chdir as chdir_sync { id `Context'
                               , withCString* `FilePath' } -> `CInt' id #}

chdir :: Context -> FilePath -> IO (Either String ())
chdir ctx path = handle_ret_error ctx =<< chdir_sync ctx path

{# fun nfs_getcwd as get_cwd { id `Context'
                             , id `Ptr CString' } -> `()' #}

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


type AccessCallback = NoDataCallback

from_access_mode :: AccessMode -> CInt
from_access_mode (AccessMode m) = fromIntegral m

{# fun nfs_access_async as access_async { id `Context'
                                        , withCString* `FilePath'
                                        , from_access_mode `AccessMode'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `CInt' id #}

accessAsync :: Context ->
               FilePath ->
               AccessMode ->
               AccessCallback ->
               IO (Either String ())
accessAsync ctx path mode cb =
  wrap_action ctx (access_async ctx path mode) cb extract_nothing

{# fun nfs_access as access_sync { id `Context'
                                 , withCString* `FilePath'
                                 , from_access_mode `AccessMode' } -> `CInt' id #}

access :: Context -> FilePath -> AccessMode -> IO (Either String ())
access ctx path mode = handle_ret_error ctx =<< access_sync ctx path mode

type ReadLinkCallback = Callback FilePath

extract_file_path :: DataExtractor FilePath
extract_file_path _ ptr = peekCString $ castPtr ptr

{# fun nfs_readlink_async as readlink_async { id `Context'
                                            , withCString* `FilePath'
                                            , id `FunPtr CCallback'
                                            , id `Ptr ()' } -> `CInt' id #}

readlinkAsync :: Context ->
                 FilePath ->
                 ReadLinkCallback ->
                 IO (Either String ())
readlinkAsync ctx path cb =
  wrap_action ctx (readlink_async ctx path) cb extract_file_path

{# fun nfs_readlink as readlink_sync { id `Context'
                                     , withCString* `FilePath'
                                     , id `Ptr CChar'
                                     , fromIntegral `Int' } -> `CInt' id #}

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

type StatVFSCallback = Callback StatVFS

extract_statvfs :: DataExtractor StatVFS
extract_statvfs _ ptr = peek $ castPtr ptr

{#fun nfs_statvfs_async as statvfs_async { id `Context'
                                         , withCString* `FilePath'
                                         , id `FunPtr CCallback'
                                         , id `Ptr ()' } -> `CInt' id #}

statvfsAsync :: Context ->
                FilePath ->
                StatVFSCallback ->
                IO (Either String ())
statvfsAsync ctx path cb =
  wrap_action ctx (statvfs_async ctx path) cb extract_statvfs

{#fun nfs_statvfs as statvfs_sync { id `Context'
                                  , withCString* `FilePath'
                                  , alloca- `StatVFS' peek* } -> `CInt' id #}

statvfs :: Context -> FilePath -> IO (Either String StatVFS)
statvfs ctx path = handle_ret_error' ctx =<< statvfs_sync ctx path

type BlockCount = Word64
type BlockSize = Word64

-- Duplicates System.Posix.Files.FileStatus which doesn't have a constructor
-- exported - bummer.
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

  -- TODO:
  -- src/System/Nfs.chs:975: (column 25) [ERROR]  >>> Unknown member name!
  -- The structure has no member called `st_atime'.  The structure is defined at
  -- ("/usr/include/x86_64-linux-gnu/bits/stat.h",46,1).

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

type StatCallback = Callback Stat

extract_stat :: DataExtractor Stat
extract_stat _ ptr = peek $ castPtr ptr

{# fun nfs_stat_async as stat_async { id `Context'
                                    , withCString* `FilePath'
                                    , id `FunPtr CCallback'
                                    , id `Ptr ()' } -> `CInt' id #}

statAsync :: Context ->
             FilePath ->
             StatCallback ->
             IO (Either String ())
statAsync ctx path cb =
  wrap_action ctx (stat_async ctx path) cb extract_stat

{# fun nfs_stat as stat_sync { id `Context'
                             , withCString* `FilePath'
                             , alloca- `Stat' peek* } -> `CInt' id #}

stat :: Context -> FilePath -> IO (Either String Stat)
stat ctx path = handle_ret_error' ctx =<< stat_sync ctx path

{# fun nfs_fstat_async as fstat_async { id `Context'
                                      , id `Fh'
                                      , id `FunPtr CCallback'
                                      , id `Ptr ()' } -> `CInt' id #}

fstatAsync :: Context ->
              Fh ->
              StatCallback ->
              IO (Either String ())
fstatAsync ctx fh cb =
  wrap_action ctx (fstat_async ctx fh) cb extract_stat

{# fun nfs_fstat as fstat_sync { id `Context'
                               , id `Fh'
                               , alloca- `Stat' peek* } -> `CInt' id #}

fstat :: Context -> Fh -> IO (Either String Stat)
fstat ctx fh = handle_ret_error' ctx =<< fstat_sync ctx fh

type UTimesCallback = NoDataCallback

type AccessTimeVal = TimeVal
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

{# fun nfs_utimes_async as utimes_async { id `Context'
                                        , withCString* `FilePath'
                                        , id `TimeValPtr'
                                        , id `FunPtr CCallback'
                                        , id `Ptr ()' } -> `CInt' id #}

utimesAsync :: Context ->
               FilePath ->
               Maybe (AccessTimeVal, ModificationTimeVal) ->
               UTimesCallback ->
               IO (Either String ())
utimesAsync ctx path mtvs cb = utimes_helper mtvs $ \ptr ->
  wrap_action ctx (utimes_async ctx path ptr) cb extract_nothing

{# fun nfs_utimes as utimes_sync { id `Context'
                                 , withCString* `FilePath'
                                 , id `TimeValPtr' } -> `CInt' id #}

utimes :: Context ->
          FilePath ->
          Maybe (AccessTimeVal, ModificationTimeVal) ->
          IO (Either String ())
utimes ctx path mtvs =
  utimes_helper mtvs $ \ptr -> handle_ret_error ctx =<< utimes_sync ctx path ptr

-- Local Variables: **
-- mode: haskell **
-- compile-command: "cd ../.. && cabal install -v" **
-- End: **
