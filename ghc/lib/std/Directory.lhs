%
% (c) The AQUA Project, Glasgow University, 1994-1999
%
\section[Directory]{Directory interface}

A directory contains a series of entries, each of which is a named
reference to a file system object (file, directory etc.).  Some
entries may be hidden, inaccessible, or have some administrative
function (e.g. "." or ".." under POSIX), but in this standard all such
entries are considered to form part of the directory contents.
Entries in sub-directories are not, however, considered to form part
of the directory contents.

Each file system object is referenced by a {\em path}.  There is
normally at least one absolute path to each file system object.  In
some operating systems, it may also be possible to have paths which
are relative to the current directory.

\begin{code}
{-# OPTIONS -#include <sys/stat.h> -#include <dirent.h> -#include "cbits/stgio.h" #-}
module Directory 
   ( 
      Permissions(Permissions,readable,writable,executable,searchable)

    , createDirectory		-- :: FilePath -> IO ()
    , removeDirectory		-- :: FilePath -> IO ()
    , renameDirectory		-- :: FilePath -> FilePath -> IO ()

    , getDirectoryContents      -- :: FilePath -> IO [FilePath]
    , getCurrentDirectory       -- :: IO FilePath
    , setCurrentDirectory       -- :: FilePath -> IO ()

    , removeFile		-- :: FilePath -> IO ()
    , renameFile                -- :: FilePath -> FilePath -> IO ()

    , doesFileExist		-- :: FilePath -> IO Bool
    , doesDirectoryExist        -- :: FilePath -> IO Bool

    , getPermissions            -- :: FilePath -> IO Permissions
    , setPermissions	        -- :: FilePath -> Permissions -> IO ()


#ifndef __HUGS__
    , getModificationTime       -- :: FilePath -> IO ClockTime
#endif
   ) where

#ifdef __HUGS__
import PreludeBuiltin
#else
import PrelBase
import PrelIOBase
import PrelHandle	
import PrelST
import PrelArr
import PrelPack		( unpackNBytesST, packString, unpackCStringST )
import PrelAddr
import Time             ( ClockTime(..) )
#endif

\end{code}

%*********************************************************
%*							*
\subsection{Permissions}
%*							*
%*********************************************************

The @Permissions@ type is used to record whether certain
operations are permissible on a file/directory:
[to whom? - owner/group/world - the Report don't say much]

\begin{code}
data Permissions
 = Permissions {
    readable,   writable, 
    executable, searchable :: Bool 
   } deriving (Eq, Ord, Read, Show)
\end{code}

%*********************************************************
%*							*
\subsection{Implementation}
%*							*
%*********************************************************

@createDirectory dir@ creates a new directory {\em dir} which is
initially empty, or as near to empty as the operating system
allows.

The operation may fail with:

\begin{itemize}
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES]@
\item @isAlreadyExistsError@ / @AlreadyExists@
The operand refers to a directory that already exists.  
@ [EEXIST]@
\item @HardwareFault@
A physical I/O error has occurred.
@ [EIO]@
\item @InvalidArgument@
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@
\item @NoSuchThing@
There is no path to the directory. 
@[ENOENT, ENOTDIR]@
\item @ResourceExhausted@
Insufficient resources (virtual memory, process file descriptors,
physical disk space, etc.) are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
\item @InappropriateType@
The path refers to an existing non-directory object.
@[EEXIST]@
\end{itemize}

\begin{code}
createDirectory :: FilePath -> IO ()
createDirectory path = do
    rc <- primCreateDirectory (primPackString path)
    if rc == 0 then return () else
        constructErrorAndFailWithInfo "createDirectory" path
\end{code}

@removeDirectory dir@ removes an existing directory {\em dir}.  The
implementation may specify additional constraints which must be
satisfied before a directory can be removed (e.g. the directory has to
be empty, or may not be in use by other processes).  It is not legal
for an implementation to partially remove a directory unless the
entire directory is removed. A conformant implementation need not
support directory removal in all situations (e.g. removal of the root
directory).

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
[@EIO@]
\item @InvalidArgument@
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@
\item @isDoesNotExist@ / @NoSuchThing@
The directory does not exist. 
@[ENOENT, ENOTDIR]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@
\item @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.  
@[EBUSY, ENOTEMPTY, EEXIST]@
\item @UnsupportedOperation@
The implementation does not support removal in this situation.
@[EINVAL]@
\item @InappropriateType@
The operand refers to an existing non-directory object.
@[ENOTDIR]@
\end{itemize}

\begin{code}
removeDirectory :: FilePath -> IO ()
removeDirectory path = do
    rc <- primRemoveDirectory (primPackString path)
    if rc == 0 then 
	return ()
     else 
        constructErrorAndFailWithInfo "removeDirectory" path
\end{code}

@removeFile file@ removes the directory entry for an existing file
{\em file}, where {\em file} is not itself a directory. The
implementation may specify additional constraints which must be
satisfied before a file can be removed (e.g. the file may not be in
use by other processes).

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
@[EIO]@
\item @InvalidArgument@
The operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@
\item @isDoesNotExist@ / @NoSuchThing@
The file does not exist. 
@[ENOENT, ENOTDIR]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@
\item @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.  
@[EBUSY]@
\item @InappropriateType@
The operand refers to an existing directory.
@[EPERM, EINVAL]@
\end{itemize}

\begin{code}
removeFile :: FilePath -> IO ()
removeFile path = do
    rc <- primRemoveFile (primPackString path)
    if rc == 0 then
        return ()
     else
        constructErrorAndFailWithInfo "removeFile" path
\end{code}

@renameDirectory old@ {\em new} changes the name of an existing
directory from {\em old} to {\em new}.  If the {\em new} directory
already exists, it is atomically replaced by the {\em old} directory.
If the {\em new} directory is neither the {\em old} directory nor an
alias of the {\em old} directory, it is removed as if by
$removeDirectory$.  A conformant implementation need not support
renaming directories in all situations (e.g. renaming to an existing
directory, or across different physical devices), but the constraints
must be documented.

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
@[EIO]@
\item @InvalidArgument@
Either operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@
\item @isDoesNotExistError@ / @NoSuchThing@
The original directory does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@
\item @ResourceExhausted@
Insufficient resources are available to perform the operation.  
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
\item @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.
@[EBUSY, ENOTEMPTY, EEXIST]@
\item @UnsupportedOperation@
The implementation does not support renaming in this situation.
@[EINVAL, EXDEV]@
\item @InappropriateType@
Either path refers to an existing non-directory object.
@[ENOTDIR, EISDIR]@
\end{itemize}

\begin{code}
renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory opath npath = do
    rc <- primRenameDirectory (primPackString opath) (primPackString npath)
    if rc == 0 then
        return ()
     else
        constructErrorAndFailWithInfo "renameDirectory" ("old: " ++ opath ++ ",new: " ++ npath)
\end{code}

@renameFile old@ {\em new} changes the name of an existing file system
object from {\em old} to {\em new}.  If the {\em new} object already
exists, it is atomically replaced by the {\em old} object.  Neither
path may refer to an existing directory.  A conformant implementation
need not support renaming files in all situations (e.g. renaming
across different physical devices), but the constraints must be
documented.

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
@[EIO]@
\item @InvalidArgument@
Either operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@
\item @isDoesNotExistError@ / @NoSuchThing@
The original file does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@
\item @ResourceExhausted@
Insufficient resources are available to perform the operation.  
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
\item @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.
@[EBUSY]@
\item @UnsupportedOperation@
The implementation does not support renaming in this situation.
@[EXDEV]@
\item @InappropriateType@
Either path refers to an existing directory.
@[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@
\end{itemize}

\begin{code}
renameFile :: FilePath -> FilePath -> IO ()
renameFile opath npath = do
    rc <- primRenameFile (primPackString opath) (primPackString npath)
    if rc == 0 then
        return ()
     else
        constructErrorAndFailWithInfo	"renameFile" opath
\end{code}

@getDirectoryContents dir@ returns a list of {\em all} entries
in {\em dir}. 

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
@[EIO]@
\item @InvalidArgument@
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@
\item @isDoesNotExistError@ / @NoSuchThing@
The directory does not exist.
@[ENOENT, ENOTDIR]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EACCES]@
\item @ResourceExhausted@
Insufficient resources are available to perform the operation.
@[EMFILE, ENFILE]@
\item @InappropriateType@
The path refers to an existing non-directory object.
@[ENOTDIR]@
\end{itemize}

\begin{code}
getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path = do
    dir <- primOpenDir (primPackString path)
    if dir == nullAddr
	then constructErrorAndFailWithInfo "getDirectoryContents" path
     	else loop dir
  where
    loop :: Addr -> IO [String]
    loop dir  = do
      dirent_ptr <- primReadDir dir
      if dirent_ptr == nullAddr
       then do
	  -- readDir__ implicitly performs closedir() when the
	  -- end is reached.
	  return [] 
       else do
          str     <- primGetDirentDName dirent_ptr
	  entry   <- primUnpackCString str
	  entries <- loop dir
          return (entry:entries)
\end{code}

If the operating system has a notion of current directories,
@getCurrentDirectory@ returns an absolute path to the
current directory of the calling process.

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
@[EIO]@
\item @isDoesNotExistError@ / @NoSuchThing@
There is no path referring to the current directory.
@[EPERM, ENOENT, ESTALE...]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EACCES]@
\item @ResourceExhausted@
Insufficient resources are available to perform the operation.
\item @UnsupportedOperation@
The operating system has no notion of current directory.
\end{itemize}

\begin{code}
getCurrentDirectory :: IO FilePath
getCurrentDirectory = do
    str <- primGetCurrentDirectory
    if str /= nullAddr
	then do
            pwd <- primUnpackCString str
            primFree str
            return pwd
    	else
            constructErrorAndFail "getCurrentDirectory"
\end{code}

If the operating system has a notion of current directories,
@setCurrentDirectory dir@ changes the current
directory of the calling process to {\em dir}.

The operation may fail with:
\begin{itemize}
\item @HardwareFault@
A physical I/O error has occurred.
@[EIO]@
\item @InvalidArgument@
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@
\item @isDoesNotExistError@ / @NoSuchThing@
The directory does not exist.
@[ENOENT, ENOTDIR]@
\item @isPermissionError@ / @PermissionDenied@
The process has insufficient privileges to perform the operation.
@[EACCES]@
\item @UnsupportedOperation@
The operating system has no notion of current directory, or the
current directory cannot be dynamically changed.
\item @InappropriateType@
The path refers to an existing non-directory object.
@[ENOTDIR]@
\end{itemize}

\begin{code}
setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory path = do
    rc <- primSetCurrentDirectory (primPackString path)
    if rc == 0 
	then return ()
	else constructErrorAndFailWithInfo "setCurrentDirectory" path
\end{code}

To clarify, @doesDirectoryExist@ returns True if a file system object
exist, and it's a directory. @doesFileExist@ returns True if the file
system object exist, but it's not a directory (i.e., for every other 
file system object that is not a directory.) 

\begin{code}
doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist name = 
 catch
   (getFileStatus name >>= \ st -> return (isDirectory st))
   (\ _ -> return False)

doesFileExist :: FilePath -> IO Bool
doesFileExist name = do 
 catch
   (getFileStatus name >>= \ st -> return (not (isDirectory st)))
   (\ _ -> return False)

foreign import ccall "libHS_cbits.so" "const_F_OK" const_F_OK  :: Int

#ifndef __HUGS__
getModificationTime :: FilePath -> IO ClockTime
getModificationTime name =
 getFileStatus name >>= \ st ->
 modificationTime st
#endif

getPermissions :: FilePath -> IO Permissions
getPermissions name = do
  st <- getFileStatus name
  let
   fm = fileMode st
   isect v = intersectFileMode v fm == v

  return (
    Permissions {
      readable   = isect ownerReadMode,
      writable   = isect ownerWriteMode,
      executable = not (isDirectory st)   && isect ownerExecuteMode,
      searchable = not (isRegularFile st) && isect ownerExecuteMode
    }
   )

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions name (Permissions r w e s) = do
    let
     read  = if r      then ownerReadMode    else emptyFileMode
     write = if w      then ownerWriteMode   else emptyFileMode
     exec  = if e || s then ownerExecuteMode else emptyFileMode

     mode  = read `unionFileMode` (write `unionFileMode` exec)

    rc <- primChmod (primPackString name) mode
    if rc == 0
	then return ()
	else ioError (IOError Nothing SystemError "setPermissions" "insufficient permissions")
\end{code}

(Sigh)..copied from Posix.Files to avoid dep. on posix library

\begin{code}
type FileStatus = PrimByteArray

getFileStatus :: FilePath -> IO FileStatus
getFileStatus name = do
    bytes <- primNewByteArray sizeof_stat
    rc <- primStat (primPackString name) bytes
    if rc == 0 
#ifdef __HUGS__
	then primUnsafeFreezeByteArray bytes
#else
	then stToIO (unsafeFreezeByteArray bytes)
#endif
     	else ioError (IOError Nothing SystemError "getFileStatus" "")

#ifndef __HUGS__
modificationTime :: FileStatus -> IO ClockTime
modificationTime stat = do
      -- ToDo: better, this is ugly stuff.
    i1 <- malloc1
    setFileMode i1 stat
    secs <- cvtUnsigned i1
    return (TOD secs 0)
  where
    malloc1 = IO $ \ s# ->
	case newIntArray# 1# s# of 
          (# s2#, barr# #) -> (# s2#, MutableByteArray bnds barr# #)

    bnds = (0,1)
    -- The C routine fills in an unsigned word.  We don't have `unsigned2Integer#,'
    -- so we freeze the data bits and use them for an MP_INT structure.  Note that
    -- zero is still handled specially, although (J# 1# 1# (ptr to 0#)) is probably
    -- acceptable to gmp.

    cvtUnsigned (MutableByteArray _ arr#) = IO $ \ s# ->
	case readIntArray# arr# 0# s# of 
	  (# s2#, r# #) ->
            if r# ==# 0# then
                (# s2#, 0 #)
            else
                case unsafeFreezeByteArray# arr# s2# of
                  (# s3#, frozen# #) -> 
			(# s3#, J# 1# 1# frozen# #)

foreign import ccall "libHS_cbits.so" "set_stat_st_mtime" 
   setFileMode :: PrimMutableByteArray RealWorld -> FileStatus -> IO ()

#endif

isDirectory :: FileStatus -> Bool
isDirectory stat = prim_S_ISDIR (fileMode stat) /= 0

isRegularFile :: FileStatus -> Bool
isRegularFile stat = prim_S_ISREG (fileMode stat) /= 0

foreign import ccall "libHS_cbits.so" "sizeof_stat" sizeof_stat :: Int
foreign import ccall "libHS_cbits.so" "prim_stat"   primStat :: PrimByteArray -> PrimMutableByteArray RealWorld -> IO Int

foreign import ccall "libHS_cbits.so" "get_stat_st_mode" fileMode     :: FileStatus -> FileMode
foreign import ccall "libHS_cbits.so" "prim_S_ISDIR"     prim_S_ISDIR :: FileMode -> Int
foreign import ccall "libHS_cbits.so" "prim_S_ISREG"     prim_S_ISREG :: FileMode -> Int
\end{code}

\begin{code}
type FileMode = Word

emptyFileMode     :: FileMode
unionFileMode     :: FileMode -> FileMode -> FileMode
intersectFileMode :: FileMode -> FileMode -> FileMode

foreign import ccall "libHS_cbits.so" "const_S_IRUSR" unsafe ownerReadMode    :: FileMode
foreign import ccall "libHS_cbits.so" "const_S_IWUSR" unsafe ownerWriteMode   :: FileMode
foreign import ccall "libHS_cbits.so" "const_S_IXUSR" unsafe ownerExecuteMode :: FileMode

#ifdef __HUGS__
emptyFileMode     = primIntToWord 0
unionFileMode     = primOrWord
intersectFileMode = primAndWord
#else
--ToDo: tidy up.
emptyFileMode     = W# (int2Word# 0#)
unionFileMode     = orWord
intersectFileMode = andWord
#endif

\end{code}

Some defns. to allow us to share code.

\begin{code}
#ifndef __HUGS__

primPackString :: [Char] -> ByteArray Int
primPackString    = packString
--ToDo: fix.
primUnpackCString :: Addr -> IO String
primUnpackCString a = stToIO (unpackCStringST a)

type PrimByteArray = ByteArray Int
type PrimMutableByteArray s = MutableByteArray RealWorld Int
type CString = PrimByteArray

orWord, andWord :: Word -> Word -> Word
orWord (W# x#) (W# y#) = W# (x# `or#` y#)
andWord (W# x#) (W# y#) = W# (x# `and#` y#)

primNewByteArray :: Int -> IO (PrimMutableByteArray s)
primNewByteArray sz_in_bytes = stToIO (newCharArray (0,sz_in_bytes))
#endif

foreign import ccall "libHS_cbits.so" "createDirectory"  	primCreateDirectory     :: CString -> IO Int
foreign import ccall "libHS_cbits.so" "removeDirectory"  	primRemoveDirectory     :: CString -> IO Int
foreign import ccall "libHS_cbits.so" "removeFile"       	primRemoveFile          :: CString -> IO Int
foreign import ccall "libHS_cbits.so" "renameDirectory"  	primRenameDirectory     :: CString -> CString -> IO Int
foreign import ccall "libHS_cbits.so" "renameFile"       	primRenameFile          :: CString -> CString -> IO Int
foreign import ccall "libHS_cbits.so" "openDir__"        	primOpenDir     	:: CString -> IO Addr
foreign import ccall "libHS_cbits.so" "readDir__"        	primReadDir     	:: Addr -> IO Addr
foreign import ccall "libHS_cbits.so" "get_dirent_d_name"   primGetDirentDName      :: Addr -> IO Addr
foreign import ccall "libHS_cbits.so" "setCurrentDirectory" primSetCurrentDirectory :: CString -> IO Int
foreign import ccall "libHS_cbits.so" "getCurrentDirectory" primGetCurrentDirectory :: IO Addr
foreign import ccall "libc.so.6"        "free"                primFree                :: Addr -> IO ()
foreign import ccall "libc.so.6"        "malloc"              primMalloc              :: Word -> IO Addr
foreign import ccall "libc.so.6"        "chmod"               primChmod               :: CString -> Word -> IO Int
\end{code}

