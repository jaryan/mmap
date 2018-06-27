\name{mmap}
\alias{mmap}
\alias{munmap}
\alias{extractFUN}
\alias{extractFUN<-}
\alias{replaceFUN}
\alias{replaceFUN<-}
\alias{is.mmap}
\alias{as.mmap}
\alias{dim.mmap}
\alias{dim<-.mmap}
\alias{dimnames.mmap}
\alias{dimnames<-.mmap}
\alias{is.array.mmap}
\alias{tempmmap}
\alias{pagesize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Map And Unmap Pages of Memory
}
\description{
Wrapper to POSIX \sQuote{mmap} and Windows MapViewOfFile system calls.
}
\usage{
mmap(file, mode = int32(), 
     extractFUN=NULL, replaceFUN=NULL,
     prot=mmapFlags("PROT_READ","PROT_WRITE"),
     flags=mmapFlags("MAP_SHARED"),
     len, off=0L, endian=.Platform$endian,
     ...)

munmap(x)

as.mmap(x, mode, file, ...)
is.mmap(x)

extractFUN(x)
replaceFUN(x)

extractFUN(x) <- value
replaceFUN(x) <- value
}
\arguments{
  \item{file}{
name of file holding data to be mapped into memory
}
  \item{mode}{
mode of data on disk. Use one of
\sQuote{char()} (char <-> \R raw),
\sQuote{int8()} (char <-> \R integer),
\sQuote{uint8()} (unsigned char <-> \R integer),
\sQuote{int16()} (short <-> \R integer),
\sQuote{uint16()} (unsigned short <-> \R integer),
\sQuote{int24()} (3 byte integer <-> \R integer),
\sQuote{uint24()} (unsigned 3 byte integer <-> \R integer),
\sQuote{int32()} (\R integer),
\sQuote{real32()} (float <-> \R double),
\sQuote{real64()} (\R double),
\sQuote{cplx()} (\R complex),
\sQuote{struct()} (Collection of Ctypes as defined by mmap).
See the related functions for details.
}
  \item{extractFUN}{
A function to convert the raw/integer/double values
returned by subsetting into a complex R class. If no
change is needed, set to NULL (default).
}
  \item{replaceFUN}{
A function to convert the R classes to underlying
C types for storage.
}
  \item{prot}{
access permission to data being mapped.
Set via bitwise OR with \code{mmapFlags} to one or more of
\sQuote{PROT_READ}: Data can be read,
\sQuote{PROT_WRITE}: Data can be written,
\sQuote{PROT_EXEC}: Data can be executed,
\sQuote{PROT_NONE}: Data cannot be accessed.
Not all will apply within the context of R objects.
The default is PROT_READ | PROT_WRITE.
}
  \item{flags}{
additional flags to \code{mmap}.
Set via bitwise OR with \code{mmapFlags} to one or more of
\sQuote{MAP_SHARED}: Changes are shared (default),
\sQuote{MAP_PRIVATE}: Changes are private,
\sQuote{MAP_FIXED}: Interpret \emph{addr} exactly (Not Applicable).
Not all will apply within the context of R objects.
}
  \item{len}{
length in bytes of mapping from offset. (EXPERT USE ONLY)
}
  \item{off}{
offset in bytes to start mapping. This \emph{must be} a multiple
of the system pagesize.  No checking is currently done, nor is there
any mmap provision to find pagesize automatically. (EXPERT USE ONLY)
}
  \item{endian}{
endianess of data. At present this is \emph{only} applied to \code{int8},\code{int16},
\code{int32},\code{float},\code{real32},\code{double}, and \code{real64} types for both
atomic and \code{struct} types. It is applied universally, and not at struct member elements.
}
  \item{\dots}{
unused
}
  \item{x}{
an object of class \sQuote{mmap}
}
  \item{value}{
a function to apply upon extraction or replacement.
}
}
\details{
The general semantics of the R function map to
the underlying operating system C function call. 
On unix-alikes this is \sQuote{mmap}, on Windows
similar functionality is provided by the
system call \sQuote{MapViewOfFile}. The notable
exception is the use of the R argument \code{file} in place of
\code{void *addr} and \code{int fildes}.  Additionally
\code{len} and \code{off} arguments are
made available to the R level call, though require
special care based on the system's mmap
and are advised for expert use only.

\code{as.mmap} allows for in-memory objects to be
converted to mmapped version on-disk.  The files are
stored in the location specified by \code{file}.
Passing an object that has an appropriate
as.mmap method will allow \R objects to be automatically
created as memory-mapped object.  
This works for most atomic types in R, including
numeric, complex, and character vectors. A special note
on character vectors: as the implementation requires
a constant number of bytes per element, each element
must be of the same width. See \code{as.mmap.character}
for more details.

Complex data types, such as 2 dimesioned vectors (matrix)
and data.frames can be supported using appropriate
\code{extractFUN} and \code{replaceFUN} functions to convert
the raw data.  Basic object conversion is made available
in included \code{as.mmap} methods for boths types as of
version 0.6-3.

All mode types are defined for single-column atomic
data, with the exception of structs.  Multiple column
objects are supported by the use of setting \code{dim}.
All data is column major. Row major orientation, as well
as supporting multiple types in one object - imitating
a data.frame, is supported via the \code{struct} mode. 

Using \code{struct} as the mode will organize the
binary data on-disk (or more correctly read data organized on disk) in
a row-major orientation.  This is similar to how a row
database would be oriented, and will provide faster
access to data that is typically viewed by row.
See \code{help(struct)} for examples of semantics as well
as performance comparisons.
}
\value{
The mmap and as.mmap call
returns an object of class \code{mmap} containing
the fields: 
\describe{
 \item{data:}{
pointer to the \sQuote{mmap}ped file.
}
 \item{bytes:}{
size of file in bytes. This is not in resident memory.
}
 \item{filedesc:}{
A names integer file descriptor, where the name
is path to the file mapped.
}
 \item{storage.mode:}{
\R type of raw data on disk. See \code{types} for details.
}
 \item{pagesize:}{
operating system pagesize.
}
 \item{extractFUN:}{
conversion function on extraction (optional).
}
 \item{replaceFUN:}{
conversion function for replacement (optional).
}
}
}
\references{
mmap: \url{http://www.opengroup.org/onlinepubs/000095399/functions/mmap.html}
}
\author{
Jeffrey A. Ryan
}

\seealso{
See Also as \code{\link{mmapFlags}},
}
\examples{

# create a binary file and map into 'ints' object
# Note that we are creating a file of 1 byte integers,
# and that the conversion is handled transparently
tmp <- tempfile()
ints <- as.mmap(1:100L, mode=int8(), file=tmp)

ints[1]
ints[]
ints[22]
ints[21:23] <- c(0,0,0)
ints[]  # changes are now on disk

# add dimension
dim(ints) <- c(10,10)
ints[]
ints[6,2] # 6th row of 2nd column
ints[ ,2] # entire 2nd column
munmap(ints)

# store Dates as natural-size 'int' on disk
writeBin(as.integer(Sys.Date()+1:10), tmp)
DATE <- mmap(tmp,extractFUN=function(x) structure(x,class="Date"))
DATE[]
munmap(DATE)

# store 2 decimal numeric as 'int' on disk, and convert on extraction
num <- mmap(tmp,extractFUN=function(x) x/100)
num[]
munmap(num)

unlink(tmp)

# convert via as.mmap munmap
int <- as.mmap(1:10L)
num <- as.mmap(rnorm(10))
}
\keyword{ utilities }
