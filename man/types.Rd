\name{C_types}
\alias{types}
\alias{sizeofCtypes}
\alias{as.Ctype}
\alias{is.Ctype}
\alias{bits}
\alias{logi8}
\alias{logi32}
\alias{cstring}
\alias{cstring.MaxWidth}
\alias{is.cstring}
\alias{char}
\alias{uchar}
\alias{cplx}
\alias{int8}
\alias{uint8}
\alias{int16}
\alias{uint16}
\alias{int24}
\alias{uint24}
\alias{int32}
\alias{int64}
\alias{real32}
\alias{real64}
\alias{as.char}
\alias{as.uchar}
\alias{as.cplx}
\alias{as.int8}
\alias{as.uint8}
\alias{as.int16}
\alias{as.uint16}
\alias{as.int24}
\alias{as.uint24}
\alias{as.int32}
\alias{as.real32}
\alias{as.real64}
\alias{as.cstring}
\alias{nbytes}
\title{
Virtual R Types On Disk.
}
\description{
These functions describe the types of raw binary
data stored on disk. 
}
\usage{
%bits(length = 0)
char(length = 0, nul = TRUE)
uchar(length = 0)
logi8(length = 0)
logi32(length = 0)
int8(length = 0)
uint8(length = 0)
int16(length = 0)
uint16(length = 0)
int24(length = 0)
uint24(length = 0)
int32(length = 0)
int64(length = 0)
real32(length = 0)
real64(length = 0)
cplx(length = 0)
cstring(length = 0, na.strings = "NA")

as.Ctype(x)
is.Ctype(x)

cstring.MaxWidth()
sizeofCtypes()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{length}{
desired length. Not used when passed to mode= in mmap call.
}
  \item{x}{
R object to coerce or test
}
  \item{nul}{
are characters delimited by a nul byte?
}
  \item{na.strings}{
string to convert to \R's NA. See Details for current implementation.
}
}
\details{
R has very limited storage types. There is one type
of integer and one type of float (double).  Storage
to disk often can be made more efficient by reducing
the precision of the data. These functions provide
for a sort of virtual mapping from disk to native
R type, for use with mmap-ed files.

When a memory mapping is created, a conversion method
if declared for both extracting values from disk, as well
as replacing elements on disk.  The preceeding
functions are used in the internal compiled code to
handle the conversion.

It is the user's responsibility to ensure that data
fits within the prescribed types. All fixed-width types support
extraction, replacement, and boolean Ops (e.g. \code{==}). See below
for note on \code{cstring} layout.

\code{cstring} reads nul-terminated strings from binary
C-style arrays. To minimize memory allocation, two additional
steps are carried out. First, when a memory map
is initiated, the length (N) of the character array is
calculated. The calculation of word offsets to facilitate access
are deferred until the first request \code{[} or a \code{Ops}
request. This offset calculation requires the creation of
an internal index made up of short integers, representing
the length of each character element. On most platforms, this
is at least 65534 (sizeof(short) - 1 for nul byte), 
but can be found via \code{cstring.MaxWidth}. 
This index will consume sizeof(short) * N memory, allocated
outside of R.

At present \code{na.strings="NA"} is ignored and all occurances
of the (binary) string \sQuote{NA} are converted to \code{NA_character_} types
in R. This is also used by the \pkg{mmap} \code{is.na} function.
}
\value{
An R typed vector of length \sQuote{length} with a
virtual type and class \sQuote{Ctype}. Additional
information related to number of bytes and whether
the vitrual type is signed is also contained.
}
\references{
\url{https://en.wikipedia.org/wiki/C_variable_types_and_declarations}
\url{https://cran.r-project.org/doc/manuals/R-exts.html}
}
\author{
Jeffrey A. Ryan
}
\note{
R vectors may be used to create files on disk
matching the specified type using the functions
\code{writeBin} with the appropriate \code{size}
argument. See also.
}
\section{Warning }{
The is no attempt to store or read metadata with
respect to the extracted or replaced data. This
is simply a low level interface to facilitate
data reading and writing. 
} 
\seealso{
\code{\link{writeBin}}
}
\examples{
tmp <- tempfile()

# write a 1 byte signed integer -128:127
writeBin(-127:127L, tmp, size=1L)
file.info(tmp)$size
one_byte <- mmap(tmp, int8())
one_byte[]
munmap(one_byte)

# write a 1 byte unsigned integer 0:255
writeBin(0:255L, tmp, size=1L)
file.info(tmp)$size
one_byte <- mmap(tmp, uint8())
one_byte[]
munmap(one_byte)

# write a 2 byte integer -32768:32767
writeBin(c(-32768L,32767L), tmp, size=2L)
file.info(tmp)$size
two_byte <- mmap(tmp, int16())
two_byte[]
munmap(two_byte)

# write a 2 byte unsigned integer 0:65535
writeBin(c(0L,65535L), tmp, size=2L)
two_byte <- mmap(tmp, uint16())
two_byte[]

# replacement methods automatically (watch precision!!)
two_byte[1] <- 50000
two_byte[]

# values outside of range (above 65535 for uint16 will be wrong)
two_byte[1] <- 65535 + 1
two_byte[]

munmap(two_byte)

# write a 4 byte integer standard R type
writeBin(1:10L, tmp, size=4L)
four_byte <- mmap(tmp, int32())
four_byte[]
munmap(four_byte)

# write 32 bit integers as 64 bit longs (where supported)
int64()  # note it is a double in R, but described as int64
writeBin(1:10L, tmp, size=8L)
eight_byte <- mmap(tmp, int64())
storage.mode(eight_byte[])  # using R doubles to preserve most long values
eight_byte[5] <- 2^40  # write as a long, a value in R that is double ~2^53 is representable
eight_byte[5]
munmap(eight_byte)

cstring()
cstring.MaxWidth()
writeBin(c("this","is","a","sentence"), tmp)
strings <- mmap(tmp, cstring())
strings[1:2]
strings[]
munmap(strings)

unlink(tmp)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO  }
