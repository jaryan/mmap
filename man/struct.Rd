\name{struct}
\alias{struct}
\alias{as.list.Ctype}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct a Ctype \sQuote{struct}.
}
\description{
Construct arbitarily complex \sQuote{struct}ures
in R for use with on-disk C struct's.
}
\usage{
struct(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Field types contained in struct.
}
}
\details{
\code{struct} provides a high level \R
based description of a C based struct
data type on disk.

The types of data that can be contained within
a structure (byte array) on disk can be any
permutation of the following: int8, uint8, int16
uint16, int32, real32, and real64.  \sQuote{struct}s
are not recursive, that is all struct's contained
within a struct must be logically flattened (core
elements extracted).

All C types are converted to the appropriate \R
type internally.

It is best to consider a struct a simple byte array,
where at specified offsets, a valid C variable type
exists.  Describing the struct using the \R
function \code{struct} allows mmap extraction
to proceed as if the entire structure was one block,
(a single \sQuote{i} value), and each block
of bytes can thus be read into R with one
operation.
}
\value{
A list of values, one element for each
type of \R data.
}
\references{
K&R, Kochran
}
\author{
Jeffrey A. Ryan
}
\note{
\sQuote{struct}'s can be thought of as \sQuote{rows}
in a database.  If many different types need always
be returned together, it will be more efficient to
store them together in a struct on disk. This reduces
the number of page hits required to fetch all required
data.  Conversley, if individual columns are desired
it will likely make sense to simply store vectors
in seperate files on disk and read in with \code{mmap}
individually as needed.
}


\seealso{
\code{\link{types}}
}
\examples{
tmp <- tempfile()

f <- file(tmp, open="ab")
u_int_8 <- c(1L, 255L, 22L)  # 1 byte, valid range 0:255
int_8 <- c(1L, -127L, -22L)  # 1 byte, valid range -128:127
int_16 <- c(1L, 25000L, -1000L) # 2 byte, valid range -32k:32k
int_32 <- c(98743L, -9083299L, 0L) # 4 byte, standard R integer
float_32 <- c(9832.22, 3.14159, 0.00001)

for(i in 1:3) {
  writeBin(u_int_8[i], f, size=1L)
  writeBin(int_8[i],   f, size=1L)
  writeBin(int_16[i],  f, size=2L)
  writeBin(int_32[i],  f, size=4L)
  writeBin(float_32[i],f, size=4L)
  writeBin(float_32[i],f, size=8L)
}
close(f)

m <- mmap(tmp, struct(uint8(), int8(), int16(), int32(), real32(), real64()))
length(m) # only 3 'struct' elements

m[]

m[1:2]

extractFUN(m) <- function(x,i,...) {
                   x <- x[i]
                   data.frame(u_int_8=x[[1]],
                                int_8=x[[2]],
                               int_16=x[[3]],
                               int_32=x[[4]],
                               float_32=x[[5]],
                               real_64=x[[6]]
                             )
                 }
m[1:2]
munmap(m)
unlink(tmp)

}
\keyword{ programming }
\keyword{ IO }
\keyword{ iteration }
