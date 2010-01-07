\name{mmap}
\alias{mmap}
\alias{munmap}
\alias{is.mmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Map And Unmap Pages of Memory
}
\description{
Wrapper to POSIX compliant \sQuote{mmap} system calls.
}
\usage{
mmap(file, mode = integer(), classFUN=NULL, prot, flags, ...)
munmap(x)

is.mmap(x)
}
\arguments{
  \item{file}{
name of file holding data to be mapped into memory
}
  \item{mode}{
mode of data as returned by \R \code{storage.mode}
}
  \item{classFUN}{
A function to convert the raw/integer/double values
returned by subsetting into a complex R class. If no
change is needed, set to NULL (default).
}
  \item{prot}{
access permission to data being mapped.
Set via bitwise OR with \code{mmapFlags} to one or more of
\sQuote{PROT_READ}: Data can be read,
\sQuote{PROT_WRITE}: Data can be written,
\sQuote{PROT_EXEC}: Data can be executed,
\sQuote{PROT_NONE}: Data cannot be accessed.
Not all will apply within the context of R objects.
}
  \item{flags}{
additional flags to \code{mmap}.
Set via bitwise OR with \code{mmapFlags} to one or more of
\sQuote{MAP_SHARED}: Changes are shared,
\sQuote{MAP_PRIVATE}: Changes are private,
\sQuote{MAP_FIXED}: Interpret \emph{addr} exactly (Not Applicable).
Not all will apply within the context of R objects.
}
  \item{\dots}{
unused
}
  \item{x}{
an object of class \sQuote{mmap}.
}
}
\details{
The general semantics of the R function map to
the underlying POSIX C function call. The notable
exception is the use of \code{file} in place of
\code{void *addr} and \code{int fildes}.  Additionally
\code{len} and \code{off} arguments are ommitted from
the interface at present, so the entire object is
currently mapped into memory.  This may change in
future releases.
}
\value{
An object of class \code{mmap} containing
the fields 
\sQuote{data} (pointer to the starting address),
\sQuote{bytes} (length of object in bytes),
\sQuote{filedesc} (integer of file descriptor),
\sQuote{storage.mode} (\R type of data),
\sQuote{pagesize} (OS pagesize),
\sQuote{classFUN} (conversion function for extraction),
}
\references{
mmap: \url{http://www.opengroup.org/onlinepubs/000095399/functions/mmap.html}
}
\author{
Jeffrey A. Ryan
}
\note{
This is currently only functional on POSIX compliant systems.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{

#create a binary file
tmp <- tempfile()
writeBin(1:100L, tmp)

# map into memory (default mode=integer())
ints <- mmap(tmp, mode=integer()) 
ints[1]
ints[]
munmap(ints)

# store Dates as 'int' on disk
DATE <- mmap(tmp,classFUN=function(x) structure(x,class="Date"))
DATE[]
munmap(DATE)

# store 2 decimal numeric as 'int' on disk
num <- mmap(tmp,classFUN=function(x) x/100)
num[]
munmap(num)

unlink(tmp)
}
}
\keyword{ utilities }
