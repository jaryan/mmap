\name{mmapFlags}
\alias{mmapFlags}
\title{
Create Bitwise Flags for mmap.
}
\description{
Allows for unquoted C constant names to
be bitwise OR'd together for passing
to \code{mmap} related calls.
}
\usage{
mmapFlags(...)
}
\arguments{
  \item{\dots}{
A comma or vertical bar \sQuote{|} seperated list of zero or more
valid \code{mmap} constants. May be quoted or unquoted
from the following: PROT_READ, PROT_WRITE, PROT_EXEC,
PROT_NONE, MAP_SHARED, MAP_PRIVATE, MAP_FIXED, MS_ASYNC, MS_SYNC,
MS_INVALIDATE. See details for more information.
}
}
\details{
Argument list may contain quoted or unquoted constants
as defined in \code{<sys/mman.h>}.  See invididual
functions for details on valid flags.

Multiple values passed in will be bitwise OR'd
together at the C level, allowing for
semantics close to that of native C calls.
}
\value{
An integer vector of length 1.
}
\author{
Jeffrey A. Ryan
}
\note{
Read your system's \sQuote{mmap} man pages for use details.
}
\seealso{
See Also as \code{\link{mmap}}, ~~~
See Also as \code{\link{mprotect}}, ~~~
}
\examples{
mmapFlags(PROT_READ)
mmapFlags(PROT_READ | PROT_WRITE)
mmapFlags("PROT_READ" | "PROT_WRITE")
mmapFlags(PROT_READ , PROT_WRITE)
mmapFlags("PROT_READ" , "PROT_WRITE")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
