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
A comma seperated list of zero or more
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
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jeffrey A. Ryan
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should bVe DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (...) 
{
    flags <- as.character(match.call(call = sys.call())[-1])
    .Call("mmap_mkFlags", flags)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
