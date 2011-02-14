\name{sizeof}
\alias{sizeof}
\title{
Calculate the Size of Datatypes
}
\description{
Calculate the number of bytes in an R data type used by \pkg{mmap}.
}
\usage{
sizeof(type)
}
\arguments{
  \item{type}{
A type constructor (function), \R atomic, or \pkg{mmap} \code{Ctype}.
}
}
\details{
A constructor for the purposes of \code{sizeof} is a function object
used to create an atomic type for \R or mmap.  These include the
base atomic type functions such as \code{integer}, \code{character},
\code{double}, \code{numeric}, \code{single}, \code{complex} and
similar.  In addition, the Ctype constructors in \pkg{mmap} such
as \code{int8}, \code{uint8}, \code{real32}, etc may be passed in.

More typically a representative object of the above types can
be passed in to determine the appropriate data size.

The purpose of this function is for use to help construct a proper
\code{offset} argument value for mmap and \code{mprotect}, though
neither use is common or encouraged since alignment to pagesize is
required from the system call.
}
\value{
Numeric bytes used.
}
\author{
Jeffrey A. Ryan
}
\seealso{
\code{\link{pagesize}}
\code{\link{as.Ctype}}
}
\examples{
# all are equal

sizeof(int32)
sizeof(int32())
sizeof(integer)
sizeof(integer())
sizeof(1L)
}
\keyword{ utilities }
