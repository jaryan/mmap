\name{make.fixedwidth}
\alias{make.fixedwidth}
\title{
Convert Character Vectors From Variable To Constant Width
}
\description{
Utility function to convert a vector of character strings
to one where each element has exactly \sQuote{width}-bytes.
}
\usage{
make.fixedwidth(x, width = NA, justify = c("left", "right"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A character vector.
}
  \item{width}{
Maximum width of each element. width=NA (default)
will expand each element to the width required
to contain the largest element of x without loss
of information.
}
  \item{justify}{
How should the results be padded? \sQuote{left} will
add spacing to the right of shorter elements in the vector
(left-justified), \sQuote{right} will do the opposite.
}
}
\details{
The current implementation of mmap only handles fixed-width
strings (nul-terminated). To simplify conversion of (potentially)
variable-width strings in a character vector, all
elements will be padded to the length of the longest string
in the vector or set to length \code{width} if specified.

All new elements will be left or right justified based on the
\code{justify} argument.
}
\value{
A character vector where each element is of fixed-width.
}
\author{
Jeffrey A. Ryan
}
\note{
Future implementions will possibly support variable-width character
vectors.
}

\examples{
month.name
make.fixedwidth(month.name)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
