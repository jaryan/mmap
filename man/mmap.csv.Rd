\name{mmap.csv}
\alias{mmap.csv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Memory Map Text File
}
\description{
Reads a file column by column and creates a memory mapped object.
}
\usage{
mmap.csv(file, 
         header = TRUE, 
         sep = ",", 
         quote = "\"", 
         dec = ".", 
         fill = TRUE, 
         comment.char = "", 
         row.names,
         ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
the name of the file containing the comma-separated values to be
mapped.
}
  \item{header}{
does the file contain a header line?
}
  \item{sep}{
field separator character
}
  \item{quote}{
the set of quoting characters
}
  \item{dec}{
the character used for decimal points in the file
}
  \item{fill}{
unimplemented
}
  \item{comment.char}{
unimplemented
}
  \item{row.names}{
what it says
}
  \item{\dots}{
additional arguments
}
}
\details{
\code{mmap.csv} is meant to be the analogue of read.csv in \R, with the
primary difference being that data is read, by column, into memory-mapped
structs on disk. The intention is to allow for comma-separated files to
be easily mapped into memory without having to load the entire object at once.
}
\value{
An \code{mmap} object containing the data from the file. All types
will be set to the equivelant type from mmap
as would be in \R from a call to \code{read.csv}.
}
\author{
Jeffrey A. Ryan
}
\note{
This is currently a very simple implementation to facilitate exploration
of the mmap package.  While the interface will remain consistent with read.csv
from \pkg{utils}, more additions to handle various out-of-core types available
in mmap as well as performance optimization will be added.
}
\section{Warning}{
At present the memory required to memory-map a csv file will be the
memory required to load a single column from the file into R using
the traditional read.table function.  This may not be adequately efficient
for extremely large data.
}

\seealso{
\code{\link{mmap}}, \code{\link{read.csv}}
}
\examples{
data(cars)
tmp <- tempfile()
write.csv(cars, file=tmp, row.names=FALSE)

m <- mmap.csv(tmp)

colnames(m) <- colnames(cars)

m[]

extractFUN(m) <- as.data.frame  # coerce list to data frame upon subset

m[1:3,]

munmap(m)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ data }
