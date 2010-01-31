\name{msync}
\alias{msync}
\title{
Synchronize Memory With Physical Storage
}
\description{
\code{msync} calls the underlying system call of
the same name.  This writes modified whole pages
back to the filesystem and updates the file modification
time.
}
\usage{
msync(x, flags=mmapFlags("MS_ASYNC"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An \code{mmap} object.
}
  \item{flags}{
One of the following flags:
\sQuote{MS_ASYNC}: return immediately (default).
\sQuote{MS_SYNC}: perform synchronous writes.
\sQuote{MS_INVALIDATE}: invalidate all cached data.

Per the man page, \sQuote{MS_ASYNC} is not
permitted to be combined with the other flags.
}
}
\details{
See the appropriate OS man page.
}
\value{
0 on success, otherwise -1.
}
\author{
Jeffrey A. Ryan
}
\keyword{ ~kwd1 }
