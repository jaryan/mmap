\name{mprotect}
\alias{mprotect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Control Protection of Pages
}
\description{
Wrapper to \code{mprotect} system call.  Not all
implementations will guarantee protection.
}
\usage{
mprotect(x, i, prot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{mmap} object.
}
  \item{i}{
location and length of pages to protect.
}
  \item{prot}{
protection flag set by \code{mmapFlags}.
Must be one or more of:
\sQuote{PROT_NONE},
\sQuote{PROT_READ},
\sQuote{PROT_WRITE},
\sQuote{PROT_EXEC}.
}
}
\details{
This functionality is very experimental, and likely to
be of limited use with R, as the result of a page access
that is protected is a SIG that isn't likely to
be caught by R.  This may be of use for other programs
sharing resource with R.
}
\value{
0 upon success, otherwise -1.
}
\references{
\sQuote{mprotect} man page.
}
\author{
Jeffrey A. Ryan
}
\keyword{ utilities }
