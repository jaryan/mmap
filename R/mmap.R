# S3 accessor methods
extractFUN <- function(x) {
  UseMethod("extractFUN")
}
`extractFUN<-` <- function(x, value) {
  UseMethod("extractFUN<-")
}
extractFUN.mmap <- function(x) {
  x$extractFUN
}
`extractFUN<-.mmap` <- function(x, value) {
  x$extractFUN <- value
  x
}
replaceFUN <- function(x) {
  UseMethod("replaceFUN")
}
`replaceFUN<-` <- function(x, value) {
  UseMethod("replaceFUN<-")
}
replaceFUN.mmap <- function(x) {
  x$replaceFUN
}
`replaceFUN<-.mmap` <- function(x, value) {
  x$replaceFUN <- value
  x
}

# Basic S3 methods
head.mmap <- function(x, n=6L, ...) {
  x[1:(min(length(x),n))]
}
tail.mmap <- function(x, n=6L, ...) {
  x[(length(x)-n):length(x)]
}
str.mmap <- function(object, ...) { 
  cat("  ",print(object))
  str(unclass(object)) 
}
summary.mmap <- function(object) { str(unclass(object)) }

print.mmap <- function(x, ...) {
  stopifnot(is.mmap(x))
  file_name <- names(x$filedesc)
  if(nchar(file_name) > 10)
    file_name <- paste(substring(file_name,0,10),"...",sep="")
  type_name <- switch(typeof(x$storage.mode),
                      "list"="struct",
                      "integer"="int",
                      "double"="num",
                      "complex"="cplx",
                      "character"="chr",
                      "raw"="raw")
  if(type_name == "struct") {
    firstN <- x[1][[1]]
  } else {
  firstN <- x[1:min(6,length(x))]
  firstN <- if(cumsum(nchar(firstN))[length(firstN)] > 20) {
                firstN[1:min(3,length(x))]
              } else {
                firstN
              }
  }
  cat(paste("<mmap:",file_name,">  (",class(x$storage.mode)[2],") ",
            type_name," [1:", length(x),"]",sep=""),firstN,"...\n")
}
print.summary_mmap <- function() {}

close.mmap <- function(con, ...) {
  munmap(con)
}

# creat flags using upper case symbols/strings
# mmapFlags(PROT_READ,PROT_WRITE) OR mmapFlags(PROT_READ | PROT_WRITE)
mmapFlags <- function(...) {
  flags <- as.character(match.call(call=sys.call())[-1])
  if(nargs()==1) {
    flags <- gsub(" ","",unlist(strsplit(flags,"\\|")))
    flags <- gsub('\"',"",flags) # in case "" | ""
  }
  .Call("mmap_mkFlags", flags, PKG="mmap")
}

# S3 constructor
mmap <- function(file, mode=int32(), 
                 extractFUN=NULL, replaceFUN=NULL,
                 prot=mmapFlags("PROT_READ","PROT_WRITE"),
                 flags=mmapFlags("MAP_SHARED"),len,off=0L,
                 ...) {
    if(missing(file))
      stop("'file' must be specified")
    if(missing(len))
      len <- file.info(file)$size
    if(off %% pagesize() != 0L)
      stop(paste("'off' must be a multiple of",pagesize(),"(pagesize)"))
    
    mmap_obj <- .Call("mmap_mmap", 
                      as.Ctype(mode),
                      file,
                      as.integer(prot), 
                      as.integer(flags), 
                      as.integer(len),
                      as.integer(off),
                      PKG="mmap")
    names(mmap_obj) <- c("data",
                         "bytes",
                         "filedesc",
                         "storage.mode",
                         "pagesize")
    mmap_obj$filedesc <- structure(mmap_obj$filedesc, .Names=file)
    mmap_obj$extractFUN <- extractFUN
    mmap_obj$replaceFUN <- replaceFUN
    class(mmap_obj) <- "mmap"
    return(mmap_obj)
}

# S3 destructor
munmap <- function(x) {
  if(!is.mmap(x))
    stop("mmap object required to munmap")
  invisible(.Call("mmap_munmap", x, PKG="mmap"))
}

msync <- function(x, flags=mmapFlags("MS_ASYNC")) {
  if(!is.mmap(x))
    stop("mmap object required to munmap")
  .Call("mmap_msync", x, as.integer(flags), PKG="mmap")
}

mprotect <- function(x, i, prot) {
  # i indicates the start and length of protection

  # TODO: add ability to protect multiple pages in a
  # range
  .Call("mmap_mprotect", x, i, prot, PKG="mmap")
}

is.mmap <- function(x) {
  inherits(x, "mmap") && .Call("mmap_is_mmapped",x,PKG="mmap")
}

`[.mmap` <- function(x, i, j, ...) {
  if(!x[[2]]) stop('no data to extract')
  if(missing(i))
    i <- 1:length(x)
  if(missing(j))
    j <- 1:length(x$storage.mode)
  if(is.character(j))
    j <- match(j, names(x$storage.mode))
  j <- j[j>0] # only positive values
  xx <- .Call("mmap_extract", i, as.integer(j), x, PKG="mmap")
  names(xx) <- names(x$storage.mode)[j]
  if(is.null(extractFUN(x))) {
    xx
  } else as.function(extractFUN(x))(xx)
#  if(is.null(extractFUN(x))) {
#    .Call("mmap_extract", i, as.integer(j), x, PKG="mmap")
#  } else as.function(extractFUN(x))(.Call("mmap_extract", i, as.integer(j), x, PKG="mmap"))
}

`[<-.mmap` <- function(x, i, j, ..., sync=TRUE, value) {
  # add type checking/coercing at the C-level
  if(!x[[2]]) stop('no data to extract')
  if(missing(i))
    i <- 1:length(x)
  if(missing(j))
    j <- 1:length(x$storage.mode)
  if(is.character(j))
    j <- match(j, names(x$storage.mode))
  if(length(i) != length(value))
    if(is.list(value))
      value <- lapply(value, rep, length.out=length(i))
    else value <- rep(value, length.out=length(i))
# likely we need to check for list()/struct to correctly handle in C
  if(max(i) > length(x) || min(i) < 0)
    stop("improper 'i' range")
  .Call("mmap_replace", i, j, value, x, PKG="mmap") 
  if(sync)
    msync(x)
  x
}

length.mmap <- function(x) {
  size_in_bytes <- x[[2]]
  size <- attr(x$storage.mode,"bytes")
  as.integer(size_in_bytes/size)
}


# coerce to disk object and mmap back in.  Need
# to register a proper finalizer in the C code, else
# we are likely to end up with memory leaks.  For now
# this is not too probable, and not too dangerous.
# Famous last words ...

as.mmap <- function(x, mode, file,...) {
  UseMethod("as.mmap")
}

#as.mmap.data.frame <- function(x, mode=as.struct(x), file, ...) 

as.mmap.integer <- function(x,
                            mode=integer(),
                            file=tempmmap(),
                            ...) {
  nbytes <- attr(as.Ctype(mode),"bytes")
  if(nbytes == 3) {
    writeBin(writeBin(x,raw())[1:(length(x)*4) %% 4 != 0], file)
  } else writeBin(x, file, size=nbytes)
  mmap(file, as.Ctype(mode))
}
as.mmap.double <- function(x,
                            mode=double(),
                            file=tempmmap(),
                            ...) {
  nbytes <- attr(as.Ctype(mode),"bytes")
  writeBin(x, file, size=nbytes)
  mmap(file, as.Ctype(mode))
}
as.mmap.complex <- function(x,
                            mode=complex(),
                            file=tempmmap(),
                            ...) {
  nbytes <- attr(as.Ctype(mode),"bytes")
  writeBin(x, file, size=nbytes)
  mmap(file, as.Ctype(mode))
}

tempmmap <- function(tmpdir=tempdir()) {
  tempfile("mmap",tmpdir)
}

pagesize <- function() {
  .Call("mmap_pagesize")
}
