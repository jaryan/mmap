# Basic S3 methods
str.mmap <- function(object, ...) { print(unclass(x)) }
summary.mmap <- function() {}
print.mmap <- function(x, ...) { print(unclass(x)) }
print.summary_mmap <- function() {}

close.mmap <- function(con, ...) {
  munmap(con)
}

mmapFlags <- function(...) {
  if(nargs()==1) {
    flags <- gsub(" ","",unlist(strsplit(as.character(match.call(call=sys.call())[-1]),"\\|")))
    flags <- gsub('\"',"",flags) # in case "" | ""
  } else {
    flags <- as.character(match.call(call=sys.call())[-1])
  }
  .Call("mmap_mkFlags", flags)
}

# S3 constructor
mmap <- function(file, mode=integer(), prot, flags, ...) {
    if(missing(file))
      stop("'file' must be specified")
    mmap_obj <- .Call("mmap_mmap", mode, file, 3L, 1L)
    names(mmap_obj) <- c("data","bytes","filedesc","storage.mode","pagesize")
    class(mmap_obj) <- "mmap"
    return(mmap_obj)
}

# S3 destructor
munmap <- function(x) {
  if(!is.mmap(x))
    stop("mmap object required to munmap")
  .Call("mmap_munmap", x)
}

msync <- function(x) {
  if(!is.mmap(x))
    stop("mmap object required to munmap")
  .Call("mmap_msync", x)
}

mprotect <- function(x, i, prot) {
  # i indicates the start and length of protection
  .Call("mmap_mprotect", x, i, prot)
}

is.mmap <- function(x) {
  inherits(x, "mmap")
}

`[.mmap` <- function(x, i, ...) {
  if(!x[[2]]) stop('no data to extract')
  if(missing(i))
    i <- 1:length(x)
  .Call("mmap_extract", i, x)
}

`[<-.mmap` <- function(x, i, ..., sync=TRUE, value) {
  if(!x[[2]]) stop('no data to extract')
  if(missing(i))
    i <- 1:length(x)
  if(i > length(x) || i < 0 || length(i) != length(value)) stop()
  .Call("mmap_replace", i, value, x) 
  if(sync)
    msync(x)
  x
}

length.mmap <- function(x) {
  size_in_bytes <- x[[2]]
  storage_mode <- storage.mode(x[[4]])
  size <- switch(storage_mode,
    "raw"=1L,"integer"=4L, "double"=8L)
  as.integer(size_in_bytes/size)
}
