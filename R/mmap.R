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
  cat("mmap object of length",length(object),":\n")
  print(head(object))
  str(unclass(object)) 
}
summary.mmap <- function(object) { str(unclass(object)) }
print.mmap <- function(x, ...) {
  cat(paste("<mmap object ",names(x$filedesc),">\n",sep="")) 
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
                 flags=mmapFlags("MAP_SHARED"),
                 ...) {
    if(missing(file))
      stop("'file' must be specified")
    mmap_obj <- .Call("mmap_mmap", 
                      mode,
                      file,
                      as.integer(prot), 
                      as.integer(flags), PKG="mmap")
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
  inherits(x, "mmap")
}

`[.mmap` <- function(x, i, ...) {
  if(!x[[2]]) stop('no data to extract')
  if(missing(i))
    i <- 1:length(x)
  if(is.null(extractFUN(x))) {
    .Call("mmap_extract", i, x, PKG="mmap")
  } else as.function(extractFUN(x))(.Call("mmap_extract", i, x, PKG="mmap"))
}

`[<-.mmap` <- function(x, i, ..., sync=TRUE, value) {
  # add type checking/coercing at the C-level
  if(!x[[2]]) stop('no data to extract')
  if(missing(i))
    i <- 1:length(x)
  if(i > length(x) || i < 0 || length(i) != length(value))
    stop("improper 'i' range")
  .Call("mmap_replace", i, value, x, PKG="mmap") 
  if(sync)
    msync(x)
  x
}

length.mmap <- function(x) {
  size_in_bytes <- x[[2]]
  size <- attr(x$storage.mode,"bytes")
  as.integer(size_in_bytes/size)
}
