Ops.mmap <- function(e1,e2) {
  OPS <- switch(.Generic,"=="=1L,
                         "!="=2L,
                         ">="=3L,
                         "<="=4L,
                         ">"= 5L,
                         "<"= 6L,
                             -1L)
  if(OPS == -1L)
    stop(paste(.Generic,"unsupported for 'mmap' objects"))
  if(is.mmap(e1)) {
    if(storage.mode(e1$storage.mode) == "character")
      e2 <- charToRaw(e2)
    .Call("mmap_compare", e2, OPS, e1, PACKAGE="mmap") 
  } else if(is.mmap(e2)) {
    if(storage.mode(e2$storage.mode) == "character")
      e1 <- charToRaw(e1)
    .Call("mmap_compare", e1, OPS, e2, PACKAGE="mmap") 
  }
}

dim.mmap <- function(x) {
  if( is.struct(x$storage.mode))
    return( c(length(x), length(x$storage.mode)) )
  if(is.null(x$dim))
    c(length(x),1)
  else
    x$dim
}

`dim<-.mmap` <- function(x, value) {
  if( is.struct(x$storage.mode))
    stop("dimensions are fixed for struct objects")
  if( is.null(value)) {
    x$dim <- value
  } else
  if( length(value) != 2) {
    stop("only dimension of length two supported")
  } else x$dim <- as.integer(value)
  x
}

dimnames.mmap <- function(x) {
  if( is.struct(x$storage.mode))
    list(NULL, names(x$storage.mode))
  else x$dimnames
}

`dimnames<-.mmap` <- function(x, value) {
  if( is.null(dim(x)))
    stop("'dimnames' applied to non-array")
  if( is.struct(x$storage.mode)) {
    names(x$storage.mode) <- value[[2]]
    x$dimnames <- value
  } else x$dimnames <- value
  x
}

is.array.mmap <- function(x) TRUE  # used for NROW/NCOL

is.na.mmap <- function(x) {
  if(is.cstring(x$storage.mode)) {
    .Call("mmap_cstring_isna",x,FALSE)
  } else {
    stop("is.na only implemented for Ctype cstring")
  }
}
anyNA.mmap <- function(x, recursive=FALSE) {
  if(is.cstring(x$storage.mode)) {
    .Call("mmap_cstring_isna",x,TRUE)
  } else {
    stop("anyNA only implemented for Ctype cstring")
  }
}
