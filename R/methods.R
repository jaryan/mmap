Ops.mmap <- function(e1,e2) {
  OPS <- switch(.Generic,"=="=1L,
                         "!="=2L,
                         ">="=3L,
                         "<="=4L,
                         ">"= 5L,
                         "<"= 6L)
  if(is.mmap(e1)) {
    if(storage.mode(e1$storage.mode) == "character")
      e2 <- charToRaw(e2)
    .Call("mmap_compare", e2, OPS, e1) 
  } else if(is.mmap(e2)) {
    if(storage.mode(e2$storage.mode) == "character")
      e1 <- charToRaw(e1)
    .Call("mmap_compare", e1, OPS, e2) 
  }
}

dim.mmap <- function(x) {
  if(is.struct(x$storage.mode))
    return( c(length(x), length(x$storage.mode)) )
  NULL
}
