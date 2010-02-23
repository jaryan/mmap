Ops.mmap <- function(e1,e2) {
  OPS <- switch(.Generic,"=="=1L,
                            "!="=2L,
                            ">="=3L,
                            "<="=4L,
                            ">"= 5L,
                            "<"= 6L)
  if(is.mmap(e1) && is.integer(e1$storage.mode)) {
    .Call("mmap_compare", as.integer(e2), OPS, e1) 
  } else if(is.mmap(e2) && is.integer(e2$storage.mode)) {
    .Call("mmap_compare", as.integer(e1), OPS, e2) 
  }
}
