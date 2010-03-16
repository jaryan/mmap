Ops.mmap <- function(e1,e2) {
  OPS <- switch(.Generic,"=="=1L,
                            "!="=2L,
                            ">="=3L,
                            "<="=4L,
                            ">"= 5L,
                            "<"= 6L)
  if(is.mmap(e1)) {
    .Call("mmap_compare", e2, OPS, e1) 
  } else if(is.mmap(e2)) {
    .Call("mmap_compare", e1, OPS, e2) 
  }
}
