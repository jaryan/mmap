.onLoad <- function(lib, pkg) {
  .Call("make_bitmask", PACKAGE="mmap")
}
