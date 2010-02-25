# C_types
as.Ctype <- function(x) {
  UseMethod("as.Ctype")
}

as.Ctype.Ctype <- function(x) return(x)

as.Ctype.integer <- function(x) {
  int32(length(x))
}
as.Ctype.double <- function(x) {
  real64(length(x))
}
as.Ctype.raw <- function(x) {
  char(length(x))
}
as.Ctype.character <- function(x) {
  uint8(length(x))
}

char <- C_raw <- C_char <- function(length=0) {
  structure(raw(length), bytes=1L, signed=1L, class=c("Ctype","char"))
}

uchar <- C_uchar <- function(length=0) {
  # unsigned 1 byte char
  structure(raw(length), bytes=1L, signed=0L, class=c("Ctype","uchar"))
}

int8 <- function(length=0) {
  # signed 1 byte int
  structure(integer(length), bytes=1L, signed=1L, class=c("Ctype","char"))
}

uint8 <- function(length=0) {
  # unsigned 1 byte int
  structure(integer(length), bytes=1L, signed=0L, class=c("Ctype","uchar"))
}

int16 <- C_short <- function(length=0) {
  structure(integer(length), bytes=2L, signed=1L, class=c("Ctype","short"))
}

uint16 <- C_ushort <- function(length=0) {
  structure(integer(length), bytes=2L, signed=0L, class=c("Ctype","ushort"))
}

int32 <- C_int <- function(length=0) {
  structure(integer(length), bytes=4L, signed=1L, class=c("Ctype","int"))
}

uint32 <- C_uint <- function(length=0) {
  structure(integer(length), bytes=4L, signed=0L, class=c("Ctype","uint"))
}

real32 <- C_float <- function(length=0) { 
  structure(double(length),  bytes=4L, signed=1L, class=c("Ctype","float"))
}

real64 <- C_double <- function(length=0) { 
  structure(double(length),  bytes=8L, signed=1L, class=c("Ctype","double"))
}

cplx <- C_complex <- function(length=0) {
  structure(complex(length),  bytes=16L, signed=1L, class=c("Ctype","complex"))
}

struct <- as.list.Ctype <- function(...) {
  dots <- list(...)
  bytes <- sapply(dots, attr, which="bytes")
  structure(dots, bytes=sum(bytes), offset=cumsum(bytes)-bytes,
            signed=NA, class=c("Ctype","struct"))
}

