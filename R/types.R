char <- C_raw <- function(n=0) {
  structure(integer(n), bytes=1L, class=c("Ctype","char"))
}

int8 <- C_char <- function(n=0) {
  structure(integer(n), bytes=1L, class=c("Ctype","char"))
}

int16 <- C_short <- function(n=0) {
  structure(integer(n), bytes=2L, class=c("Ctype","short"))
}

int32 <- C_int <- function(n=0) {
  structure(integer(n), bytes=4L, class=c("Ctype","int"))
}

real32 <- C_float <- function(n=0) { 
  structure(double(n),  bytes=4L, class=c("Ctype","float"))
}

real64 <- C_double <- function(n=0) { 
  structure(double(n),  bytes=8L, class=c("Ctype","double"))
}
