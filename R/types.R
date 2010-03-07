# C_types
sizeofCtypes <- function() {
  structure(.Call("sizeof_Ctypes"), 
            .Names=c("char","short","int","long","float","double"))
}

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
as.Ctype.complex <- function(x) {
  cplx(length(x))
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
  dots <- lapply(list(...),as.Ctype)
  bytes <- sapply(dots, attr, which="bytes")
  structure(dots, bytes=sum(bytes), offset=cumsum(bytes)-bytes,
            signed=NA, class=c("Ctype","struct"))
}

`[[<-.struct` <- function(x,i,value) {
  x <- unclass(x)
  x[[i]] <- as.Ctype(value)
  do.call(struct,x)
}

print.Ctype <- function(x, ...) {
  if(class(x)[2] == "struct") {
    cat("struct:\n")
    for(i in 1:length(x)) {
    cat(paste("  (",class(x[[i]])[2],") ",sep=""))
    attributes(x[[i]]) <- NULL
    if(length(x[[i]])==0)
      cat(paste(typeof(x[[i]]),"(0)\n",sep=""))
    else
    cat(x[[i]],"\n")
    }
  } else {
    cat(paste("(",class(x)[2],") ",sep=""))
    attributes(x) <- NULL
    if(length(x)==0)
      cat(paste(typeof(x),"(0)\n",sep=""))
    else
    cat(x,"\n")
  }
}

as.struct <- function(x, ...) {
  UseMethod("as.struct")
}

as.struct.default <- function(x) {
  if(inherits(x,"struct"))
    return(x)
  x <- as.list(x)
  types <- lapply(lapply(x,class), 
             function(CLASS) switch(CLASS,
                             "raw"=char(),
                             "integer"=int32(),
                             "numeric"=,
                             "double"=real64(),
                             "complex"=cplx())
            )
  do.call(struct, types)
}

nbytes <- function(x) attr(x, "bytes")
