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
  if(length(x) == 1 && x==0)
    int32()
  else int32(length(x))
}
as.Ctype.double <- function(x) {
  if(length(x) == 1 && x==0)
    real64()
  else real64(length(x))
}
as.Ctype.raw <- function(x) {
  if(length(x) == 1 && x==0)
    uchar()
  else uchar(length(x))
}
as.Ctype.character <- function(x) {
  if(length(x) == 1 && x==0)
    char()
  else char(length(x))
}
as.Ctype.complex <- function(x) {
  if(length(x) == 1 && x==0)
    cplx()
  else cplx(length(x))
}

char <- C_char <- function(length=0) {
  if(length==0) { # a char byte
    structure(raw(length), bytes=1L, signed=1L, class=c("Ctype","char"))
  } else {
    structure(character(length+1), bytes=as.integer(length+1), signed=0L,
              class=c("Ctype","char"))
  }
}

as.char <- function(x, ...) UseMethod("as.char")
as.char.mmap <- function(x, length, ...) {
  x$storage.mode <- char(length)
  x 
}

uchar <- C_uchar <- function(length=0) {
  # unsigned 1 byte char
  structure(raw(length), bytes=1L, signed=0L, class=c("Ctype","uchar"))
}

as.uchar <- function(x, ...) UseMethod("as.uchar")
as.uchar.mmap <- function(x, length, ...) {
  x$storage.mode <- uchar(length)
  x 
}

int8 <- function(length=0) {
  # signed 1 byte int
  structure(integer(length), bytes=1L, signed=1L, class=c("Ctype","char"))
}
as.int8 <- function(x, length, ...) UseMethod("as.int8")
as.int8.mmap <- function(x, length=0, ...) {
  x$storage.mode <- int8(length)
  x
}

uint8 <- function(length=0) {
  # unsigned 1 byte int
  structure(integer(length), bytes=1L, signed=0L, class=c("Ctype","uchar"))
}
as.uint8 <- function(x, length, ...) UseMethod("as.uint8")
as.uint8.mmap <- function(x, length=0, ...) {
  x$storage.mode <- uint8(length)
  x
}

int16 <- C_short <- function(length=0) {
  structure(integer(length), bytes=2L, signed=1L, class=c("Ctype","short"))
}
as.int16 <- function(x, length, ...) UseMethod("as.int16")
as.int16.mmap <- function(x, length=0, ...) {
  x$storage.mode <- int16(length)
  x
}

uint16 <- C_ushort <- function(length=0) {
  structure(integer(length), bytes=2L, signed=0L, class=c("Ctype","ushort"))
}
as.uint16 <- function(x, length, ...) UseMethod("as.uint16")
as.uint16.mmap <- function(x, length=0, ...) {
  x$storage.mode <- uint16(length)
  x
}

int24 <- C_int24 <- function(length=0) {
  structure(integer(length), bytes=3L, signed=1L, class=c("Ctype","int24"))
}
as.int24 <- function(x, length, ...) UseMethod("as.int24")
as.int24.mmap <- function(x, length=0, ...) {
  x$storage.mode <- int24(length)
  x
}

uint24 <- C_uint24 <- function(length=0) {
  structure(integer(length), bytes=3L, signed=0L, class=c("Ctype","uint24"))
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
    .class <- class(x)[2]
    attributes(x) <- NULL
    if(length(x)==0) {
      cat(paste(typeof(x),"(0)\n",sep=""))
    } else
    if(.class=="char") {
      cat(paste("character(",length(x),")\n",sep=""))
    } else
    cat(x,"\n")
  }
}

is.struct <- function(x) {
  inherits(x, "struct")
}

as.struct <- function(x, ...) {
  UseMethod("as.struct")
}

as.struct.default <- function(x, ...) {
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

nbytes <- function(x) UseMethod("nbytes")
nbytes.Ctype <- function(x) attr(x, "bytes")
nbytes.mmap <- function(x) x$bytes
