library(mmap)
tmp <- tempfile()

##### int8() #####
# write binary from min to max signed 2^8
writeBin(-128:127L, tmp, size=1)

m <- mmap(tmp, int8())  # signed 1 byte integers
if( !all(m[] == (-128:127L)) )
  stop("m[] == (-128:127L)")

# test replacement
m[] <- -128L
if( !all(m[] == -128))
  stop("m[] == -128")
munmap(m)




#### uint8() ####
writeBin(0:255L, tmp, size=1)
m <- mmap(tmp, uint8())  # unsigned 1 byte integers
if( !all(m[] == 0:255L) )
  stop("m[] == 0:255L")

# test replacement
m[] <- 255L;
if( !all(m[] == 255L))
  stop("m[] == 255L")
munmap(m)




#### int16() ####
writeBin(-32768:32767L, tmp, size=2)
m <- mmap(tmp, int16())  # signed 2 byte integers
if( !all(m[] == -32768:32767L) )
  stop("m[] == -32768:32767L")

# test replacement
m[] <- -32768L
if( !all(m[] == -32768L))
  stop("m[] == -32768L")
munmap(m)




#### uint16() ####
writeBin(0:65535L, tmp, size=2)
m <- mmap(tmp, uint16())  # unsigned 2 byte integers
if( !all(m[] == 0:65535L) )
  stop("m[] == 0:65535L")

# test replacement
m[] <- 65535L
if( !all(m[] == 65535L))
  stop("m[] == 65535L")
munmap(m)




#### int32() ####
writeBin(-1e6:1e6L, tmp, size=4)
m <- mmap(tmp, int32())  # unsigned 2 byte integers
if( !all(m[] == -1e6:1e6L) )
  stop("m[] == -1e6:1e6L")

# test replacement
m[] <- .Machine$integer.max
if( !all(m[] == .Machine$integer.max))
  stop("m[] == .Machine$integer.max")
munmap(m)




#### int64() ####
writeBin(0.0, tmp)
m <- mmap(tmp, int64())  # signed 8 byte integers as doubles
m[] <- 2^40
if( !all(m[] == 2^40) )
  stop("m[] == 2^40")
munmap(m)

