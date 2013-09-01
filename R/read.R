readBenc <- function(what) {
  if (is.character(what)) {
    fn <- path.expand(what)
    if (length(fn) > 1L)
      return(lapply(fn, readBenc))
    s <- file.info(fn)$size
    if (is.null(s)) stop("Cannot open `",fn,"'")
    f <- file(fn, "rb")
    on.exit(close(f))
    return(readBenc(readBin(f, raw(), s)))
  }
  if (inherits(what, "connection")) {
    c <- 1024L*1024L
    r <- readBin(what, raw(), c)
    if (length(r) == c) while (TRUE) {
      n <- readBin(what, raw(), c)
      r <- c(r, n)
      if (length(n) < c) break
    }
    return(readBenc(r))
  }
  .Call(benc_decode, what)
}
