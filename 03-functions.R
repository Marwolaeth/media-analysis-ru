strip_html <- function(s) {
  require(rvest)
  html_text(read_html(paste0('<body>', s, '</body>')))
}
strip_html <- cmpfun(strip_html)

# russian stopwords
stopwords_ru <- readLines('Tools/stopwords_ru.txt', encoding = 'UTF-8')
# from different sources
ru_stopwords <- union(stopwords_ru, tm::stopwords('russian'))
rm(stopwords_ru)

skip_combn <- function (x, m, skip = NULL, ...) 
{
  stopifnot(length(m) == 1L, is.numeric(m))
  if (m < 0) 
    stop('m < 0', domain = NA)
  if (is.numeric(x) && length(x) == 1L && x > 0 && trunc(x) == 
      x) 
    x <- seq_len(x)
  n <- length(x)
  if (n < m) 
    stop('n < m', domain = NA)
  if(is.null(skip)) skip <- n
  x0 <- x
  m <- as.integer(m)
  e <- 0
  h <- 0
  a <- seq_len(m)
  len.r <- length(r <- x[a])
  count <- as.integer(
    if(skip < 2) {
      n * m^skip - m^(1 + skip) + 1
    } else {
      n * skip^m * 2
    }
  )
  out <- vector('list', count)
  out[[1L]] <- r
  if (m > 0) {
    i <- 2L
    e <- max(0, m - m * skip)
    h <- max(0, e - a[1])
    nmmp1 <- n - m + 1L
    while (a[1L] != nmmp1) {
      if ((e < n - h) & (e - a[1L] < max(1, m * (skip - 1)))) {
        h <- 1L
        e <- a[m]
        j <- 1L
      }
      else {
        e <- a[m - h]
        h <- h + 1L
        j <- 1L:h
      }
      a[m - h + j] <- e + j
      if(skip == 0) {
        e <- a[m]
        h <- e - a[1] 
      }
      r <- x[a]
      out[[i]] <- r
      i <- i + 1L
    }
  }
  out[map_int(out, length) > 0L]
}
skip_combn <- cmpfun(skip_combn)

weight_terms <- function(
  tpos,
  method = 'log_hyperbolic',
  numerator = 2,
  base = 10,
  correction = 2
) {
  stopifnot(method %in% c('log', 'hyperbolic', 'log_hyperbolic'))
  if (method == 'log') {
    return(log(rev(tpos) + correction, base = base))
  } else if (method == 'hyperbolic') {
    return(numerator / tpos)
  } else {
    return(numerator / (log(tpos, base = base) + correction))
  }
}
weight_terms <- cmpfun(weight_terms)

# ngram_terms <- function(x)
