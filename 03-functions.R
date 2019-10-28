strip_html <- function(s) {
  require(rvest)
  html_text(read_html(paste0('<body>', s, '</body>')))
}
strip_html <- cmpfun(strip_html)

# russian stopwords
stopwords_ru <- readLines('Tools/stopwords_ru.txt', encoding = 'UTF-8')
# from different sources
ru_stopwords <- Reduce(
  base::union,
  list(
    stopwords_ru,
    tm::stopwords('russian'),
    # stopwords::data_stopwords_snowball$ru is equal to tm::stopwords('ru')
    stopwords::data_stopwords_stopwordsiso$ru
  )
)
rm(stopwords_ru)

water_terms <- c(
  'INTJ',  # междометие
  'PART',  # частица
  'DET',   # определитель
  'ADP',   # предлог
  'PRON',  # местоимение
  'CCONJ', # соединительный союз
  'SCONJ'  # подчинительный союз
)

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
# skip_combn <- cmpfun(skip_combn)

weight_terms <- function(
  term.rank,
  method = 'log_hyperbolic',
  numerator = 2,
  base = 10,
  correction = 2,
  pos.weights = list(
    PROPN = 1.5,
    NOUN  = 1.2
  ),
  term.pos = NULL
) {
  stopifnot(method %in% c('log', 'hyperbolic', 'log_hyperbolic'))
  if (length(pos.weights)) {
    if (is.null(term.pos) | is.na(term.pos)) {
      weight <- 1
      warning(
        'Parts-of-speech are not specified: using default value',
        .call = FALSE
      )
    }
    weight_df <- data.frame(
      upos   = names(pos.weights),
      weight = pos.weights,
      stringsAsFactors = FALSE
    )
    weight_df <- base::merge(
      weight_df,
      data.frame(upos = term.pos, stringsAsFactors = FALSE),
      all.y = TRUE,
      sort = FALSE
    )
    weight <- weight_df[['weight']]
    rm(weight_df)
  }
  if (method == 'log') {
    return(weight * log(rev(term.rank) + correction, base = base))
  } else if (method == 'hyperbolic') {
    return(weight * (numerator / term.rank))
  } else {
    return(weight * (numerator / (log(term.rank, base = base) + correction)))
  }
}
# weight_terms <- cmpfun(weight_terms)

chop_ngrams <- function(
  x,
  ngram = 2L,
  skip  = 0L,
  FUN,
  ...
) {
  sapply(
    skip_combn(
      x, m = ngram, skip = skip
    ),
    FUN,
    ...
  )
}

ngram_terms <- function(
  df,
  n = 1L:2L,
  k = 0L,
  token.var  = 'lemma',
  pos.var    = 'upos',
  use.weight = TRUE,
  weight.var = if (use.weight) {
    grep('weight', names(df), value = TRUE)[1]
  } else {
    NULL
  },
  correction   = 0,
  remove.stopw = FALSE,
  remove.water = FALSE,
  stopw    = NULL
) {
  require(data.table)
  require(purrr)
  if (is.data.table(df)) {
    d <- copy(df)
  } else {
    d <- as.data.table(df)
    rm(df)
  }
  # && to ensure that the result of comparison is of length one
  if (use.weight && (is.na(weight.var) | is.null(weight.var))) {
    stop('Token weight variable is not specified and cannot be estimated')
  }
  if (remove.stopw) {
    if (is.null(stopw) || is.na(stopw)) {
      stopw <- tryCatch(
       get(grep('stopword', ls(envir = .GlobalEnv), value = TRUE)),
        error = function(e) {
          if (require(stopwords)) {
            stopwords::data_stopwords_stopwordsiso$ru
          } else if (require(tm)) {
            tm::stopwords(kind = 'ru')
          } else {
            stop('Stopword list is not specified and cannot be estimated')
          }
        }
      )
    }
    setkeyv(d, token.var)
    d <- d[!(d[[token.var]] %chin% stopw)]
    setkey(d, NULL)
  }
  if (remove.water) {
    d <- tryCatch(
      d[!(d[[pos.var]] %chin% water_terms)],
      error = function(e) d
    )
  }
  l <- as.data.table(
    expand.grid(n = n, k = k, KEEP.OUT.ATTRS = FALSE)
  )[n > 1 | k == 0]
  res <- l[
    , .(
      ngram = chop_ngrams(
        d[[token.var]],
        ngram = n,
        skip = k,
        FUN = paste,
        collapse = ' '
      )
    ),
    keyby = .(n, k)
  ]
  if (use.weight) {
    match_weights <- function(w, corr = 0, kw = 0L) {
      sum(w) / max(1, w - corr)
    }
    resw <- l[, .(weight = chop_ngrams(
      d[[weight.var]],
      ngram = n,
      skip = k,
      FUN = match_weights,
      corr = correction,
      kw = skip
    )),
    keyby = .(n, k)
    ]
    res <- res[resw, nomatch = 0]
  }
}
