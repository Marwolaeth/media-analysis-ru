news <- fread('Data/news_8_noise3.csv', encoding = 'UTF-8')
# str(news)   # news is of class "data.table"
names(news)
news <- set_names(
  news,
  c(
    'date',
    'doc_id',
    'header',
    'text',
    'source',
    'url',
    'source.type',
    'language',
    'tag'
  )
)
news <- news[, c('date', 'doc_id', 'header', 'text', 'source', 'tag')]
news[, -c('text')] # truncated preview
# OlsonNames()[str_detect(OlsonNames(), 'Moscow')] # 'Europe/Moscow'
news[,
     c('date', 'text') := 
       list(
         dmy_hm(date, tz = 'Europe/Moscow'),
         strip_html(paste(header, text))
        )
    ]
str(news)
setkey(news, doc_id)

tic()
df <- udpipe(
  news[, .(doc_id, text)],
  syntagrus,
  parallel.cores = 2L
) %>%
  as.data.table()
toc()
names(df)
# df
# df[paragraph_id >= 2][1:40]
# all(is.na(df$deps))
df <- df[
  ,
  .(doc_id,
    sentence_id,
    term_id,
    token_id,
    token,
    lemma,
    upos,
    head_token_id,
    dep_rel
  )
]
setkey(df, doc_id, term_id)

# x <- 1L:444L
# (y <- 2 / (log(x, 10) + 2))
# plot(x, y, type = 'l')
# hist(y, breaks = 30)
# 
# summary(df[, .(n = max(term_id)), by = doc_id])
# summary(df[, .N, by = .(doc_id, sentence_id)])

# df[, term_weight := 2 / (log(term_id, 10) + 2), by = doc_id]
# 
# df_doc <- copy(df) %>% setkey(doc_id)
# df_term <- copy(df) %>% setkey(doc_id, term_id)
# 
# microbenchmark::microbenchmark(
#   nokey = df[, term_weight := 2 / (log(term_id, 10) + 2), by = doc_id],
#   doc   = df_doc[,term_weight := 2 / (log(term_id, 10) + 2), by = doc_id],
#   term  = df_term[,term_weight := 2 / (log(term_id, 10) + 2), by = doc_id],
#   fun_nokey = df[, term_weight := weight_terms(term_id), by = doc_id],
#   fun_doc   = df_doc[, term_weight := weight_terms(term_id), by = doc_id],
#   fun_term  = df_term[, term_weight := weight_terms(term_id), by = doc_id]
# )

df[, term_weight := weight_terms(term_id), by = doc_id]

# summary(df)
# hist(df$term_weight, breaks = 30)

# (x <- df[doc_id == unique(doc_id)[111] & sentence_id == 4 & upos != 'PUNCT',
#         token])
# n <- 3L
# k <- 1L
# map(0:k, ~ seq(1, 22, by = .+1)) %>%
#   map(~ cbind(., lead(., 1)))
# map2_int(1:n, 0:k, ~ seq(1, 22, by = k+1))
# 
# skip_combn(1:13, 2, skip = 2)

# l <- expand.grid(m = 1:3, k = 0:2, KEEP.OUT.ATTRS = FALSE)
# l$length <- map2(l[[1]], l[[2]], ~ skip_combn(1:10, m = .x, skip = .y)) %>%
#   map_int(length)
# l
# 
# m <- 1L
# k <- 1L
# sum(10 - ((0:k)^m * m - 0))

(x <- df[doc_id == unique(doc_id)[111] & sentence_id == 4 & upos != 'PUNCT',
        token])

(xw <- df[doc_id == unique(doc_id)[111] & sentence_id == 4 & upos != 'PUNCT',
         term_weight])
skip_combn(x, m = 3, skip = 1) %>% map(~ paste(., collapse = ' ')) %>% reduce(c)

m <- 3L
k <- 2L
l <- expand.grid(m = 1:m, k = 0:k, KEEP.OUT.ATTRS = FALSE) %>%
  filter(m > 1 | k == 0)
t <- map2(
  l[[1]],
  l[[2]],
  ~ skip_combn(
    x,
    m = .x,
    skip = .y
  ) %>%
    map(~ paste(., collapse = ' '))
) %>%
  unlist()

w <- map2(
  l[[1]],
  l[[2]],
  ~ skip_combn(
    xw,
    m = .x,
    skip = .y
  ) %>%
    map(function(w) sum(w) / max(1, .x - 0))
) %>%
  unlist()
data.frame(t, w, stringsAsFactors = F) %>% distinct()

# ngrams(x = x, 2)

ngrams <- df[, ]