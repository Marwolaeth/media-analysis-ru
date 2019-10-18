news <- fread('Data/news_8_noise3.csv', encoding = 'UTF-8')
# str(news)   # news is of class "data.table"
names(news)
news <- set_names(
  news,
  c(
    'date',
    'id',
    'header',
    'text',
    'source',
    'url',
    'source.type',
    'language',
    'tag'
  )
)
news <- news[, c('date', 'id', 'header', 'text', 'source', 'tag')]
news[, -c('text')] # truncated preview
# OlsonNames()[str_detect(OlsonNames(), 'Moscow')] # 'Europe/Moscow'
news[,
     c('date', 'text') := 
       list(
         dmy_hm(date, tz = 'Europe/Moscow'),
         map_chr(paste(header, text), strip_html)
        )
    ]
str(news)

tic()
df <- udpipe(
  news[, .(doc_id = id, text)],
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

df[, term_weight := 2 / (log(term_id, 10) + 2), by = doc_id]
# summary(df)
# hist(df$term_weight, breaks = 30)

(x <- df[doc_id == unique(doc_id)[111] & sentence_id == 4 & upos != 'PUNCT',
        token])
n <- 2L
k <- 1L
map(0:k, ~ seq(1, 22, by = .+1)) %>%
  map()
map2_int(1:n, 0:k, ~ seq(1, 22, by = k+1))
