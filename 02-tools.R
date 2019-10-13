syntagrus <- udpipe_load_model(
  'Tools/udpipe/Russian/russian-syntagrus-ud-2.0-170801.udpipe'
)
# Currently trying larger cross-language model
udmod <- udpipe_load_model(
  'Tools/udpipe/Cross/deep-ud-2.4-data.tgz'
)

####################
# Train udpipe model
####################

news <- fread('Data/news_train.csv', encoding = 'UTF-8')
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
         paste(header, text)
       )
     ]
str(news)

df <- udpipe(
  object = syntagrus$file,
  x = news[, .(doc_id = id, text)],
  trace = 10,
  parallel.cores = 3L
) %>%
  as.data.frame(detailed = TRUE) %>%
  as.data.table()
write_fst(df, path = 'Data/Intermediate/news_train_raw.fst', compress = 20)
