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
         paste(header, text)
        )
    ]
str(news)

df <- udpipe(news[, text], syntagrus, trace = 10, parallel.cores = 8L) %>%
  as.data.frame(detailed = TRUE) %>%
  as.data.table()
