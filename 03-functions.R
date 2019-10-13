strip_html <- function(s) {
  require(rvest)
  html_text(read_html(s))
}

# russian stopwords
stopwords_ru <- readLines('Tools/stopwords_ru.txt', encoding = 'UTF-8')
# from different sources
ru_stopwords <- union(stopwords_ru, tm::stopwords('russian'))
rm(stopwords_ru)
