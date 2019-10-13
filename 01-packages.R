if (!require (pacman)) install.packages('pacman')
# Permanently required packages
pacman::p_load(
  data.table,
  tm,
  udpipe,
  purrr,
  stringr,
  lubridate,
  igraph,
  RNewsflow,
  compiler,
  tictoc,
  fst,
  update = TRUE
)
# Occasionally required packages
pacman::p_install(c('rvest', 'microbenchmark', 'stringi', 'ggplot2'))
