if (!require (pacman)) install.packages('pacman')
# Permanently required packages
pacman::p_install_version('udpipe', '0.8.3')
pacman::p_load(
  data.table,
  udpipe,
  tm,
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
