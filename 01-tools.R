if (!require (pacman)) install.packages('pacman')
pacman::p_load(
  data.table,
  tm,
  udpipe,
  purrr,
  stringr,
  stringi,
  igraph,
  RNewsflow,
  compiler,
  microbenchmark,
  tictoc,
  update = TRUE
)
