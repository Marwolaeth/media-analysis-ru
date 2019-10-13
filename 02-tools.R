# syntagrus <- udpipe_load_model(
#   'Tools/udpipe/Russian/russian-syntagrus-ud-2.0-17080.udpipe'
# )
# Currently trying larger cross-language model
udmod <- udpipe_load_model(
  'Tools/udpipe/Cross/deep-ud-2.4-data.tgz'
) %>%
  '['('file_model')
