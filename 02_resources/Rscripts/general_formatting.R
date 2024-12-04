nifu_global_general_formatting <-
  fs::path(Sys.getenv("USERPROFILE"), "NIFU", "Metode - General", "SAROS-core", "shared resources", "nifu_global_general_formatting.R")
if(!file.exists(nifu_global_general_formatting)) {
  cli::cli_abort("{.file {nifu_global_general_formatting}} er utilgjengelig. Har du synkronisert Metode - General-teamet til din maskin?")
} else source(nifu_global_general_formatting, echo = FALSE)
