# write a version of run_caribou.sh for each batch

make_files <- function(batch){
  batch <- as.character(batch)
  # need this to make it have unix line endings
  outfile <-  file(paste0("cloud/run_caribou", batch, ".sh"), "wb")
  readLines("cloud/run_caribou.sh") |> stringr::str_replace_all("<batch>", batch)|>
    writeLines(con = outfile)
  close(outfile)
}

purrr::walk(1:24, make_files)
