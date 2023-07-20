# Uses secrets stored in the local credential store with the keyring package
if(0){
  # prompts you to enter the secret
  keyring::key_set("Azure_SASURL")
  keyring::key_set("Azure_subnetId")
  # retrieves the secret
  keyring::key_get("Azure_SASURL")
}



# write a version of run_caribou.sh and task json for each batch

make_files <- function(batch){
  batch <- as.character(batch)
  # need this to make it have unix line endings
  outfile <-  file(paste0("cloud/run_caribou", batch, ".sh"), "wb")
  readLines("cloud/run_caribou.sh") |> stringr::str_replace_all("<batch>", batch)|>
    stringr::str_replace_all("<PAT>", gh::gh_token()) |>
    writeLines(con = outfile)
  close(outfile)
}

purrr::walk(1:24, make_files)


make_task <- function(batch){
  batch <- as.character(batch)
  # need this to make it have unix line endings
  outfile <-  file(paste0("cloud/caribouDemo", batch, ".json"), "wb")
  readLines("cloud/caribouDemo.json") |> stringr::str_replace_all("<batch>", batch)|>
    stringr::str_replace_all("<SASURL>", keyring::key_get("Azure_SASURL"))|>
    writeLines(con = outfile)
  close(outfile)
}
purrr::walk(1:24, make_task)

# Add secrets to pool
# need this to make it have unix line endings
outfile <-  file(paste0("cloud/caribou_add_pool1.json"), "wb")
readLines("cloud/caribou_add_pool.json") |>
  stringr::str_replace_all("<subnetId>", keyring::key_get("Azure_subnetId"))|>
  writeLines(con = outfile)
close(outfile)
