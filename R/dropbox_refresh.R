refresh_db_token<-function(path = "auth/db_token.rds"){ 
  rdrop2::drop_auth(rdstoken = path, cache = FALSE)
  # download a small file to get the token to refresh properly. Allows upload functions to work. 
  rdrop2::drop_download("/dtra_rvf2/data/rvf2_github_data/tinyfile.txt",local_path = here::here("auth"), overwrite = TRUE)
  file.remove(here::here("auth/tinyfile.txt"))
}
