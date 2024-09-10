#' Get make a zip file path
#'
#' Take a file path, remove the extension, replace the extension
#' with .zip
#'
#' @param file_path character.
#'
#' @return character. String where extension is replaced by zip
#' @export
#'
#' @examples
#'
#' file_path <- "hello.csv"
#' make_zip_path(file_path)
#'
#' file_path_with_dir <- "foo/bar/hello.csv"
#' make_zip_path(file_path_with_dir)
#'
make_zip_path <- function(file_path){
  file_path_no_ext <- fs::path_ext_remove(file_path)
  zip_path <- sprintf("%s.zip",file_path_no_ext)
  return(zip_path)
}

