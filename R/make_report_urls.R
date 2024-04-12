#' Make the urls for the reports
#'
#
#' @param aws_deploy_target List. Output from aws_s3_upload
#'
#' @param pattern String. Regex pattern for matching file paths
#'
#' @return
#' @author Collin Schwantes
#' @export
make_report_urls <- function(aws_deploy_target, pattern = "") {
  
  keys <- purrr::map_chr(aws_deploy_target,
                         ~.x$key)
  
  pattern_check <- grepl(pattern, keys)
  
  urls <- containerTemplateUtils::make_eha_secure_url(
    remote_path = keys[pattern_check])
  
  return(urls)
}