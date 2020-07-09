# library(utils)
# library(httr)
# library(glue::glue)

log_msg <- function(msg, verbose = TRUE) {
  if (verbose) {
    print(msg)
  }
}

get_resource <- function(resource_type, resource_id,
                         access_token = NULL, laxy_api_url = "https://api.laxy.io") {
  url <- glue::glue("{laxy_api_url}/api/v1/{resource_type}/{resource_id}/")
  if (!is.null(access_token)) {
    url <- glue::glue("{url}?access_token={access_token}")
  }
  r <- httr::GET(url)
  #if (httr::status_code(r) != 200) {
  #}
  resource_json <- httr::content(r, "parsed")
  return(resource_json)
}

# Get the job details, grab the input and output fileset ids
get_job <- function(job_id, access_token = NULL, laxy_api_url = "https://api.laxy.io") {
  return(get_resource('job', job_id, access_token = access_token, laxy_api_url = laxy_api_url))
}

get_fileset <- function(fileset_id, access_token = NULL, laxy_api_url = "https://api.laxy.io") {
  return(get_resource('fileset', fileset_id, access_token = access_token, laxy_api_url = laxy_api_url))
}

get_laxy_file <- function(job_id, filepath, access_token = NULL, laxy_api_url = "https://api.laxy.io") {
  url <- glue::glue("{laxy_api_url}/api/v1/job/{job_id}/files/{filepath}")
  if (!is.null(access_token)) {
    url <- glue::glue("{url}?access_token={access_token}")
  }
  r <- httr::GET(url, httr::write_disk(glue::glue("{job_id}/{filepath}"), overwrite = TRUE))
  return(r)
}

#' Download files from a Laxy job
#'
#' @param job_id the job ID
#' @param access_token the secret access token
#' @param glob_filter a wildcard glob to select a subset of files, eg \code{"output/*/*.txt"}
#' @param clobber_truncated overwrite any existing file that appears smaller than the remote copy
#' @param skip_existing skip any files that already exist locally, don't overwrite
#' @param max_size if set, don't download files larger than this (in bytes)
#' @param verbose if \code{TRUE}, output verbose logging
#' @param laxy_api_url set the Laxy API endpoint, defaults to https://api.laxy.io
#'
#' @return Returns a vector of file paths that were downloaded
#' @export
#'
#' @examples
#'
#' downloaded_files <-
#'   get_job_files("4KIORsrZLl8NLktlmhloor",
#'                  access_token = "51d97057-8b48-4457-a40d-72c6641fa539")
#'
#' downloaded_files <-
#'   get_job_files("4KIORsrZLl8NLktlmhloor",
#'                  glob_filter = "*/sikRun/countFiles/*.txt",
#'                  access_token = "51d97057-8b48-4457-a40d-72c6641fa539",
#'                  laxy_api_url = "https://dev-api.laxy.io:8001")
#'
get_job_files <- function(job_id, access_token = NULL, glob_filter = "*",
                          clobber_truncated = TRUE, skip_existing = TRUE,
                          max_size = NULL,
                          verbose = FALSE,
                          laxy_api_url = "https://api.laxy.io") {

  job <- get_job(job_id, access_token = access_token, laxy_api_url = laxy_api_url)
  input_fileset <- get_fileset(job$input_fileset_id, access_token = access_token, laxy_api_url = laxy_api_url)
  output_fileset <- get_fileset(job$output_fileset_id, access_token = access_token, laxy_api_url = laxy_api_url)

  downloaded <- c()
  for (f in append(input_fileset$files, output_fileset$files)) {
    FNAME <- f$name
    FPATH <- f$path
    OUTPATH <- glue::glue("{job_id}/{FPATH}/{FNAME}")
    file_size <- f$metadata$size

    # Skip files that don't match the glob pattern
    if (!any(grepl(utils::glob2rx(glob_filter), c(glue::glue("{FPATH}/{FNAME}"))))) {
      next
    }

    # Skip files larger than max_size
    if (!is.null(max_size) & !is.null(file_size)) {
      if (file_size > max_size) {
        next
      }
    }

    if (!dir.exists(glue::glue("{job_id}/{FPATH}"))) {
      dir.create(glue::glue("{job_id}/{FPATH}"), recursive = TRUE)
    }

    if (skip_existing & file.exists(OUTPATH)) {
      if (!is.null(file_size) & clobber_truncated & file.size(OUTPATH) < file_size) {
        log_msg(glue::glue("Removing are redownloading {FPATH}/{FNAME}, appears truncated."), verbose)
        file.remove(OUTPATH)
      } else {
        log_msg(glue::glue("Skipping {FPATH}/{FNAME}, exists."), verbose)
      }
    } else {

      # URL <- glue::glue("{BASE_URL}/api/v1/job/{job_id}/files/{FPATH}/{FNAME}?access_token={ACCESS_TOKEN}")
      # r <- httr::GET(URL, write_disk(glue::glue("{job_id}/{FPATH}/{FNAME}"), overwrite = TRUE))
      r <- get_laxy_file(job_id, glue::glue("{FPATH}/{FNAME}"), access_token = access_token)

      if (httr::status_code(r) != 200) {
        log_msg(glue::glue("Download of {FPATH}/{FNAME} failed."), verbose)
      }
      downloaded <- append(downloaded, glue::glue("{FPATH}/{FNAME}"))
    }
  }

  return(downloaded)
}
