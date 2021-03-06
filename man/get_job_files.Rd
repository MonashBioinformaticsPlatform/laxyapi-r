% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/laxy-api-client.R
\name{get_job_files}
\alias{get_job_files}
\title{Download files from a Laxy job}
\usage{
get_job_files(
  job_id,
  access_token = NULL,
  glob_filter = "*",
  clobber_truncated = TRUE,
  skip_existing = TRUE,
  max_size = NULL,
  verbose = FALSE,
  laxy_api_url = "https://api.laxy.io"
)
}
\arguments{
\item{job_id}{the job ID}

\item{access_token}{the secret access token}

\item{glob_filter}{a wildcard glob to select a subset of files, eg \code{"output/*/*.txt"}}

\item{clobber_truncated}{overwrite any existing file that appears smaller than the remote copy}

\item{skip_existing}{skip any files that already exist locally, don't overwrite}

\item{max_size}{if set, don't download files larger than this (in bytes)}

\item{verbose}{if \code{TRUE}, output verbose logging}

\item{laxy_api_url}{set the Laxy API endpoint, defaults to https://api.laxy.io}
}
\value{
Returns a vector of file paths that were downloaded
}
\description{
Download files from a Laxy job
}
\examples{

downloaded_files <-
  get_job_files("4KIORsrZLl8NLktlmhloor",
                 access_token = "51d97057-8b48-4457-a40d-72c6641fa539")

downloaded_files <-
  get_job_files("4KIORsrZLl8NLktlmhloor",
                 glob_filter = "*/sikRun/countFiles/*.txt",
                 access_token = "51d97057-8b48-4457-a40d-72c6641fa539",
                 laxy_api_url = "https://dev-api.laxy.io:8001")

}
