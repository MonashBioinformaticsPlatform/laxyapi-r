# laxyapi

Interact with the Laxy API via R. Download job files.

```R
install.packages("remotes")
remotes::install_github('MonashBioinformaticsPlatform/laxyapi-r')

JOB_ID <- "bl4F00longIDfr0MLAXY"
ACCESS_TOKEN <- "51d97057-8b48-4457-a40d-72c6641fa539"

# Get all the files from the job
all_job_files <- laxyapi::get_job_files(
  JOB_ID,
  access_token = ACCESS_TOKEN)

# Download just the files in */sikRun/countFiles/*.txt
# These would be just the count files from an RNAsik run
#
downloaded_count_files <- laxyapi::get_job_files(
  JOB_ID,
  glob_filter = "*/sikRun/countFiles/*.txt",
  access_token = ACCESS_TOKEN)
```
