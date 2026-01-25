# storage.R - S3 and local file operations
# Handles reading/writing data to local storage and AWS S3

library(aws.s3)

#' Get AWS S3 bucket name from environment
#'
#' @return S3 bucket name
get_s3_bucket <- function() {
  bucket <- Sys.getenv("AWS_S3_BUCKET", unset = "wsmfp-setlists")
  return(bucket)
}

#' Upload a file to S3
#'
#' @param local_path Local file path
#' @param s3_key S3 object key (path in bucket)
#' @param bucket S3 bucket name (defaults to env variable)
#' @return TRUE if successful, FALSE otherwise
upload_to_s3 <- function(local_path, s3_key, bucket = get_s3_bucket()) {
  tryCatch({
    aws.s3::put_object(
      file = local_path,
      object = s3_key,
      bucket = bucket
    )
    print(paste0("✓ Uploaded to s3://", bucket, "/", s3_key))
    return(TRUE)
  }, error = function(e) {
    warning(paste0("✗ Failed to upload ", local_path, " to S3: ", e$message))
    return(FALSE)
  })
}

#' Download a file from S3
#'
#' @param s3_key S3 object key
#' @param local_path Local destination path
#' @param bucket S3 bucket name
#' @return TRUE if successful, FALSE otherwise
download_from_s3 <- function(s3_key, local_path, bucket = get_s3_bucket()) {
  tryCatch({
    aws.s3::save_object(
      object = s3_key,
      bucket = bucket,
      file = local_path
    )
    print(paste0("✓ Downloaded s3://", bucket, "/", s3_key, " to ", local_path))
    return(TRUE)
  }, error = function(e) {
    warning(paste0("✗ Failed to download ", s3_key, " from S3: ", e$message))
    return(FALSE)
  })
}

#' Save RDS file locally and to S3
#'
#' @param data Data object to save
#' @param filename Base filename (e.g., "WSP_Song_Fact_Table_1986_to_2025.rds")
#' @param local_dir Local directory (default: "./Data/")
#' @param s3_dir S3 directory prefix (default: "")
#' @return TRUE if both saves succeeded, FALSE otherwise
save_rds_with_s3 <- function(data, filename, local_dir = "./Data/", s3_dir = "") {
  # Ensure local directory exists
  if (!dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE)
  }
  
  local_path <- file.path(local_dir, filename)
  s3_key <- if (s3_dir == "") filename else file.path(s3_dir, filename)
  
  # Save locally
  tryCatch({
    saveRDS(data, file = local_path)
    print(paste0("✓ Saved locally: ", local_path))
  }, error = function(e) {
    warning(paste0("✗ Failed to save locally: ", e$message))
    return(FALSE)
  })
  
  # Upload to S3
  s3_success <- upload_to_s3(local_path, s3_key)
  
  return(s3_success)
}

#' Load RDS file from local or S3 (with fallback)
#'
#' @param filename Filename
#' @param local_dir Local directory
#' @param s3_dir S3 directory prefix
#' @return Data object or NULL if not found
load_rds_with_s3 <- function(filename, local_dir = "./Data/", s3_dir = "") {
  local_path <- file.path(local_dir, filename)
  s3_key <- if (s3_dir == "") filename else file.path(s3_dir, filename)
  
  # Try local first
  if (file.exists(local_path)) {
    print(paste0("✓ Loading from local: ", local_path))
    return(readRDS(local_path))
  }
  
  # Fallback to S3
  print(paste0("Local file not found. Attempting S3 download: ", s3_key))
  
  if (download_from_s3(s3_key, local_path)) {
    return(readRDS(local_path))
  }
  
  warning(paste0("✗ Could not load ", filename, " from local or S3"))
  return(NULL)
}

#' Save CSV file locally and to S3
#'
#' @param data Data frame to save
#' @param filename Base filename
#' @param local_dir Local directory
#' @param s3_dir S3 directory prefix
#' @return TRUE if both saves succeeded
save_csv_with_s3 <- function(data, filename, local_dir = "./Data/", s3_dir = "") {
  if (!dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE)
  }
  
  local_path <- file.path(local_dir, filename)
  s3_key <- if (s3_dir == "") filename else file.path(s3_dir, filename)
  
  # Save locally
  tryCatch({
    write.csv(data, local_path, row.names = FALSE)
    print(paste0("✓ Saved locally: ", local_path))
  }, error = function(e) {
    warning(paste0("✗ Failed to save CSV locally: ", e$message))
    return(FALSE)
  })
  
  # Upload to S3
  s3_success <- upload_to_s3(local_path, s3_key)
  
  return(s3_success)
}
