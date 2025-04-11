##FROM excessILI package, Marcus Russi
now <- lubridate::now()

retrievePath <- function(fname, basepath='.', goalDate=lubridate::now()) {
  
  # Construct the path to the folder where all copies of 'fname' should be 
  # stored
  fullpath <- file.path(basepath, fname)
  
  # Make sure that this path is a directory I.e., archive/fname.txt needs to 
  # be a folder, not regular file.
  if (!dir.exists(fullpath))
    stop(sprintf("Path %s doesn't exist, or isn't a directory", fullpath))
  
  # Be sure that 'goalDate' is a Date object
  assertthat::assert_that(any("POSIXct" %in% class(goalDate)))
  
  # Get all the files in this directory
  dirListing <- list.files(fullpath)
  
  # If there were no files in the directory, we can't retrieve them!
  if (length(dirListing) == 0)
    stop(sprintf("No files were found in dirctory %s", fullpath))
  
  # Get a list of modification times for each file in the directory
  fullPaths <- file.path(fullpath, dirListing)
  mtimes <- purrr::lift_dl(file.mtime)(fullPaths)
  
  # The interval elapsed between the modified time of each file, and the 
  # goalDate. Our goal is to get the file with the smallest delta.
  absDeltas <-
    purrr::map_dbl(mtimes, ~lubridate::int_length(. %--% goalDate)) %>%
    abs
  
  # Find the index, and return it
  idx <- which(absDeltas == min(absDeltas))[1]
  
  fullPaths[idx]
}

#' Retrieve an parquet modified nearest to a specific date
#' 
#' Retrieves an parquet file stored in \code{basepath/fname} that has
#' the closest modification date to \code{goalDate}. If no such file exists, an
#' error will be thrown.
#'
#' @inheritParams retrievePath
#'
#' @examples
#' storeRDS(mtcars, 'mtcars')
#' identical(mtcars, retrieveRDS('mtcars'))
#'
#' storeRDS(mtcars, 'mtcars', basepath='.')
#' mtcars2 <- mtcars
#' mtcars2[,'cyl'] <- 1
#' storeRDS(mtcars2, 'mtcars')
#' mtcars_retrieved <- retrieveRDS('mtcars', goalDate = lubridate::now())
#' identical(mtcars, mtcars_retrieved)
#'
#' @export
retrievePQT <- function(fname, basepath='.', goalDate=Sys.time())
  retrievePath(fname, basepath, goalDate) %>% read_parquet()

#' Store an R object into the file cache
#' 
#' Given an R object, attempts to store it in the directory
#' \code{basepath/fname}. The name given to the file will be of the form
#' \code{DATE.PQT}, where \code{DATE} is of the format
#' \code{\%Y_\%m_\%d_\%H_\%M}.  An error will be thrown if \code{basepath} does
#' not exist. However, if \code{basepath/fname} does not exist, an attempt will
#' be made to create it. The \code{DATE} is the current time. Intended to be
#' used with \code{\link{retrievePQT}}. See \code{\link{mostRecentTimestamp}}
#' for an usage example.
#' 
#' @param fname The name of the directory in \code{basepath} where various
#'   revisions of the file are stored. I.e., \code{file.txt} should be a
#'   directory, with revisions of the true \code{file.txt} stored inside of
#'   it.
#'
#' @param obj An R object
#'
#' @param basepath A string. The path which stores \code{fname}. Default '.'
#'
#' @return A message announcing the path the object has been written to
#' 
#' @examples
#' savePQT(mtcars, 'cars')
#' savePQT(mtcars, 'cars')
#' # Now the filesystem has, in '.':
#' # ├── mtcars
#' # │   ├── 2020_04_09_16_40.parquet
#' # │   ├── 2020_04_09_16_41.parquet
#'
#' @export
storePQT <- function(obj, fname, basepath='.') {
  
  if (!dir.exists(basepath))
    stop(sprintf("Basepath '%s' does not exist. Cannot write file.", basepath))
  
  fullPath <- file.path(basepath, fname)
  
  # Create the directory for 'fname' if it doesn't exist. Notify the user.
  if (!dir.exists(fullPath)) {
    message(sprintf("Creating directory %s", fullPath))
    success <- dir.create(fullPath, recursive = FALSE, showWarnings = TRUE)
    
    if (any(!success))
      stop(sprintf("Failed to create directory %s", fullPath))
  }
  
  name <- format(Sys.time(), format="%Y_%m_%d_%H_%M.parquet")
  writepath <- file.path(basepath, fname, name)
  
  #saveRDS(obj, writepath)
  write_parquet(obj, writepath)
  
  message(sprintf("Wrote object to %s", writepath))
}

#' Identify the timestamp of the most recently modified file in the file cache
#' 
#' Returns the timestamp of the most recently modified file. If no such file
#' exists, or if the directory \code{basepath/fname} doesn't exist, returns NA.
#' 
#' @param fname The name of the directory in \code{basepath} where various
#'   revisions of the file are stored. I.e., \code{file.txt} should be a
#'   directory, with revisions of the true \code{file.txt} stored inside of
#'   it.
#'
#' @param basepath A string. The path which stores the \code{fname} directory.
#'   Default '.'
#'
#' @return A POSIXct object specifying the \code{mtime} of the most recently
#'   modified file in \code{basepath/fname}
#'
#' @examples
#' library(lubridate)
#'
#' saveRDS(mtcars, 'cars')
#' saveRDS(mtcars, 'cars')
#' 
#' # Some time elapses...
#' 
#' # Decide if the latest version of 'mtcars' is "too old"
#' if (mostRecentTimestamp('mtcars') %--% now() > hours(24)) {
#'   # Store a "new" version
#'   saveRDS(mtcars, 'cars')
#' } else {
#'   cached_mtcars <- retrieveRDS('mtcars')
#' }
#'
#' @export
mostRecentTimestamp <- function(fname, basepath='.') {
  
  # Construct the path to the folder where all copies of 'fname' should be 
  # stored
  fullpath <- file.path(basepath, fname)
  
  # Make sure that this path is a directory I.e., archive/fname.txt needs to 
  # be a folder, not regular file.
  if (!dir.exists(fullpath))
    return(NA)
  
  # Get all the files in this directory
  dirListing <- list.files(fullpath)
  
  # If there were no files in the directory, we can't retrieve them!
  if (length(dirListing) == 0)
    return(NA)
  
  # Get a list of modification times for each file in the directory
  fullPaths <- file.path(fullpath, dirListing)
  mtimes <- purrr::lift_dl(file.mtime)(fullPaths)
  
  max(mtimes)
}

###################################################

#Return the date and file name of most recent file

datetimeStamp <- function( basepath=".", goalDate = lubridate::now()) {
  # Previously was mostRecentTimestamp() and retrievePath() used by retrieveRDS().
  # 
  #  "basepath": path location following the getwd() result.
  # 
  # This function lists all of the dates a source was archived, and will
  # summarize the most recently received archive per source type detected.
  
  # Confirm that "goalDate" is a Date object.
  assertthat::assert_that(any("POSIXct" %in% class(goalDate)))
  
  
  # Construct the path to the folder where all copies of "storeName" are stored.
  fullpath <- file.path(basepath)
  
  # Confirm directory at the provided path exists.
  if (!dir.exists(fullpath)){
    stop(sprintf("Path '%s' doesn't exist, or isn't a directory", fullpath))
  }
  
  
  # Get all the files in this directory.
  dirListing <- list.files(fullpath)
  
  # If there were no files in the directory, throw an error.
  if (length(dirListing) == 0){
    stop(sprintf("No files were found in dirctory '%s'", fullpath))
  }
  
  
  # For each source available, draw out the most recent time stamp recorded.
  # 
  # The original code was drawing the time information from the file metadata 
  # as opposed to the name. This script instead pulls that information from the
  # file name itself. It also keeps some details about the source type, which is
  # user provided. Notice this assumes files are stored 
  # "source_name_%Y_%m_%d_%H_%M.parquet", as used in storeRDS().
  
  # Assuming the time-stamp elements are the same, pull everything preceding
  # as the "source name" details.
  source <- str_replace(dirListing, "_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}.parquet", "")
  
  ##Extract the date from the file name
  
  extract_date <- function(filename) {
    # Use a regular expression to extract the date in YYYY_MM_DD format
    match <- regmatches(filename, regexpr("\\d{4}_\\d{2}_\\d{2}", filename))
    # Convert to Date format
    as.Date(match, format = "%Y_%m_%d")
  }
  
  #extract_date(all_dates)
  
  file_date <- extract_date(source)
  
  all_files <- data.frame("Source" = source, 
                          "History" = file_date,
                          "filePath" = dirListing)
  
  # From the dates, save the most recently represented data pull for each source, 
  # according to the file name time stamp. Also find the file for each source that 
  # has the minimum time interval between the time stamp and goalDate (Delta).
  
  relative_reports <- all_files %>%
    group_by(Source) %>%
    # Calculate the difference between the time stamp and goalDate.
    mutate(Delta = abs( purrr::map_dbl(History, ~lubridate::int_length(. %--% goalDate)) )/(60*60*24)  ) %>%
    # Store only the min Delta and max History.
    filter(History == max(History)|Delta == min(Delta)) %>%
    # Label the filtered rows by which condition it met, including a condition that
    # denotes if a row met both recent pull and min delta conditions.
    mutate("Status" = ifelse(History == max(History), "Recent Pull", "Min Delta")) %>%
    mutate("Status" = ifelse(Delta == min(Delta) & History == max(History), "Both", Status)) %>%
    ungroup() %>%
    as.data.frame()
  
  # Report the results.
  list("Record History" = all_dates, "Report Relative to Date" = relative_reports)
  
}
