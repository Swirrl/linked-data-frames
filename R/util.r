#' Execute SPARQL select query and parse results
#'
#' @param progress_env an environment across which to retain the progress bar
#' @return A progress bar function for use with httr
download_progress <- function(progress_env = new.env()) {
  bar <- NULL

  show_progress <- function(down, up) {
    total <- down[[1]]
    now <- down[[2]]

    # ignore start-up (where down & up are zero) and
    # upload during query POST before download begins
    # by waiting until either down value is non-zero
    if ((total+now) > 0) {
      if (is.null(bar)) {
        bar <<- cli::cli_progress_bar("Downloading data",
                                      type="download",
                                      total = ifelse(total==0, NA_real_, total),
                                      .envir=progress_env)
      }
      if (now != 0) {
        if (now == total) {
          cli::cli_progress_done(id=bar, .envir=progress_env)
        } else {
          cli::cli_progress_update(id=bar, set=now, .envir=progress_env)
        }
      }
    }
    TRUE
  }

  show_progress
}
