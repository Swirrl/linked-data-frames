# Intervals

#' @importFrom zeallot %<-%
NULL

# type for the intervals ontology: http://reference.data.gov.uk/def/intervals
# https://github.com/epimorphics/IntervalServer/blob/master/interval-uris.md


#' `interval` vector
#'
#' This represents an interval from the UK Government
#' [intervals ontology](http://reference.data.gov.uk/def/intervals).
#'
#' `is_interval` returns `TRUE` if `x` is an interval and `FALSE` otherwise.
#'
#' @param uri A character vector of URIs
#' @param x An interval
#' @return An S3 vector of class `ldf_interval`.
#' @export
#' @examples
#' year <- interval("http://reference.data.gov.uk/id/year/2020")
#' is_interval(year)
#' month <- interval("http://reference.data.gov.uk/id/month/2020-07")
#' day <- interval("http://reference.data.gov.uk/id/day/2020-04-22")
#' p7d <- interval("http://reference.data.gov.uk/id/gregorian-interval/2020-04-27T00:00:00/P7D")
#' gov_year <- interval("http://reference.data.gov.uk/id/government-year/2019-2020")
interval <- function(uri) {
  new_interval(uri)
}

new_interval <- function(uri=character()) {
  vec_assert(uri, character())
  new_vctr(uri, class = "ldf_interval")
}

#' @export
#' @rdname interval
is_interval <- function(x) {
  inherits(x, "ldf_interval")
}

#' The start date of an interval
#'
#' @param int An interval
#' @return A Date
#' @export
int_start <- function(int) {
  date_strings <- purrr::map_chr(int, function(i) {
    switch (int_type(i),
            day = int_value(i),
            month = paste0(int_value(i),"-01"),
            year = paste0(int_value(i),"-01-01"),
            "gregorian-interval" = int_date(i),
            "government-year" = paste0(substr(int_value(i),1,4),"-04-01"),
            stop("unknown interval type: ", int_type(i)))
  })
  as.Date(date_strings)
}

#' The end date of an interval
#'
#' @param int An interval
#' @return A Date
#' @export
int_end <- function(int) {
  date_strings <- purrr::map_chr(int, function(i) {
    switch (int_type(i),
      day = int_value(i),
      month = month_end(int_value(i)),
      year = paste0(int_value(i),"-12-31"),
      "gregorian-interval" = paste0(as.Date(int_date(i)) + int_duration_days(i)),
      "government-year" = paste0(substr(int_value(i),6,9),"-03-31"),
      stop("unknown interval type: ", int_type(i))
    )
  })
  as.Date(date_strings)
}

int_type <- function(int) {
  stringr::str_match(vec_data(int), "^http://reference.data.gov.uk/id/([\\w-]+)/.*$")[,2]
}

int_value <- function(int) {
  stringr::str_match(vec_data(int), "^http://reference.data.gov.uk/id/[\\w-]+/(.*)$")[,2]
}

int_date <- function(int) {
  unlist(lapply(strsplit(int_value(int),"/"), function(x) x[1]))
}

int_duration <- function(int) {
  unlist(lapply(strsplit(int_value(int),"/"), function(x) x[2]))
}

int_duration_days <- function(int) {
  components <- stringr::str_match(int_duration(int), "^P(\\d+)(\\w+)$")
  switch(components[,3],
    D = as.integer(components[,2]),
    stop("unknown interval duration")
  )
}

#' @export
label.ldf_interval <- function(x) {
  purrr::map_chr(x, function(i) {
    switch (int_type(i),
            day = int_value(i),
            month = int_value(i),
            year = int_value(i),
            "gregorian-interval" = paste(as.Date(int_date(i)), int_duration(i), sep=" "),
            "government-year" = int_value(i),
            stop("unknown interval type: ", int_type(i))
    )
  })
}

#' @export
format.ldf_interval <- function(x, ...) {
  format(label(x))
}

#' @export
vec_ptype2.ldf_interval.character <- function(x, y, ...) character()

#' @export
vec_ptype2.character.ldf_interval <- function(x, y, ...) character()

#' @export
vec_cast.ldf_interval.character <- function(x, to, ...) interval(x)

#' @export
vec_cast.character.ldf_interval <- function(x, to, ...) vec_data(x) # label.ldf_interval

month_end <- function(x) {
  year <- month <- NULL # for test check
  c(year, month) %<-% as.numeric(strsplit(x,"-")[[1]])
  as.character(as.Date(paste0(c(year, month+1, 1), collapse="-")) - 1)
}
