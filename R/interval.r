# type for the intervals ontology: http://reference.data.gov.uk/def/intervals

new_interval <- function(uri=character()) {
  vec_assert(uri, character())
  new_vctr(uri, class = "ldf_interval")
}

interval <- function(uri) {
  new_interval(uri)
}

int_start <- function(int) {
  date_strings <- purrr::map_chr(int, function(i) {
    switch (int_type(i),
            day = int_value(i),
            year = paste0(int_value(i),"-01-01"),
            "gregorian-interval" = int_date(i),
            stop("unknown interval type"))
  })
  as.Date(date_strings)
}

int_end <- function(int) {
  date_strings <- purrr::map_chr(int, function(i) {
    switch (int_type(i),
      day = int_value(i),
      year = paste0(int_value(i),"-12-31"),
      "gregorian-interval" = paste0(as.Date(int_date(i)) + int_duration_days(i)),
      stop("unknown interval type")
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

label.ldf_interval <- function(int) {
  purrr::map_chr(int, function(i) {
    switch (int_type(i),
            day = int_value(i),
            year = int_value(i),
            "gregorian-interval" = paste(as.Date(int_date(i)), int_duration(i), sep=" "),
            stop("unknown interval type")
    )
  })
}

format.ldf_interval <- function(x, ...) {
  format(label(x))
}

# vec_ptype2.ldf_interval.character <- function(x, y, ...) character()
# vec_ptype2.character.ldf_interval <- function(x, y, ...) character()
# vec_cast.ldf_interval.character <- function(x, to, ...) interval(x)
# vec_cast.character.ldf_interval <- function(x, to, ...) vec_data(x)
