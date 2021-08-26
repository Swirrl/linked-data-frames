# this is going to overlap with lubridate Intervals

describe("intervals", {
  year <- interval("http://reference.data.gov.uk/id/year/2020")
  month <- interval("http://reference.data.gov.uk/id/month/2020-07")
  day <- interval("http://reference.data.gov.uk/id/day/2020-04-22")
  p7d <- interval("http://reference.data.gov.uk/id/gregorian-interval/2020-04-27T00:00:00/P7D")
  gov_year <- interval("http://reference.data.gov.uk/id/government-year/2019-2020")

  test_that("types", {
    expect_equal(int_type(year), "year")
    expect_equal(int_type(month), "month")
    expect_equal(int_type(day), "day")
    expect_equal(int_type(p7d), "gregorian-interval")
    expect_equal(int_type(gov_year), "government-year")
  })

  test_that("labels", {
    expect_equal(label(year), "2020")
    expect_equal(label(month), "2020-07")
    expect_equal(label(day), "2020-04-22")
    expect_equal(label(p7d), "2020-04-27 P7D")
    expect_equal(label(gov_year), "2019-2020")
  })

  test_that("start and end datetimes", {
    expect_equal(int_start(year), as.Date("2020-01-01"))
    expect_equal(int_end(year), as.Date("2020-12-31"))

    expect_equal(int_start(month), as.Date("2020-07-01"))
    expect_equal(int_end(month), as.Date("2020-07-31"))

    expect_equal(int_start(day), as.Date("2020-04-22"))
    expect_equal(int_end(day), as.Date("2020-04-22"))

    expect_equal(int_start(p7d), as.Date("2020-04-27"))
    expect_equal(int_end(p7d), as.Date("2020-05-04"))

    expect_equal(int_start(gov_year), as.Date("2019-04-01"))
    expect_equal(int_end(gov_year), as.Date("2020-03-31"))
  })

  test_that("casting is possible", {
    expect_equal(as.character(month), "http://reference.data.gov.uk/id/month/2020-07")
  })
})


