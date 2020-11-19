describe("resources can be constructed", {
  test_that("with zero-length constructor", {
    expect_equal(length(resource()),0)
  })

  test_that("with URIs as strings", {
    r <- resource(c("http://example.net/id/1","http://example.net/id/2"))
    expect_s3_class(r, "ldf_resource")
    expect_equal(length(r),2)
  })

  test_that("with URIs as factors", {
    r <- resource(factor(c("http://example.net/id/1","http://example.net/id/2")))
    expect_type(uri(r), "character")
  })

  test_that("with NAs", {
    r <- resource(c(NA,NA))
    expect_equal(length(r),2)
  })
})

describe("resources can have arbitrary descriptions", {
  uris <- c("http://example.net/id/1",
            "http://example.net/id/2",
            "http://example.net/id/3")

  test_that("properties retrieved via uri", {
    r <- resource(uris[3:1], data=data.frame(uri=uris, id=1:3))
    expect_equal(property(r, "id"), 3:1)
  })

  test_that("warning is given if property is missing", {
    r <- resource(uris, data=data.frame(uri=uris))
    expect_warning(property(r, "id"))
  })

  test_that("descriptions must include uri", {
    expect_error(resource(uris, data=data.frame(id=1:3)))
  })

  test_that("data can't contain duplicate uris", {
    expect_error(resource(uris, data=data.frame(uri=uris[c(1,1,2,3)])))
  })

  test_that("data must contain all uris", {
    expect_error(resource(uris, data=data.frame(uri=uris[1:2])))
  })

  test_that("data doesn't need to contain entry for NA uris", {
    r <- resource(NA, data=data.frame(uri=uris[1:2]))
    expect_equal(uri(r), NA_character_) # i.e. no error raised
  })

  test_that("data can be a tibble", {
    r <- resource("a", tibble::tibble(uri="a",label="A"))
    expect_equal(label(r), "A")
  })
})

describe("accessors", {
  uris <- c("http://example.net/id/apple",
            "http://example.net/id/banana",
            "http://example.net/id/carrot")
  labels <- c("Apple","Banana","Carrot")
  sort_priorities <- 1:3
  data <- data.frame(uri=uris,
                     label=labels,
                     sort_priority=sort_priorities,
                     stringsAsFactors = F)
  r <- resource(uris, data)

  test_that("for uri", {
    expect_equal(uri(r), uris)
  })

  test_that("for label", {
    expect_equal(label(r), labels)
  })

  test_that("for sort priority", {
    expect_equal(sort_priority(r), sort_priorities)
  })
})

describe("curie function", {
  uris <- c("http://example.net/id/apple",
            "http://example.net/id/banana",
            "http://example.net/id/carrot")
  r <- resource(uris)

  test_that("returns full URIs without prefix", {
    expect_equal(curie(r), uris)
  })

  test_that("compacts URIs with prefix", {
    expect_equal(curie(r, c(eg="http://example.net/id/")),
                 c("eg:apple", "eg:banana", "eg:carrot"))
  })

  test_that("default prefixes set in option", {
    withr::with_options(
      list(ldf_prefixes=c(eg="http://example.net/id/")),
      expect_equal(curie(r), c("eg:apple", "eg:banana", "eg:carrot"))
    )
  })
})

describe("formatter", {
  uris <- c("http://example.net/id/apple",
            "http://example.net/id/banana",
            "http://example.net/id/carrot")

  test_that("defaults to curie", {
    r <- resource(uris)
    expect_equal(format(r), format(uris))
  })

  test_that("uses label if available", {
    labels <- c("Apple","Banana","Carrot")
    r <- resource(uris, data=data.frame(uri=uris, label=labels, stringsAsFactors = F))
    expect_equal(format(r), format(labels))
  })
})

describe("casting and coercion", {
  test_that("may be cast to itself", {
    expect_equal(vec_cast(resource("a"), resource()), resource("a"))
  })

  test_that("may be cast to character", {
    expect_equal(vec_cast(resource("a", data=data.frame(uri="a",label="A",stringsAsFactors=F)), character()),
                 c("a")) # TODO: can we change this to return the label instead (without breaking other stuff)
  })

  test_that("may be cast from a character", {
    expect_equal(vec_cast(c("a"), resource()),
                 resource("a"))
  })
})

describe("combining", {
  test_that("may be cast from a character", {
    expect_equal(vec_c(c("a"), resource("b")),
                 c("a", "b"))
  })
})

describe("works with other functions", {
  undescribed_r <- resource(c("http://example.net/id/1",
                              "http://example.net/id/1",
                              "http://example.net/id/2"))
  uris <- c("http://example.net/id/apple",
            "http://example.net/id/banana",
            "http://example.net/id/carrot")
  labels <- c("Apple","Banana","Carrot")
  data <- data.frame(uri=uris,
                     label=labels,
                     stringsAsFactors = F)
  described_r <- resource(uris, data)

  test_that("table of undescribed resources", {
    tbl <- table(undescribed_r)
    expect_equal(length(tbl), 2)
    expect_equal(tbl[["http://example.net/id/1"]], 2)
  })

  test_that("table of undescribed resources", {
    tbl <- table(described_r)
    expect_equal(length(tbl), 3)
    expect_equal(tbl[["http://example.net/id/carrot"]], 1)
    # would be nicer if this was the label
    # can't do that because factor(r) sees the underlying type is character and doesn't attempt to call as.character
  })

  test_that("as.matrix", {
    m <- as.matrix(data.frame(r=described_r))
    expect_equal(dim(m), c(3,1))

    m <- as.matrix(data.frame(r=described_r))
    expect_equal(dim(m), c(3,1))
  })
})
# with data specified as a tibble or sf object
# underlying type could be integer with uri mapping
# sorts by sort priority
# ensure merges via uri - and that data is merged
# subset data when uri is subset
# vec_proxy/ vec_restore should update data
# if character gets cast to resource (as the richer type) and the data for that URI is missing, what should happen?
#   raise a warning? allow it to happen (to be cleaned up later) or require that the character be cast explicitly
#   (with appropriate data) and not allow coercion.
# Printing with `str`, need to do `str(x, max.level=1)`.
# Can we use `property` function with tidy-select? `?tidyr_tidy_select` e.g. `select(x, label(column))`

# extract geo? demonstrate that...
# can show code or label
# can find parent
# can find geometry