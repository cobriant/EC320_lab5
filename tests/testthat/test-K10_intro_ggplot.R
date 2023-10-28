K10 <- read_file("../../R/K10_intro_ggplot.R")

test_that("K10: Question 1a", {
  skip_incomplete(K10, "1a")
  expect_equal(parse_eval(str_match_q(K10, "1a")), 9)
})

test_that("K10: Question 1b", {
  skip_incomplete(K10, "1b")
  expect_equal(parse_eval(str_match_q(K10, "1b")), 374)
})

test_that("K10: Question 1c", {
  skip_incomplete(K10, "1c")
  expect_equal(length(parse_eval(str_match_q(K10, "1c"))), 9)
})

test_that("K10: Question 2", {
  skip_incomplete(K10, "2")
  all(
    expect_true("GeomBar" %in% class(x$layers[[1]]$geom)),
    expect_true(as_label(parse_eval(str_match_q(K10, 2))$mapping$x) == "sex")
  )
})

test_that("K10: Question 3", {
  skip_incomplete(K10, "3")
  all(
    expect_true(str_detect(str_match_q(K10, 3)[[1]], "geom_bar")),
    expect_true(as_label(parse_eval(str_match_q(K10, 3))$mapping$x) == "alcohol")
  )
})

test_that("K10: Question 4", {
  skip_incomplete(K10, "4")
  all(
    expect_true(str_detect(str_match_q(K10, 4)[[1]], "geom_histogram")),
    expect_true(as_label(parse_eval(str_match_q(K10, 4))$mapping$x) == "final_grade")
  )
})

test_that("K10: Question 5", {
  skip_incomplete(K10, 5)
  all(
    expect_true(str_detect(str_match_q(K10, 5)[[1]], "geom_bar")),
    expect_true(as_label(parse_eval(str_match_q(K10, 5))$mapping$x) == "study_time")
  )
})

test_that("K10: Question 6", {
  skip_incomplete(K10, 6)
  all(
    expect_true(str_detect(str_match_q(K10, 6)[[1]], "geom_bar")),
    expect_true(as_label(parse_eval(str_match_q(K10, 6))$mapping$x) == "romantic")
  )
})

test_that("K10: Question 7", {
  skip_incomplete(K10, 7)
  all(
    expect_true(str_detect(str_match_q(K10, 7)[[1]], "geom_point")),
    expect_true(as_label(parse_eval(str_match_q(K10, 7))$mapping$x) == "grade1"),
    expect_true(as_label(parse_eval(str_match_q(K10, 7))$mapping$y) == "grade2")
  )
})

test_that("K10: Question 8", {
  skip_incomplete(K10, 8)
  all(
    expect_true(str_detect(str_match_q(K10, 8)[[1]], "geom_point")),
    expect_true(str_detect(str_match_q(K10, 8)[[1]], "geom_smooth")),
    expect_true(as_label(parse_eval(str_match_q(K10, 8))$mapping$x) == "absences"),
    expect_true(as_label(parse_eval(str_match_q(K10, 8))$mapping$y) == "grade2")
  )
})

test_that("K10: Question 9", {
  skip_incomplete(K10, 9)
  all(
    expect_true(str_detect(str_match_q(K10, 9)[[1]], "geom_boxplot")),
    expect_true(as_label(parse_eval(str_match_q(K10, 9))$mapping$x) == "alcohol"),
    expect_true(as_label(parse_eval(str_match_q(K10, 9))$mapping$y) == "final_grade")
  )
})

test_that("K10: Question 10", {
  skip_incomplete(K10, 10)
  all(
    expect_true(str_detect(str_match_q(K10, 10)[[1]], "geom_bar")),
    expect_true(str_detect(str_match_q(K10, 10)[[1]], "facet_wrap")),
    expect_true(str_detect(str_match_q(K10, 10)[[1]], "romantic")),
    expect_true(str_detect(str_match_q(K10, 10)[[1]], "sex"))
  )
})




