library(testthat)
library(withr)
library(arrow)
library(dplyr)
library(stringr)

test_that("load_existing_data loads valid Parquet data", {
    with_tempdir({
        df <- data.frame(datum = as.Date("2024-01-01") + 0:2, value = 1:3, month = c("2024-01", "2024-01", "2024-01"))
        write_dataset(df, "testdata", partitioning = "month", format = "parquet")
        result <- load_existing_data("testdata")
        expect_s3_class(result, "data.frame")
        expect_equal(nrow(result), 3)
        expect_true(all(c("datum", "value", "month") %in% names(result)))
        expect_true(inherits(result$datum, "Date"))
    })
})

test_that("load_existing_data errors if directory does not exist", {
    expect_error(load_existing_data("nonexistent_dir"), "Kein bestehender Datensatz")
})

test_that("load_existing_data errors on invalid Parquet data", {
    with_tempdir({
        dir.create("badparquet")
        file.create(file.path("badparquet", "file.parquet"))
        expect_error(load_existing_data("badparquet"), "Fehler beim Laden des Datensatzes")
    })
})

test_that("create_dates_to_crawl returns correct sequence", {
    df <- data.frame(datum = as.Date("2024-01-01"))
    today <- Sys.Date()
    seq_expected <- seq(as.Date("2024-01-01"), today, by = "days")
    expect_equal(create_dates_to_crawl(df), seq_expected)
})

test_that("get_month_list extracts unique months", {
    dates <- as.Date(c("2024-01-01", "2024-01-15", "2024-02-01"))
    months <- get_month_list(dates)
    expect_true(all(months %in% c("2024-01", "2024-02")))
})

test_that("split_date splits date correctly", {
    d <- as.Date("2024-03-15")
    parts <- split_date(d)
    expect_equal(parts$year, "2024")
    expect_equal(parts$month, "03")
    expect_equal(parts$day, "15")
})

