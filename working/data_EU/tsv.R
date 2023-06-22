library(tidyverse)
library(tsibble)
library(rlang)
library(janitor)

filter_fr_var <- function(x, var) {
  filter(x, grepl(sprintf("(%s,FR)|(AVG,NAC,USD)", var), !!sym(names(x)[1])))
}

clean_num <- function(x) {
  gsub("^(\\s*)(\\d+)(\\.?)(\\d*)(\\s|[A-Za-z])*$", "\\2\\3\\4", x) |>
    as.numeric() |>
    suppressWarnings()
}

format_date <- function(x) {
  if (all(grepl("M", x))) yearmonth(x) else yearquarter(x)
}

aggregate_to_quarter <- function(x, agg_fun) {
  if (inherits(x$date, "yearmonth")) {
    x <- mutate(x, .group = yearquarter(date)) |>
      group_by(.group) |>
      summarise(!!names(x)[2] := (!!agg_fun)(!!sym(names(x)[2]))) |>
      rename(date = .group)
  }
  x
}

process_file <- function(file, var, fun) {
  file |>
    read_delim("\t") |>
    filter_fr_var(var) |>
    (\(.) mutate(., across(names(.)[-1], clean_num)))() |>
    pivot_longer(-1, names_to = "date", values_to = var) |>
    mutate(date = format_date(date)) |>
    select(-1) |>
    aggregate_to_quarter(fun) |>
    as_tsibble(index = date) |>
    filter(date < yearquarter("2023Q2"))
}

process_tsv_files <- function(data_spec) {
  data_ls <- data_spec |>
    read_csv() |>
    array_branch(1) |>
    map(\(x) process_file(x[1], x[2], eval_tidy(parse_expr(x[3]))))
  data <- data_ls[[1]]
  invisible(map(data_ls[-1], \(x) {
    data <<- full_join(data, x, by = "date")
  }))
  data
}

process_csv_file <- function(file) {
  file |>
    read_csv() |>
    filter(Country == "France") |>
    select(3, 4) |>
    rename(date = Date) |>
    mutate(date = yearquarter(date), .group = date) |>
    group_by(.group) |>
    (\(.) summarise(., !!names(.)[2] := mean(!!sym(names(.)[2]))))() |>
    rename(date = .group) |>
    as_tsibble(index = date)
}

process_data <- function() {
  process_csv_file("european_wholesale_electricity_price_data_daily-5.csv") |>
    right_join(process_tsv_files("../dataneeded.csv")) |>
    drop_na() |>
    fill_gaps() |>
    (\(.) set_names(., make_clean_names(names(.))))()
}

write_csv(process_data(), "../final_data.csv")
