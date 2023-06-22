library(tidyverse)
library(tsibble)
library(rlang)
library(janitor)
#When run the code, please set working directory, these four package will help me to reform the data
#The general idea of the I write all the function I needed to reform the data, when I use loop import the required file name, variable name and the required formula (sum and mean) one by one 

filter_fr_var <- function(x, var) {
  filter(x, grepl(sprintf("(%s,DE)|(AVG,NAC,USD)", var), !!sym(names(x)[1])))
}#only select the data with,FR or AVG,NAC,USD(it becuase the exchange rate is by EUR to USD, so EUROSTAT does not label with France.
#in general data from EUROSTAT, the general form of the data is classified by Unit of measure,Statistical classification of economic activities in the European Community,National accounts indicator,Geopolitical entity, 
#so we can simple use ,Geopolitical entity(in our case,FR) to remove all other country data and only keep France data.

clean_num <- function(x) {
  gsub("^(\\s*)(\\d+)(\\.?)(\\d*)(\\s|[A-Za-z])*$", "\\2\\3\\4", x) |>
    as.numeric() |>
    suppressWarnings()
}
#in some euro data, even there dispaly as a number, but they include other special characters and not made as numeric,
#Therefore, we need to remove all special characters and set all of our data as numeric, furthermore, if there as missing variable, we reset them as by NA.

format_date <- function(x) {
  if (all(grepl("M", x))) yearmonth(x) else yearquarter(x)
}
#the data come from EUROSTAT has two difference time variables, one is group by monthly, and other is by quarterly, so we select all the time varible.

aggregate_to_quarter <- function(x, agg_fun) {
  if (inherits(x$date, "yearmonth")) {
    x <- mutate(x, .group = yearquarter(date)) |>
      group_by(.group) |>
      summarise(!!names(x)[2] := (!!agg_fun)(!!sym(names(x)[2]))) |>
      rename(date = .group)
  }
  x
}

#after I select all time varaibles, I need reform all data group by monthly to quarterly and change their name to quarterly
#under this step, I select all monthly data, and regroup by quarter.
#In addition, in my data initial file I call the fun column (see dataneeded.csv for details), and in the third column of that file I call two formulas, mean and sum, so when changing the time variable to quarterly, I also use the function in the third column of dataneeded.csv to change the monthly data according to Request to change to quarterly data using sum or mean. They are done by the function "process_file" 

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
#I have changed the data from rows to columns. And moved all the data I needed variable name to the first row (these are the column coordinates in EUROSTAT's data)
#In EU data, they were last updated in April 2023, but since I use quarterly data, I removed the second quarter of 2023.
#all above codes are the data come from EUROSTAT as they use a tsv file. 



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
#after I finish the data process of EUORSTAT, we move for the data from EMBER. (electricity price for EU.) the data is a csv file.


process_csv_file <- function(file) {
  file |>
    read_csv() |>
    filter(Country == "Germany") |>
    select(3, 4) |>
    rename(date = Date) |>
    mutate(date = yearquarter(date), .group = date) |>
    group_by(.group) |>
    (\(.) summarise(., !!names(.)[2] := mean(!!sym(names(.)[2]))))() |>
    rename(date = .group) |>
    as_tsibble(index = date)
}

#In CSV file, The data is kind of well sorted, so we selected the data with France ourselves. However, this data is daily data and I need to change it to quarterly data. I calculated the quarterly mean price and merged it with the above processed file in the same format.

process_data <- function() {
  process_csv_file("european_wholesale_electricity_price_data_daily-5.csv") |>
    right_join(process_tsv_files("../dataneeded.csv")) |>
    drop_na() |>
    fill_gaps() |>
    (\(.) set_names(., make_clean_names(names(.))))()
}
#We combined all the data above and then removed all the columns with NA, as the data was not collected at the same time for different files, and we ended up with perfect data from the first quarter of 2015 to the first quarter of 2023.

write_csv(process_data(), "../final_data_Germany.csv")
#In the final step, we export the integrated data and create a new csv file