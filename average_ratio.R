#' Script for calculating average ratio of number of cases of coronavirus by country
#' Original idea by Daniel Victor Tausk



get_data <- function() {
  url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  read.csv(url, stringsAsFactors = F)
}

wrangle_data <- function(df) {
  df %>%
    dplyr::select(-Province.State,  -Lat, -Long) %>%
    group_by(Country.Region) %>%
    summarise_all(sum) %>%
    gather(date, cases, -Country.Region) %>%
    mutate(date = str_replace(date, "^X", "") %>% mdy) 
    
}

calculate_ratio <- function(df) {
  df %>%
    arrange(date) %>%
    group_by(Country.Region) %>%
    mutate(ratio = cases/lag(cases))
}
  

calculate_mov_avg <- function(vec, n = 5, agg_func, std_func) {
  map(0:(n-1), ~ lag(vec, .x)) %>%
    reduce(agg_func) %>%
    std_func(n)
}

analyze_country <- function(country = "Italy", date_f = "25/02/2020", days_ma = 5,
                            agg_func = `+`, std_func = function(x,n) `/`(x, n)) {
  df <- calculate_ratio(wrangle_data(get_data()))
  df %>%
    mutate(avg_ratio = calculate_mov_avg(ratio, n = days_ma, agg_func = agg_func, std_func = std_func)) %>%
    filter(Country.Region %in% country) %>%
    filter(date > dmy(date_f))
}


plot_country <- function(df) {
  df %>%
  ggplot(aes(x = date, y = avg_ratio)) +
    geom_line() + 
    theme_bw() +
    facet_wrap(~ Country.Region)
}




# Calculate arithmetic average for countries with 5 day moving averages
analyze_country(country = c("Italy", "Germany","France","Spain","Brazil","China", "US",
                         "Korea, South", "Japan"), date_f = "25/02/2020", days_ma = 5) %>%
  plot_country()

# Calculate geometric average for countries with 5 day moving averages
analyze_country(country = c("Italy", "Germany","France","Spain","Brazil","China", "US",
                            "Korea, South", "Japan"), date_f = "25/02/2020", days_ma = 5,
                agg_func = `*`, std_func = function(x, n) `^`(x, (1/n))) %>%
  plot_country()

