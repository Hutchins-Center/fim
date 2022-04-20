
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
format_tsibble <- function(df){
  df %>%
    mutate(date = tsibble::yearquarter(date)) %>%#formatting date
    relocate(id, .before = date) %>%
    tsibble::as_tsibble(key = id, index = date)#setting data as time series (id = historical and projection, date = quarter year)
}

annual_to_quarter <- function(df){ 
  year <-
    df %>%
      tsibble::index_var()
  min <-
    df %>%
    select(rlang::enexpr(year)) %>%
    min() 
  
  max <- 
    df %>%
    select(rlang::enexpr(year)) %>%
    max()
  start <- tsibble::yearquarter(glue::glue('{min} Q1'))
  end <- tsibble::yearquarter(glue::glue('{max} Q4'))
  x <- seq(start,  end, by = 1)#creates vector for time variable
  
  df %>%
    as_tibble() %>%
    slice(rep(1:n(), each=4)) %>%#takes each row and repeats it 4 times
    mutate(date = tsibble::yearquarter(x, fiscal_start =  1)) %>%#fiscal_start option for yearquarter converts to calendar year
    relocate(date, .before =  everything()) %>%
    tsibble::as_tsibble(index = date)
}

 

fiscal_to_calendar <- function(df){
  index <-
    df %>%
    index_var()  
  index <- rlang::ensym(index)#allows you to be general with your index var
  df %>%
    mutate("{{index}}" := {{index}} - 1)#special assignment bc this is a fxn, look into it in meta programming 
    
}  

monthly_to_quarterly <- function(df){
  df %>%
    mutate(yq = tsibble::yearquarter(date)) %>%
    as_tsibble(index = date) %>%
    select(date, yq, everything()) %>%
    index_by(yq) %>%
    mutate(
      across(
        .cols = where(is.numeric), 
        .fns = ~ mean(.x, na.rm = TRUE)
      )
    ) %>%
    filter(row_number()== n()) %>%
    ungroup() %>%
    select(-yq)
}
