


get_cbo_projections <- function(){

  fim::projections %>% #created using projections.R script that reads in from CBO projections spreadsheet
    cola_adjustment() %>% #inflation adjusts health spending and ui in federal social benefits, smooths the series
    smooth_budget_series() %>% #smooths federal taxes, health outlays, and ui
    implicit_price_deflators() %>%#computes deflators for federal and state purchases and consumption
    growth_rates() %>%#computes qoq growth rates of the levels 
    alternative_tax_scenario() %>% #keeps the current law tax growth for federal personal income taxes post-2025
    format_tsibble() %>% #declares data as time series as with id and time variable
    select(id, date, gdp, gdph, gdppothq, gdppotq, starts_with('j'), dc, c, ch ,ends_with('growth'), cpiu, federal_ui, state_ui, unemployment_rate)
      #keeps the relevant variables
}

safe_quarter  <- function(df){
  df %>% 
    as_tibble() %>% 
    mutate(date = as.character(date))
}

undo_safe_quarter <- function(df){
  df %>% 
    mutate(date = yearquarter(date)) %>% 
    as_tsibble(index = 'date')
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
read_data <- function(){

 
  projections <- get_cbo_projections()#cleans the CBO projection data from projections.R and creates the projections data frame   
  
  #Historical data from Haver created in national_accounts.R
  fim::national_accounts %>%
    coalesce_join(projections, by = 'date') %>% #joining projections using date variable
    as_tsibble(key = id, index = date)#declaring time series with id and date that now has historical and projection 
}



#' Load contributions used in figures
#'
#' @return
#' @export
#'
#' @examples
load_contributions <- function(){
  start <- lubridate::as_date("2000-01-01")
  end <- lubridate::as_date("2022-12-31")
  

  
  readxl::read_xlsx(glue::glue(drake::file_in("results/{get_current_month()}/fim-{get_current_month()}.xlsx"))) %>%
    dplyr::select(date, fiscal_impact, fiscal_impact_moving_average,
           tidyselect::ends_with('cont'), recession) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>% 
    dplyr::filter(date > start & date <= end)
}
