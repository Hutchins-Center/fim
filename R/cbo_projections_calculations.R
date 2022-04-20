
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
cola_adjustment <- function(df){

  get_cola_rate <- function(df){
    df %>%
      mutate(cpiu_g = fim::q_a(cpiu) / 100, #cpiu_g = quarterly annualized growth rate of cpiu (urban cpi)
             cola_rate = if_else(lubridate::quarter(date) == 1,
                                 lag(cpiu_g, 2),# cola_rate= takes the cpiu_g from q3 of previous year and gives to the q1 
                                 NULL)) %>%
      tidyr::fill(cola_rate)#carries forward the cola_rate from q1 to the rest of the year 
    ##smooths out CBO adjustment in q1 
  }
  smooth_transfers_net_health_ui <- function(df){
    df %>%
      mutate(gftfp_unadj = gftfp,#Federal Social Benefits
             health_ui = TTR::SMA(yptmd + yptmr + yptu, n = 4),#simple moving average over 4 quarters of the sum of 
                          #Total Medicaid Spending, Medicare, Unemployment Insurance (NIPA Definition - i.e. no distinction bet fed and state)

             smooth_gftfp_minus_health_ui = TTR::SMA((gftfp - health_ui) * (1 - cola_rate), n =4),#social benefits less health and UI, smoothed and cost adjusted  
             gftfp = smooth_gftfp_minus_health_ui * (1 + cola_rate) + health_ui) #getting a measure of federal social benefits which is smooth with cost adj health and UI 
  }

#applying the two functions to the data frame 
  df %>%
    get_cola_rate() %>%
    smooth_transfers_net_health_ui()
  
}


#' Alternative tax scenario
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
  alternative_tax_scenario <- function(df){
    # Construct alternative scenario from CBO for personal current taxes, under which the TCJA provisions for income taxes don't
    # expire in 2025
    expdate <- tsibble::yearquarter('2025 Q3')
    
    df %>%
      mutate(gfrptCurrentLaw = gfrpt,
             gfrptCurrentLaw_growth = gfrpt_growth,
             gfrpt_growth =
               if_else(date > expdate,
                       lag(gfrpt_growth),
                       gfrpt_growth,
                       missing = NULL
               ),
             gfrpt  = if_else(date >= expdate,
                              lag(gfrpt) * (1 + gfrpt_growth / 400),#for deannualizing the numerator 
                              gfrpt))
  }
#' Implicit price deflators
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
  implicit_price_deflators <- function(df){
    # Implicit price deflators
    df %>% 
      mutate(jgf =  gf/gfh, #Federal Purchases (NIPA consistent)/Real Federal Purchases
             jgs = gs/gsh, #State Purchases (NIPA consistent)/Real State Purchases
             jc = c/ch)#Consumption/Real Consumption
  }
#' Forecast state taxes
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
   forecast_state_taxes <- function(df){
    df %>% 
      left_join(hist %>%
                  select(date, gsrpt ,gsrpri, gsrcp ,gsrs),
                all.x = F) %>%
      filter(date > '2016-12-31') %>%
      mutate(
        across(
          .cols = c("gsrpt" ,"gsrpri", "gsrcp" ,"gsrs"),
          .fns = ~ na.locf(. / gdp) * gdp
        )
      )
  }
#' Calculate growth rates
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
  growth_rates <- function(df){
    df %>%
      mutate(
        across(
          .cols = where(is.numeric) & !ends_with('_growth'),
          .fns = ~ q_g(.),#simple growth rate using levels from preceding period: qoq 
          .names = "{.col}_growth"
        ) 
      )
  }
  
  

#' Get growth rates of federal transfers
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
federal_transfers_growth_rates <- function(df){
  df %>%
    mutate(
      across(
        .cols = c("gfrpt",  "gfrpri",  "gfrcp",  "gfrs"),
        .fns = ~ q_g(.x),
        .names = "{.col}_g"
      )
    ) 
  }

#' Get health growth rates
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
health_outlays_growth_rates <- function(df){
  df %>%
    mutate(
      across(
        .cols = c("yptmr",  "yptmd" ),
        .fns = ~ q_g(.x),
        .names = "{.col}_growth"
      )
    ) 
}

smooth_budget_series <- function(df) {
  federal_taxes <- c('gfrpt', 'gfrpri', 'gfrcp', 'gfrs')#Federal Personal Income Taxes, Federal Production Taxes, Federal Corporate Income Taxes, Federal Social Insurance

  health_outlays <- c('yptmd', 'yptmr')#Total Medicaid Spending, Medicare

  unemployment_insurance <- 'yptu'#Unemployment Insurance (NIPA Definition)

  df %>%
    #taking the average of the 4 preceding periods within each column (same as simple moving average)
    mutate(across(all_of(c(federal_taxes, health_outlays, unemployment_insurance)),
                  ~ zoo::rollapply(.x, width = 4, mean, fill = NA,min_obs = 1, align = 'right')))#align takes the preceding 4, ~means custom fxn
}
