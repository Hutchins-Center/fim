# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
Sys.setenv(TZ = 'UTC')
librarian::shelf(Haver, tidyverse, readxl, writexl, tsibble, purrr)

haver.path("//ESDATA01/DLX/DATA/")
devtools::load_all()

# 0.1 Pull Raw Data---------------------------------------------------------------

START <- "01-01-1970"

# Quarterly -------------------------------------------------------------------------------------------------------

# BEA NIPAs 

# Haver codes and FIM variable names. We will want to pull the Haver codes and then rename them with more descriptive variable names in the reference column which are used in the FIM. 
names_usna <- 
  tibble::tribble(
    ~code,                        ~reference,
    "gdp",                             "gdp",
    "gdph",                        "real_gdp",
    "jgdp",                    "gdp_deflator",
    "c",                     "consumption",
    "ch",                "real_consumption",
    "jc",            "consumption_deflator",
    "jgf",      "federal_purchases_deflator",
    "jgs",        "state_purchases_deflator",
    "jgse",     "consumption_grants_deflator",
    "jgsi",      "investment_grants_deflator",
    "yptmr",                        "medicare",
    "yptmd",                        "medicaid",
    "yptu",                              "ui",
    "gtfp",                 "social_benefits",
    "ypog",                 "paymentPersonal",
    "yptx",                  "personal_taxes",
    "ytpi",                "production_taxes",
    "yctlg",                 "corporate_taxes",
    "g",                       "purchases",
    "grcsi",          "paymentSocialInsurance",
    "dc",            "consumption_deflator",
    "gf",               "federal_purchases",
    "gs",                 "state_purchases",
    "gfh",          "real_federal_purchases",
    "gsh",            "real_state_purchases",
    "gfrpt",          "federal_personal_taxes",
    "gfrpri",        "federal_production_taxes",
    "gfrcp",         "federal_corporate_taxes",
    "gfrs",           "federal_payroll_taxes",
    "gftfp",         "federal_social_benefits",
    "gfeg",        "gross_consumption_grants",
    "gsrpt",            "state_personal_taxes",
    "gsrpri",          "state_production_taxes",
    "gsrcp",           "state_corporate_taxes",
    "gsrs",             "state_payroll_taxes",
    "gstfp",           "state_social_benefits",
    "gset",              "state_expenditures",
    "gfeghhx",                   "health_grants",
    "gfeghdx",                 "medicaid_grants",
    "gfeigx",               "investment_grants",
    "gfsub",               "federal_subsidies",
    "gssub",                 "state_subsidies",
    "gsub",                       "subsidies",
    "gftfpe",                   "rebate_checks",
    "gftfpr", "medicare_reimbursement_increase",
    "gftfpp",                   "nonprofit_ppp",
    "gftfpv",  "nonprofit_provider_relief_fund",
    "gfsubp",                             "ppp",
    "gfsubg",                        "aviation",
    "gfsube",              "employee_retention",
    "gfsubs",                         "transit",
    "gfsubf",     "coronavirus_food_assistance",
    "gfsubv",            "provider_relief_fund",
    "gfsubk",                 "paid_sick_leave",
    "gfegc",         "coronavirus_relief_fund",
    "gfege",    "education_stabilization_fund",
    "gfegv",     "provider_relief_fund_grants",
    "yptue",                            "peuc",
    "yptup",                             "pua",
    "yptuc",                             "puc",
    "gftfpu",                    "ui_expansion",
    "yptub",            "ui_extended_benefits",
    "yptol",           "wages_lost_assistance",
    "gfctp",       "capital_transfer_payments",
    "gftffx",                            "snap",
    "ylwsd",              "wages_and_salaries",
    "yop",              "proprietors_income",
    "yri",                   "rental_income",
    "ypiar",                    "asset_income",
    "ycpd",               "corporate_profits",
    "gfsubr",  "restaurant_revitalization_fund",
    "gfsubd",                  "disaster_loans",
    "gftfbdx",                 "social_security"
  )

# Economic Statistics

usecon <- 
  pull_data(c("PCW", "GDPPOTHQ", "GDPPOTQ", "RECESSQ",
              'LASGOVA', 'LALGOVA', 'CPGS'), 
            "usecon",
            start.date = START)

cpi <- 
  pull_data(c('UI'), 'cpidata', start.date = START) %>%
  monthly_to_quarterly() %>% 
  rename(cpiu = ui)


# Wages Lost Assistance Program (Monthly)
wla <- pull_data('YPTOLM',
                 'usna',
                 frequency = 'monthly',
                 start.date = START) %>%
  monthly_to_quarterly() %>%
  mutate(yptolm = na_if(yptolm, 'NaN'))
# Child Tax Credit (Monthly)
ctc <- pull_data('YPTOCM',
                 'usna',
                 frequency = 'monthly',
                 start.date = START) %>%
  monthly_to_quarterly() %>% 
  mutate(yptocm = na_if(yptocm, 'NaN'))

usna <-
  pull_data(names_usna$code,
            "usna",
            start.date = START) %>%
  as_tibble() %>% 
  left_join(cpi) %>%
  left_join(usecon) %>% 
 # left_join(child_tax_credit) %>% 
  # Convert SNAP from millions to billions
  mutate(gftffx = gftffx / 1e3) %>% 
  left_join(ctc, by = 'date')

usethis::use_data(usna)


monthly_state_ui <- c('LICL', 'LWCL', 'LUFP','LULP','LUWC','LUWP','LUBP','LUWB','LUEX','LUD','LUWBY', 'LUBPT', 'LUFPT', 'LULPT')

state_ui <- pull_data(monthly_state_ui,
                         'usecon',
                         start.date = START) %>%
  as_tibble() %>%
  write_xlsx('data/monthly_state_ui.xlsx')
# Write csv to current month's folder
haver_raw_list <- 
  list(national_accounts = usna,
       economic_statistics = usecon)


## Exporting csv with the desired file names and into the right path
output_xlsx <- function(data, names){ 
  folder_path <- "inst/extdata/"
  write_xlsx(data, paste0(folder_path, names, ".xlsx"))
}


list(data = haver_raw_list,
     names = names(haver_raw_list)) %>% 
  purrr::pmap(output_xlsx) 

  df = usna
 
   df %>%
    set_names(
      names_usna %>%
        pull(reference) %>%
        magrittr::extract(
          names(df) %>%
            match(names_usna$code)
        )
    ) %>% names() 
     mutate(id = 'historical',
            # Millions to billions
            across(c(health_grants, medicaid_grants, investment_grants),
                   ~ .x / 1000))



national_accounts <- 
  
  usna %>% 
  mutate(id = 'historical') %>%
  millions_to_billions() %>%
  rename(cpiu = ui,
         
  ) %>% 
  # mutate(jgdp = gdp / gdph,
  #        jc = c  / ch,
  #        jgf = gf /gfh,
  #        jgs = gs/  gsh,
  #        jgse =  jgse / 100,
  #        jgsi = jgsi / 100) %>%
  mutate(across(starts_with('j'), ~ q_g(.x), .names = '{.col}_growth')) %>% 
  format_tsibble() %>% 
  #When adding new codes to read in from Haver, make sure to relocate them at the end of the spreadsheet using the below function:
  relocate(ylwsd:gftfbdx, .after = 'jgsi_growth') %>% 
  relocate(yptocm, .after = everything())

usethis::use_data(national_accounts, overwrite = TRUE)

devtools::load_all()
fim::national_accounts %>% 
  select(-id) %>% 
  pivot_longer(-date) %>% 
  as_tibble() %>% 
  pivot_wider(names_from = date,
              values_from = value) %>% 
  openxlsx::write.xlsx('data/haver_pivoted.xlsx', overwrite = TRUE)



# Create an auxilliary tibble with recession start and end dates. This will be used to create the charts with recession shading in the Fiscal Impact pdf posted online. 
recessions <-
  fim::national_accounts %>% 
  as_tibble() %>% 
  select(date, recession = recessq) %>% 
  mutate(
    date,
    diff = recession - dplyr::lag(recession),
    business_cycle = case_when(diff == 2 ~ 'recession_start',
                               diff == -2 ~ 'recession_end',
                               recession == 1 ~ 'recession',
                               recession == -1 ~ 'expansion'),
    .keep = 'used') %>% 
  filter(business_cycle == 'recession_start' | business_cycle == 'recession_end') %>% 
  pivot_longer(business_cycle) %>% 
  mutate(date2 = as_date(date)) %>%
  pivot_wider(names_from = value, 
              values_from = date) %>% 
  select(recession_start, recession_end) %>% 
  mutate(across(any_of(c('recession_start', 'recession_end')),
                .fns = ~ coalesce(.x, dplyr::lead(.x))),
         recession_end = dplyr::lead(recession_end),
         .keep = 'used') %>% 
  unique() %>% 
  drop_na()

usethis::use_data(recessions, overwrite = TRUE)


# Check values and then:
# gert::git_commit_all('Haver update')
# gert::git_push()