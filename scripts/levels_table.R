levels <- contributions %>% 
  select(date, federal_purchases, state_purchases, 
         consumption_grants, investment_grants, 
         federal_corporate_taxes, state_corporate_taxes, federal_non_corporate_taxes, state_non_corporate_taxes,
         federal_health_outlays, state_health_outlays,
         federal_subsidies, state_subsidies, 
         federal_aid_to_small_businesses_arp,
         federal_ui, state_ui,
         rebate_checks, rebate_checks_arp,
         federal_other_vulnerable_arp,
         federal_other_direct_aid_arp,
         federal_social_benefits_remainder = federal_social_benefits,
         state_social_benefits_remainder = state_social_benefits) %>% 
  filter_index("1999 Q4" ~ "2023 Q3") %>% 
  as_tibble() %>% 
  select(-id)


write.xlsx(levels, 'levels.xlsx', overwrite = TRUE)
