# Preliminaries - getting set up

library(groundhog)
groundhog.library(
  c(
    "tidyverse",
    "data.table",
    "broom",
    "fixest",
    "cowplot"
  ),
  "2026-10-27"  #had to update date for groundhog
)

if(!interactive()) pdf(NULL) 

cces <- readRDS("replication-assignment/data/clean/cces.rds")


# Triple-difference regressions - running multiple feols models for black turnout

# Full sample (w/ fixed effects)
triple_1 <- feols(
  c(vote, registered) ~ black_full_shelby + black_partial_shelby |
    race_state + race_year + state_year,
  cluster = ~ state,
  weights = ~ weight_cumulative,
  data = cces
)

# Full sample (w/ controls and fixed effects)
triple_2 <- feols(
  c(vote, registered) ~ black_full_shelby + black_partial_shelby + gender + age + age_sq |
    race_state + race_year + state_year,
  cluster = ~ state,
  weights = ~ weight_cumulative,
  data = cces
)

# Full sample (w/ controls, fixed effects, and race-state time trends)
triple_3 <- feols(
  c(vote, registered) ~ black_full_shelby + black_partial_shelby + gender + age + age_sq |
    race_state + race_year + state_year + race_state[year],
  cluster = ~ state,
  weights = ~ weight_cumulative,
  data = cces
)

# Heterogeneity (w/ fixed effects)
triple_4 <- feols(
  c(vote, registered) ~ black_full_shelby_pres + black_full_shelby_mid + black_partial_shelby_pres + black_partial_shelby_mid |
    race_state + race_year + state_year,
  cluster = ~ state,
  weights = ~ weight_cumulative,
  data = cces 
)

# Heterogeneity (w/ controls and fixed effects)
triple_5 <- feols(
  c(vote, registered) ~ black_full_shelby_pres + black_full_shelby_mid + black_partial_shelby_pres + black_partial_shelby_mid + gender + age + age_sq |
    race_state + race_year + state_year,
  cluster = ~ state,
  weights = ~ weight_cumulative,
  data = cces
)

# Heterogeneity (w/ controls, fixed effects, and race-state time trends)
triple_6 <- feols(
  c(vote, registered) ~ black_full_shelby_pres + black_full_shelby_mid + black_partial_shelby_pres + black_partial_shelby_mid + gender + age + age_sq |
    race_state + race_year + state_year + race_state[year],
  cluster = ~ state,
  weights = ~ weight_cumulative,
  data = cces
)

# Tidy results - asked google how to fix the error I was getting (Error in `tidy()`:
#! No `tidy()` method for objects of class <fixest_multi>.
#Run `rlang::last_trace()` to see where the error occurred.), told me to add a [[1]] after passing it every model so it gets an individual model
#basically this creates the table in a data frame
triple_differences <- bind_rows(
  triple_1[lhs = "vote"][[1]] %>% 
    broom::tidy(conf.int = TRUE) %>% 
    mutate(controls = FALSE, race_state_trends = FALSE, lhs = "vote"),
  
  triple_2[lhs = "vote"][[1]] %>% 
    broom::tidy(conf.int = TRUE) %>% 
    mutate(controls = TRUE, race_state_trends = FALSE, lhs = "vote"),
  
  triple_3[lhs = "vote"][[1]] %>% 
    broom::tidy(conf.int = TRUE) %>% 
    mutate(controls = TRUE, race_state_trends = TRUE, lhs = "vote"),
  
  triple_4[lhs = "vote"][[1]] %>% 
    broom::tidy(conf.int = TRUE) %>% 
    mutate(controls = FALSE, race_state_trends = FALSE, lhs = "vote"),
  
  triple_5[lhs = "vote"][[1]] %>% 
    broom::tidy(conf.int = TRUE) %>% 
    mutate(controls = TRUE, race_state_trends = FALSE, lhs = "vote"),
  
  triple_6[lhs = "vote"][[1]] %>% 
    broom::tidy(conf.int = TRUE) %>% 
    mutate(controls = TRUE, race_state_trends = TRUE, lhs = "vote"),
  
  triple_1[lhs = "registered"][[1]] %>% 
    broom::tidy(conf.int = TRUE) %>% 
    mutate(controls = FALSE, race_state_trends = FALSE, lhs = "registered"),
  
  triple_2[lhs = "registered"][[1]] %>% 
    broom::tidy(conf.int = TRUE) %>% 
    mutate(controls = TRUE, race_state_trends = FALSE, lhs = "registered"),
  
  triple_3[lhs = "registered"][[1]] %>% 
    broom::tidy(conf.int = TRUE) %>% 
    mutate(controls = TRUE, race_state_trends = TRUE, lhs = "registered"),
  
  triple_4[lhs = "registered"][[1]] %>% 
    broom::tidy(conf.int = TRUE) %>% 
    mutate(controls = FALSE, race_state_trends = FALSE, lhs = "registered"),
  
  triple_5[lhs = "registered"][[1]] %>% 
    broom::tidy(conf.int = TRUE) %>% 
    mutate(controls = TRUE, race_state_trends = FALSE, lhs = "registered"),
  
  triple_6[lhs = "registered"][[1]] %>% 
    broom::tidy(conf.int = TRUE) %>% 
    mutate(controls = TRUE, race_state_trends = TRUE, lhs = "registered")
) %>% 
  filter(stringr::str_detect(term, "shelby"))

# Export results
saveRDS(triple_differences, "replication-assignment/data/clean/triple_differences.rds")


# Regression table - gives latex code for tables

# Effective observations
eff_obs <- cces %>% 
  filter(weight_cumulative > 0) %>% 
  select(race, state, year) %>% 
  distinct() %>% 
  nrow()

setFixest_dict(
  c(
    vote = "Turnout",
    registered = "Registration",
    black_full_shelby = "Black $\\times$ fully covered $\\times$ \\textit{Shelby}",
    black_partial_shelby = "Black $\\times$ partially covered $\\times$ \\textit{Shelby}",
    black_full_shelby_mid =  "Black $\\times$ fully covered $\\times$ \\textit{Shelby} $\\times$ midterm",
    black_full_shelby_pres = "Black $\\times$ fully covered $\\times$ \\textit{Shelby} $\\times$ presidential",
    black_partial_shelby_mid = "Black $\\times$ partially covered $\\times$ \\textit{Shelby} $\\times$ midterm",
    black_partial_shelby_pres = "Black $\\times$ partially covered $\\times$ \\textit{Shelby} $\\times$ presidential",
    race_state = "Race $\\times$ state",
    race_year = "Race $\\times$ year",
    state_year = "State $\\times$ year"
  )
)

etable(
  triple_1[lhs = "vote"],
  triple_2[lhs = "vote"],
  triple_3[lhs = "vote"],
  triple_4[lhs = "vote"],
  triple_5[lhs = "vote"],
  triple_6[lhs = "vote"],
  drop = c("age", "age_sq", "gender"),
  drop.section = "slopes",
  order = c("fully"),
  extraline = list(
    "-Demographic controls" = c(F, T, T, F, T, T),
    "-Race $\\times$ state time trends" = c(F, F, T, F, F, T),
    "_Effective observations (race $\\times$ state $\\times$ year)" = c(rep(eff_obs, times = 6))
  ), 
  digits = 3,
  digits.stats = 2,
  fitstat = ~ n,
  tex = T,
  replace = T,
  style.tex = style.tex("aer", fixef.title = "\\midrule"),
  file = "replication-assignment/outputs/table1.tex"
)

etable(
  triple_1[lhs = "registered"],
  triple_2[lhs = "registered"],
  triple_3[lhs = "registered"],
  triple_4[lhs = "registered"],
  triple_5[lhs = "registered"],
  triple_6[lhs = "registered"],
  drop = c("age", "age_sq", "gender"),
  drop.section = "slopes",
  order = c("fully"),
  extraline = list(
    "-Demographic controls" = c(F, T, T, F, T, T),
    "-Race $\\times$ state time trends" = c(F, F, T, F, F, T),
    "_Effective observations (race $\\times$ state $\\times$ year)" = c(rep(eff_obs, times = 6))
  ), 
  digits = 3,
  digits.stats = 2,
  fitstat = ~ n,
  tex = T,
  replace = T,
  style.tex = style.tex("aer", fixef.title = "\\midrule"),
  file = "replication-assignment/outputs/table2.tex"
)


