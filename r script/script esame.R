library(haven)
library(knitr)
library(tidyverse)
library(fixest)
library(modelsummary)
library(marginaleffects)
library(stargazer)
library(purrr)
library(broom)

# setting the working directory
setwd("/Users/riccardogaist/Desktop/multivariate/paper/ghost_games")

# Load the dataset

df = read_dta("ghost_games.dta")


# --- table 1 --- 

df_summary = df %>%
  mutate(
    crowd = ifelse(post_covid == 1, "Without Crowd", "With Crowd"),
    side = ifelse(home == 1, "Home", "Away")
  )

# group and commute the means

table1 = df_summary %>%
  group_by(crowd, side) %>%
  summarise(
    Fouls = mean(fouls, na.rm = TRUE),
    Yellows = mean(yellows, na.rm = TRUE),
    Reds = mean(reds, na.rm = TRUE),
    Penalties = mean(penalties, na.rm = TRUE),
    
    .groups = "drop"
    
  ) %>%
  arrange(crowd, side)



kable(table1, digits = 3, caption = "Table 1: Average fouls,
      yellow/red cards, and penalties with and without crowd")



# wanted to add the significance levels

get_stars = function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  return("")
}

compare_means = function(varname) {
  map_dfr(unique(df_summary$side), function(s) {
    data_sub = df_summary %>% filter(side == s)
    with_crowd = data_sub %>% filter(crowd == "With Crowd") %>% pull(!!sym(varname))
    without_crowd = data_sub %>% filter(crowd == "Without Crowd") %>% pull(!!sym(varname))
    
    if (length(with_crowd) > 1 & length(without_crowd) > 1) {
      p_val <- t.test(with_crowd, without_crowd)$p.value
    } else {
      p_val <- NA
    }
    
    tibble(
      side = s,
      !!varname := get_stars(p_val)
    )
  })
}


stars_fouls = compare_means("fouls")
stars_yellows = compare_means("yellows")
stars_reds = compare_means("reds")
stars_penalties = compare_means("penalties")




table1_with_stars = table1 %>%
  left_join(stars_fouls, by = "side") %>%
  left_join(stars_yellows, by = "side") %>%
  left_join(stars_reds, by = "side") %>%
  left_join(stars_penalties, by = "side") %>%
  mutate(
    Fouls = paste0(round(Fouls, 3), fouls),
    Yellows = paste0(round(Yellows, 3), yellows),
    Reds = paste0(round(Reds, 3), reds),
    Penalties = paste0(round(Penalties, 3), penalties)
  ) %>%
  select(crowd, side, Fouls, Yellows, Reds, Penalties)

kable(
  table1_with_stars,
  caption = "Table 1: Average fouls, yellow/red cards, and penalties with significance stars"
)









# Function to get significance stars (ghost games vs with crowd)
get_stars = function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  return("")
}

# Function to get gamma symbols (away vs home)
get_gamma = function(p) {
  if (p < 0.001) return("ᵞᵞᵞ")
  if (p < 0.01) return("ᵞᵞ")
  if (p < 0.05) return("ᵞ")
  return("")
}

# Compare with crowd vs without crowd within each side (home/away)
compare_means = function(varname) {
  map_dfr(unique(df_summary$side), function(s) {
    data_sub = df_summary %>% filter(side == s)
    with_crowd = data_sub %>% filter(crowd == "With Crowd") %>% pull(!!sym(varname))
    without_crowd = data_sub %>% filter(crowd == "Without Crowd") %>% pull(!!sym(varname))
    
    if (length(with_crowd) > 1 & length(without_crowd) > 1) {
      p_val <- t.test(with_crowd, without_crowd)$p.value
    } else {
      p_val <- NA
    }
    
    tibble(
      side = s,
      !!varname := get_stars(p_val)
    )
  })
}

# Compare home vs away within each crowd condition
compare_home_away = function(varname) {
  map_dfr(unique(df_summary$crowd), function(crowd_condition) {
    data_sub = df_summary %>% filter(crowd == crowd_condition)
    home = data_sub %>% filter(side == "Home") %>% pull(!!sym(varname))
    away = data_sub %>% filter(side == "Away") %>% pull(!!sym(varname))
    
    if (length(home) > 1 & length(away) > 1) {
      p_val <- t.test(home, away)$p.value
    } else {
      p_val <- NA
    }
    
    tibble(
      crowd = crowd_condition,
      !!varname := get_gamma(p_val)
    )
  })
}

# Apply functions to each variable of interest
stars_fouls = compare_means("fouls")
stars_yellows = compare_means("yellows")
stars_reds = compare_means("reds")
stars_penalties = compare_means("penalties")

gamma_fouls = compare_home_away("fouls")
gamma_yellows = compare_home_away("yellows")
gamma_reds = compare_home_away("reds")
gamma_penalties = compare_home_away("penalties")

# Merge all results into final table
table1_full = table1 %>%
  left_join(stars_fouls, by = "side") %>%
  left_join(stars_yellows, by = "side") %>%
  left_join(stars_reds, by = "side") %>%
  left_join(stars_penalties, by = "side") %>%
  left_join(gamma_fouls, by = "crowd") %>%
  left_join(gamma_yellows, by = "crowd") %>%
  left_join(gamma_reds, by = "crowd") %>%
  left_join(gamma_penalties, by = "crowd") %>%
  mutate(
    Fouls = paste0(round(Fouls, 3), fouls.x, fouls.y),
    Yellows = paste0(round(Yellows, 3), yellows.x, yellows.y),
    Reds = paste0(round(Reds, 3), reds.x, reds.y),
    Penalties = paste0(round(Penalties, 3), penalties.x, penalties.y)
  ) %>%
  select(crowd, side, Fouls, Yellows, Reds, Penalties)

# Display final table with significance stars and gamma symbols
kable(
  table1_full,
  caption = "Table 1: Average fouls, yellow/red cards, and penalties with significance stars and gamma symbols"
)







# --- Table 2 setup --- 


# Define the common formula structure
formula_main_preds = "home + post_covid + post_home + var + home_var + trav_lt20m + ELO_diff"
fixed_effects_formula = "team + opponent + season" 


# --- Fit the models --- 

# checking the command

?fepois
# Clustered standard errors by game_id

model_fouls = fepois(
  as.formula(paste("fouls ~", formula_main_preds, "|", fixed_effects_formula)),
  data = df,
  vcov = ~game_id 
)

model_yellows = fepois(
  as.formula(paste("yellows ~", formula_main_preds, "|", fixed_effects_formula)),
  data = df,
  vcov = ~game_id
)

model_reds = fepois( 
  as.formula(paste("reds ~", formula_main_preds, "|", fixed_effects_formula)),
  data = df,
  vcov = ~game_id
)

model_penalties = fepois(
  as.formula(paste("penalties ~", formula_main_preds, "|", fixed_effects_formula)),
  data = df,
  vcov = ~game_id
)

# --- Calculate Average Marginal Effects (AMEs) ---

# the equivalent of dydx (STATA) in `marginaleffects` for Poisson models 

mfx_variables = c("home", "post_covid", "post_home", "var",
                   "home_var", "trav_lt20m", "ELO_diff")

mfx_fouls = avg_slopes(model_fouls, variables = mfx_variables)
mfx_yellows = avg_slopes(model_yellows, variables = mfx_variables)
mfx_reds = avg_slopes(model_reds, variables = mfx_variables)
mfx_penalties = avg_slopes(model_penalties, variables = mfx_variables)


# --- Prepare for modelsummary ---
# Pass the marginaleffects objects to modelsummary
models_mfx = list(
  "Fouls" = mfx_fouls,
  "Yellow cards" = mfx_yellows,
  "Red cards" = mfx_reds,
  "Penalty kicks" = mfx_penalties
)

# Define custom row labels
custom_row_labels = c(
  "home" = "Home team",
  "post_covid" = "Crowd-less game",
  "post_home" = "Crowd-less game * Home",
  "var" = "VAR",
  "home_var" = "Home team * VAR",
  "trav_lt20m" = "Derby",
  "ELO_diff" = "ELO difference"
)



# Generate the table using modelsummary
msummary(
  models_mfx,
  fmt = "%.4f", # format estimates to 4 decimal
  stars = c('*' = .1, '**' = .05, '***' = .01), # define significance stars
  coef_map = custom_row_labels, # apply custom row labels
  add_rows = tibble::tribble(
    ~term, ~Fouls, ~`Yellow cards`, ~`Red cards`, ~`Penalty kicks`,
    "Observations", as.character(nobs(model_fouls)), as.character(nobs(model_yellows)), as.character(nobs(model_reds)), as.character(nobs(model_penalties)),
    "Team, opponent fixed effects", "Yes", "Yes", "Yes", "Yes",
    "Season fixed effects", "Yes", "Yes", "Yes", "Yes"
  ),
  
  title = "Table 2. Poisson Regression Results: Determinants of Referee Decisions.",
  notes = list(
    "Standard errors are calculated allowing a correlation between observations from the same game." ,
    "Significance levels: *p < .10, **p < .05, ***p < .01."
  ),
  
  
  
  output = "markdown"
)








# Table 3 – OLS regressions: Determinants of Goal Difference


# -----------------------
# Regression - Model 1
# -----------------------

model1 <- lm(goal_diff ~ post_covid + ELO_diff + distance, data = df %>% filter(home == 1))



# -----------------------
# Regression  - MOD 2
# -----------------------

# Adding difference in fouls, yellows, reds and penalties

model2 <- lm(goal_diff ~ post_covid + ELO_diff + distance +
               fouls_diff + yellow_diff + red_diff + pk_diff, data = df %>% filter(home == 1))


# -----------------------
# Regression - MOD 3 -  MOD 1 + VAR
# -----------------------

model3 <- lm(goal_diff ~ post_covid + ELO_diff + distance + var, data = df %>% filter(home == 1))


# -----------------------
# Regression  - MOD 4 -  MOD 2 + VAR
# -----------------------

model4 <- lm(goal_diff ~ post_covid + ELO_diff + distance + var +
               fouls_diff + yellow_diff + red_diff + pk_diff, data = df %>% filter(home == 1))


# ------------------------------
# Quick check models
# ------------------------------
summary(model1)
summary(model2)
summary(model3)
summary(model4)

# ------------------------------
# Comparative Table, Table 3 Replication
# ------------------------------

stargazer(model1, model2, model3, model4,
          type = "text",
          title = "Tabella 3 - Regressioni OLS: Determinanti del Goal Difference",
          dep.var.labels = "Goal Difference (Home - Away)",
          covariate.labels = c("Crowd-less game", "ELO difference", "Distance between teams", 
                               "VAR", "Foul difference", "Yellow card difference",
                               "Red card difference", "Penalty kick difference"),
          omit.stat = c("f", "ser"),
          digits = 3,
          no.space = TRUE)





goal_diff_model = lm(goal_diff ~ post_covid + ELO_diff + distance + fouls_diff +
                       yellow_diff + red_diff + pk_diff, data = df, subset = (home == 1))

stargazer::stargazer(goal_diff_model, type = "text")

# Which factors have the strongest impact on goal difference?


# coefficents and confidence intervals

coef_df = broom::tidy(goal_diff_model, conf.int = TRUE)

coef_df_sub = broom::tidy(goal_diff_model_subset, conf.int = TRUE)

#remove intercept for the graph part 

coef_df = coef_df[coef_df$term != "(Intercept)", ]

coef_df_sub = coef_df_sub[coef_df_sub$term != "(Intercept)", ]

#Tidy coefficients with confidence intervals

coef_df = tidy(goal_diff_model, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(Model = "Full Sample")

coef_df_sub = tidy(goal_diff_model_subset, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(Model = "Subset")

# Combine the two datasets
coef_combined = bind_rows(coef_df, coef_df_sub)

# plotting part
ggplot(coef_combined, aes(x = estimate, y = reorder(term, estimate), color = Model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0.2, position = position_dodge(width = 0.5), linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Estimated Coefficients", y = "Variable", 
       title = "Comparison of Coef Estimates Full Sample vs Subset") +
  theme_minimal(base_size = 12)




# Regression for the subsample: Subsample using data for teams with an 
#ELO diff of at most 1

df_subsample = df %>%
  filter(season == 2015 & 2016 & 2017 & 2018 & 2019,
         abs(ELO_diff) <= 1)


goal_diff_model_subset = lm(goal_diff ~ post_covid + ELO_diff + distance + fouls_diff +
                        yellow_diff + red_diff + pk_diff,
                      data = df_subsample,
                      subset = (home == 1))

# comparing the two models

stargazer(goal_diff_model, goal_diff_model_subset, type = "text",
          title = "Comparison of Goal Difference Models",
          column.labels = c("Full Sample", "Subset"),
          dep.var.labels = "Goal Difference")



