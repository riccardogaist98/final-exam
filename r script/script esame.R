library(haven)
library(knitr)
library(tidyverse)
library(fixest)
library(modelsummary)
library(marginaleffects)
# setting the working directory
setwd("/Users/riccardogaist/Desktop/multivariate/paper/ghost_games")

# Load the dataset

df = read_dta("ghost_games.dta")


# tabella 1 (poi sistema) - creo crowd and side

df_summary <- df %>%
  mutate(
    crowd = ifelse(post_covid == 1, "Without Crowd", "With Crowd"),
    side = ifelse(home == 1, "Home", "Away")
  )

# group and commute the means

table1 <- df_summary %>%
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
















# Start table 2 (poi sistema)


















# Define the common formula structure
formula_main_preds = "home + post_covid + post_home + var + home_var + trav_lt20m + ELO_diff"
fixed_effects_formula = "team + opponent + season" 


# --- Fit the models --- 

?fepois
# Clustered standard errors by game_id

model_fouls <- fepois(
  as.formula(paste("fouls ~", formula_main_preds, "|", fixed_effects_formula)),
  data = df,
  vcov = ~game_id 
)

model_yellows <- fepois(
  as.formula(paste("yellows ~", formula_main_preds, "|", fixed_effects_formula)),
  data = df,
  vcov = ~game_id
)

model_reds <- fepois( 
  as.formula(paste("reds ~", formula_main_preds, "|", fixed_effects_formula)),
  data = df,
  vcov = ~game_id
)

model_penalties <- fepois(
  as.formula(paste("penalties ~", formula_main_preds, "|", fixed_effects_formula)),
  data = df,
  vcov = ~game_id
)

# --- Calculate Average Marginal Effects (AMEs) ---

# the equivalent of dydx (STATA) in `marginaleffects` for Poisson models 

mfx_variables <- c("home", "post_covid", "post_home", "var",
                   "home_var", "trav_lt20m", "ELO_diff")

mfx_fouls <- avg_slopes(model_fouls, variables = mfx_variables)
mfx_yellows <- avg_slopes(model_yellows, variables = mfx_variables)
mfx_reds <- avg_slopes(model_reds, variables = mfx_variables)
mfx_penalties <- avg_slopes(model_penalties, variables = mfx_variables)


# --- Prepare for modelsummary ---
# Pass the marginaleffects objects to modelsummary
models_mfx <- list(
  "Fouls" = mfx_fouls,
  "Yellow cards" = mfx_yellows,
  "Red cards" = mfx_reds,
  "Penalty kicks" = mfx_penalties
)

# Define custom row labels


# Generate the table using modelsummary
msummary(
  models_mfx,
  fmt = "%.4f", # Format estimates to 4 decimal places
  stars = c('*' = .1, '**' = .05, '***' = .01), # Define significance stars
  coef_map = custom_row_labels, # Apply custom row labels
  
  # To add 'Observations' and fixed effects, we create custom rows.
  # Get nobs from the original fepois objects.
  add_rows = tibble::tribble(
    ~term, ~Fouls, ~`Yellow cards`, ~`Red cards`, ~`Penalty kicks`,
    "Observations", as.character(nobs(model_fouls)), as.character(nobs(model_yellows)), as.character(nobs(model_reds)), as.character(nobs(model_penalties)),
    "Team, opponent fixed effects", "Yes", "Yes", "Yes", "Yes",
    "Season fixed effects", "Yes", "Yes", "Yes", "Yes"
  ),
  output = "markdown" # Or "latex_tabular"
)


# GRAZIE MILLE DIO . DIO MIO GRAZIEEEEEEEEEEEEEEEEEE
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








