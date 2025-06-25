library(haven)
library(knitr)
library(tidyverse)
library(fixest)
library(modelsummary)
library(marginaleffects)
library(stargazer)
# setting the working directory
setwd("/Users/riccardogaist/Desktop/multivariate/paper/ghost_games")

# Load the dataset

df = read_dta("ghost_games.dta")


# --- tabella 1 --- (poi sistema) - creo crowd and side

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

# --- Table 2 setup --- 


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
custom_row_labels <- c(
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





game_map <- df %>%
  distinct(game_id, date, season, team, opponent) %>%
  arrange(game_id) %>% 
  mutate(
    home_team_name = as_factor(team),
    away_team_name = as_factor(opponent)
  )
    


df1 <- df %>%
  mutate(
    home_team_name = as_factor(team),
    away_team_name = as_factor(opponent)
  )


# tabella 3 ----
# Tabella 3 – OLS regressions: Determinants of Goal Difference




# ------------------------------
# Pulizia dei dati (facoltativa)
# ------------------------------
# Mantieni solo le righe complete per le variabili chiave
df_clean <- df %>%
  filter(
    !is.na(goal_diff),
    !is.na(post_covid),
    !is.na(ELO_diff),
    !is.na(distance),
    !is.na(var),
    !is.na(fouls_diff),
    !is.na(yellow_diff),
    !is.na(red_diff),
    !is.na(pk_diff)
  )

# -----------------------
# Regressione - Modello 1
# -----------------------
# Include: crowd-less indicator, ELO diff, distanza
model1 <- lm(goal_diff ~ post_covid + ELO_diff + distance, data = df_clean)

# -----------------------
# regression  - MOD 2
# -----------------------
# Adding difference in fouls, yellows, reds and penalties
model2 <- lm(goal_diff ~ post_covid + ELO_diff + distance +
               fouls_diff + yellow_diff + red_diff + pk_diff, data = df_clean)

# -----------------------
# regression - MOD 3
# -----------------------
# MOD 1 + VAR
model3 <- lm(goal_diff ~ post_covid + ELO_diff + distance + var, data = df_clean)

# -----------------------
# regression  - MOD 4
# -----------------------
# MOD 2 + VAR
model4 <- lm(goal_diff ~ post_covid + ELO_diff + distance + var +
               fouls_diff + yellow_diff + red_diff + pk_diff, data = df_clean)

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


# check the variables name in the df dataset

names(df)



goal_diff_model = lm(goal_diff ~ post_covid + ELO_diff + distance + fouls_diff +
                       yellow_diff + red_diff + pk_diff, data = df, subset = (home == 1))

summary(goal_diff_model)

names(df)

# Visualizzare coefficente nel modello di goal difference
# quali fattori influenzano maggioramente la differenza nei gol fatti

library(ggplot2)
library(broom)

# ottieni i coefficienti con intervalli di confidenza
coef_df <- broom::tidy(goal_diff_model, conf.int = TRUE)

# rimuovi l'intercetta per il grafico
coef_df <- coef_df[coef_df$term != "(Intercept)", ]

ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Estimated coefficients", y = "Variable", title = "Effects of variables on goal difference") +
  theme_minimal()







#prova subset

library(dplyr)

# Lista delle squadre Premier League nel dataset (controlla che siano corrette)


# Soglia di equilibrio su ELO_diff



# Controllo righe

df_subsample <- df %>%
  filter(season == 2019,
         abs(ELO_diff) <= 0.5)

goal_diff_model_subset <- lm(goal_diff ~ post_covid + ELO_diff + distance + fouls_diff +
                        yellow_diff + red_diff + pk_diff,
                      data = df_subsample,
                      subset = (home == 1))
summary(goal_diff_model)
summary(goal_diff_model_subset)

