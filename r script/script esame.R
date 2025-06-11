library(tidyverse)
library(haven)
library(fixest)


# setting the working directory

setwd("/Users/riccardogaist/Desktop/multivariate/paper/dataverse_files (4)")

# Load the dataset

df = read_dta("Ghost_games_replication_data.dta")

# Check the first few rows of the dataset

head(df)

# use the Poisson Regression model to analyze what factors influence 
# the number of fouls in a soccer game
?feglm
model_fouls <- feglm(fouls ~ factor(home) + factor(post_covid) + factor(post_home) +
                       factor(var) + factor(home_var) + trav_lt20m + ELO_diff +
                       factor(season),
                     data = df, family = "poisson")


summary(model_fouls)
mean_fouls <- exp(coef(model_fouls)[1])

# 0. L'intercept ha senso??

# 0.  (Intercept) = 2.613 → è log(falli medi) nella condizione base.(tutto il 
# resto è 0) - # In una partita fuori casa, pre-Covid, senza VAR, con squadre
# di pari forza e nessuna variabile attiva… quanti falli ci si aspetta?

# exp(Coeff.) --> exp(2.6134) ≈ 13.6 (numero di falli fischiati in media in 
# assenza di tutte le altre condizioni) 

# i want to see the mean of fauls when all the factors are placed on 0
mean_fouls <- exp(coef(model_fouls)[1])

# 1. exp(β 1)=exp(−0.0288)≈0.9716 - Quindi, giocando in casa, ci si aspetta che 
# i falli fischiati siano circa il 97.16% di quelli fischiati in trasferta, 
# cioè circa il 2.84% in meno 


# 2. 