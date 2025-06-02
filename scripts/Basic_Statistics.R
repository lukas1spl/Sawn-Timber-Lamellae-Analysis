#################################
#################################
#### Basic Statistical Tests ####
#################################
#################################

# 1. Normal Distribution and Homogeneity in Variances
#     a) Shapiro-Wilk and Levene test
#     b) saving table

# packages
library(tibble)
library(xtable)
library(dplyr)
library(car)


###########################################################
### 1. Normal Distribution and Homogeneity in Variances ###
###########################################################

lamellae <- readRDS("data/lamellae.rds")


#####################################
## a) Shapiro-Wilk and Levene test ##

# columns for Shapiro–Wilk and Levene tests
cols_dmmpp <- c("Density", "MOE", "MOR", "PrePD", "PostPD")

# performing tests and storing results in data frame
stat_props <- tibble(
  Properties   = cols_dmmpp,
  Shapiro_W   = sapply(cols_dmmpp, function(col) shapiro.test(lamellae[[col]])$statistic),
  Shapiro_p   = sapply(cols_dmmpp, function(col) shapiro.test(lamellae[[col]])$p.value),
  Levene_F    = sapply(cols_dmmpp, function(col) {
    test <- leveneTest(as.formula(paste(col, "~ Quality_Factor")), data = lamellae)
    test$`F value`[1]  # extract F-value from the test result
  }),
  Levene_p    = sapply(cols_dmmpp, function(col) {
    test <- leveneTest(as.formula(paste(col, "~ Quality_Factor")), data = lamellae)
    test$`Pr(>F)`[1]  # extract p-value from the test result
  })
)

# formatting values to scientific notation
stat_props_formatted <- stat_props %>%
  mutate(
    Shapiro_W = formatC(Shapiro_W, format = "f", digits = 3),
    Shapiro_p = formatC(Shapiro_p, format = "e", digits = 2),
    Levene_F  = formatC(Levene_F, format = "f", digits = 2),
    Levene_p  = formatC(Levene_p, format = "e", digits = 2)
  )


#####################
## b) Saving Table ##

# converting data frame to LaTeX format
stat_props_latex <- xtable(stat_props_formatted, caption = "Shapiro–Wilk and Levene Test Results")

# saving .tex file
sink("tables/stat_props.tex")
print(stat_props_latex, include.rownames = FALSE)
sink()


