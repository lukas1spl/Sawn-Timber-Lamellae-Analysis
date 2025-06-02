#####################################################
#####################################################
#### Influence of Density on Lamellae Properties ####
#####################################################
#####################################################

# 1. Correlations between Lamellae Properties
#     a) Correlation Coefficient Matrix
#     b) Visualizing Correlation Matrix
# 2. Spearman Correlation Coefficients
#     a) Filtering Data by Quality Class
#     b) Calculating Spearman Correlations
#     c) Converting Results into a Data Frame
#     d) Saving Table
# 3. Linear Regression
#     a) Linear Regression for Density vs. Lamellae Properties
#     b) Visualizing Linear Regression

# Packages
library(ggcorrplot)
library(ggplot2)
library(dplyr)
library(xtable)
library(gridExtra)



###################################################
### 1. Correlations between Lamellae Properties ###
###################################################

lamellae <- readRDS("data/lamellae.rds")

#######################################
## a) Correlation Coefficient Matrix ##

# columns for the correlation matrix
props <- c("MOE", "MOR", "PrePD", "PostPD", "Density")

# calculating the correlation matrix
cor_vals <- cor(lamellae[, props], method = "spearman", use = "complete.obs")
cor_vals <- cor_vals[props, props]  # Ensure rows and columns are in the desired order


#######################################
## b) Visualizing Correlation Matrix ##

cor_matrix <- ggcorrplot(
  cor_vals,
  method = "square",
  type = "full",         # Show the full matrix
  show.diag = FALSE,     # Leave the diagonal blank
  colors = c("#6D9EC1", "white", "#E46726"),
  outline.color = "white",
  ggtheme = ggplot2::theme_dark(),
  lab = TRUE) +
  scale_y_discrete(limits = rev(props)) +
  scale_x_discrete(limits = props) +
  theme(
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black")
  )

print(cor_matrix)

# saving correlation matrix
ggsave("plots/cor_matrix.png", plot = cor_matrix,
       width = 6.75, height = 5.25, units = "in", dpi = 1000)



############################################
### 2. Spearman Correlation Coefficients ###
############################################

lamellae <- readRDS("data/lamellae.rds")


########################################
## a) Filtering Data by Quality Class ##

lamellae1 <- lamellae %>% filter(Quality == 1)

lamellae2 <- lamellae %>% filter(Quality == 2)

lamellae3 <- lamellae %>% filter(Quality == 3)


#######################################################
## b) Calculating Spearmann Correlation Coefficients ##

corr_density <- rbind(
  lamellae = c(
    MOE    = cor(lamellae$Density, lamellae$MOE, method = "spearman", use = "complete.obs"),
    MOR    = cor(lamellae$Density, lamellae$MOR, method = "spearman", use = "complete.obs"),
    PrePD  = cor(lamellae$Density, lamellae$PrePD, method = "spearman", use = "complete.obs"),
    PostPD = cor(lamellae$Density, lamellae$PostPD, method = "spearman", use = "complete.obs")
  ),
  lamellae1 = c(
    MOE    = cor(lamellae1$Density, lamellae1$MOE, method = "spearman", use = "complete.obs"),
    MOR    = cor(lamellae1$Density, lamellae1$MOR, method = "spearman", use = "complete.obs"),
    PrePD  = cor(lamellae1$Density, lamellae1$PrePD, method = "spearman", use = "complete.obs"),
    PostPD = cor(lamellae1$Density, lamellae1$PostPD, method = "spearman", use = "complete.obs")
  ),
  lamellae2 = c(
    MOE    = cor(lamellae2$Density, lamellae2$MOE, method = "spearman", use = "complete.obs"),
    MOR    = cor(lamellae2$Density, lamellae2$MOR, method = "spearman", use = "complete.obs"),
    PrePD  = cor(lamellae2$Density, lamellae2$PrePD, method = "spearman", use = "complete.obs"),
    PostPD = cor(lamellae2$Density, lamellae2$PostPD, method = "spearman", use = "complete.obs")
  ),
  lamellae3 = c(
    MOE    = cor(lamellae3$Density, lamellae3$MOE, method = "spearman", use = "complete.obs"),
    MOR    = cor(lamellae3$Density, lamellae3$MOR, method = "spearman", use = "complete.obs"),
    PrePD  = cor(lamellae3$Density, lamellae3$PrePD, method = "spearman", use = "complete.obs"),
    PostPD = cor(lamellae3$Density, lamellae3$PostPD, method = "spearman", use = "complete.obs")
  )
)

#############################################
## c) Converting Results into a Data Frame ##

corr_density <- as.data.frame(corr_density)

# round values to two digits
corr_density <- round(corr_density,2)

# adding row names
rownames(corr_density) <- c("lamellae", "lamellae1", "lamellae2", "lamellae3")


#####################
## d) Saving Table ##

# converting data frame to LaTeX format
xt_corr_density <- xtable(corr_density, caption = "")

# saving .tex file
sink("tables/corr_density.tex")
print(xt_corr_density, include.rownames = TRUE, caption.placement = "top")
sink()



############################
### 3. Linear Regression ###
############################

lamellae <- readRDS("data/lamellae.rds")


##############################################################
## a) Linear Regression for Density vs. Lamellae Properties ##

# data frame for linear regression results
lm_results <- data.frame(
  Quality = character(),
  Response = character(),
  Intercept = numeric(),
  Intercept_SE = numeric(),
  Slope = numeric(),
  Slope_SE = numeric(),
  t_value = numeric(),
  p_value = numeric(),
  R_squared = numeric(),
  Residual_SE = numeric(),
  DF_residual = numeric(),
  F_statistic = numeric(),
  stringsAsFactors = FALSE
)

# performing linear regression by looping over the quality classes
for (q in c(1, 2, 3)) {
  # sub-setting data for the current quality class
  data_q <- lamellae[lamellae$Quality == q, ]
  
  ## MOE ~ Density
  lm_moe <- lm(MOE ~ Density, data = data_q)
  s_moe <- summary(lm_moe)
  # storing parameters + rounding values
  row_moe <- data.frame(
    Quality = as.character(q),
    Response = "MOE",
    Intercept = round(coef(lm_moe)[1], 2),
    Intercept_SE = round(s_moe$coefficients[1, 2], 2),
    Slope = round(coef(lm_moe)[2], 3),
    Slope_SE = round(s_moe$coefficients[2, 2], 2),
    t_value = round(s_moe$coefficients[2, 3], 2),
    p_value = signif(s_moe$coefficients[2, 4], 2),
    R_squared = round(s_moe$r.squared, 2),
    Residual_SE = round(s_moe$sigma, 2),
    DF_residual = s_moe$df[2],
    F_statistic = round(s_moe$fstatistic[1], 1),
    stringsAsFactors = FALSE
  )
  lm_results <- rbind(lm_results, row_moe)
  
  ## MOR ~ Density
  lm_mor <- lm(MOR ~ Density, data = data_q)
  s_mor <- summary(lm_mor)
  # storing parameters + rounding values
  row_mor <- data.frame(
    Quality = as.character(q),
    Response = "MOR",
    Intercept = round(coef(lm_mor)[1], 2),
    Intercept_SE = round(s_mor$coefficients[1, 2], 2),
    Slope = round(coef(lm_mor)[2], 3),
    Slope_SE = round(s_mor$coefficients[2, 2], 2),
    t_value = round(s_mor$coefficients[2, 3], 2),
    p_value = signif(s_mor$coefficients[2, 4], 2),
    R_squared = round(s_mor$r.squared, 2),
    Residual_SE = round(s_mor$sigma, 2),
    DF_residual = s_mor$df[2],
    F_statistic = round(s_mor$fstatistic[1], 1),
    stringsAsFactors = FALSE
  )
  lm_results <- rbind(lm_results, row_mor)
}


######################################
## b) Visualizing Linear Regression ##

# color scheme for quality classes
colors_qual <- c("1" = "royalblue4", 
                 "2" = "steelblue3",
                 "3" = "turquoise3")

# names for quality classes
quality_names <- c("1" = "Standard Quality (N)",
                   "2" = "Industrial Quality (I)",
                   "3" = "Substandard Quality (A)")

name_dens <- c("Density" = "Density [kg/m³]")

names_strength <- c("MOE" = "MOE [GPa]",
                    "MOR" = "MOR [MPa]")

## Compute global axis limits for Density and responses (MOE and MOR) across all subsets
global_x_min <- min(c(lamellae1$Density, lamellae2$Density, lamellae3$Density), na.rm = TRUE)
global_x_max <- max(c(lamellae1$Density, lamellae2$Density, lamellae3$Density), na.rm = TRUE)

global_moe_y_min <- min(c(lamellae1$MOE, lamellae2$MOE, lamellae3$MOE), na.rm = TRUE)
global_moe_y_max <- max(c(lamellae1$MOE, lamellae2$MOE, lamellae3$MOE), na.rm = TRUE)

global_mor_y_min <- min(c(lamellae1$MOR, lamellae2$MOR, lamellae3$MOR), na.rm = TRUE)
global_mor_y_max <- max(c(lamellae1$MOR, lamellae2$MOR, lamellae3$MOR), na.rm = TRUE)

# Pre-calculate static annotation positions based on global limits
x_pos_global <- global_x_min + 0.025 * (global_x_max - global_x_min)
y_pos_moe <- global_moe_y_max - 0.05 * (global_moe_y_max - global_moe_y_min)
y_pos_mor <- global_mor_y_max - 0.05 * (global_mor_y_max - global_mor_y_min)

# Helper function to create scatterplots with consistent axis limits and manual breaks
create_scatterplot <- function(data, response, quality_label, point_shape, include_title = TRUE) {
  # extract regression results
  model_params <- lm_results[lm_results$Quality == as.character(quality_label) &
                               lm_results$Response == response, ]
  annot_text <- paste0(
    "Slope = ", model_params$Slope, "\n",
    "p = ", model_params$p_value, "\n",
    "R² = ", model_params$R_squared, "\n",
    "RSE = ", model_params$Residual_SE, "\n",
    "F = ", model_params$F_statistic
  )
  # choose static y position
  y_label <- if (response == "MOE") y_pos_moe else y_pos_mor
  
  p <- ggplot(data, aes(x = Density, y = .data[[response]])) +
    geom_point(color = colors_qual[as.character(quality_label)], shape = point_shape) +
    geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
    labs(x = name_dens, y = names_strength[response]) +
    annotate(
      "label", x = x_pos_global, y = y_label, label = annot_text,
      color = "darkorange", fill = "white", alpha = 0.75, hjust = 0, vjust = 1, size = 5
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 14, color = "gray25"),
      axis.title = element_text(size = 16, face = "bold", color = "black"),
      title = element_text(size = 18, face = "bold", color = "black")
    )
  # force title space: actual text or blank placeholder
  if (include_title) {
    p <- p + ggtitle(quality_names[as.character(quality_label)])
  } else {
    p <- p + ggtitle(" ")  # blank title to reserve space
  }
  
  # fixed breaks and limits
  p <- p + scale_x_continuous(
    breaks = c(350, 400, 450, 500, 550),
    limits = c(global_x_min, global_x_max)
  )
  if (response == "MOE") {
    p <- p + scale_y_continuous(
      breaks = c(2, 4, 6, 8, 10, 12),
      limits = c(global_moe_y_min, global_moe_y_max)
    )
  } else {
    p <- p + scale_y_continuous(
      breaks = c(20, 40, 60, 80, 100),
      limits = c(global_mor_y_min, global_mor_y_max)
    )
  }
  return(p)
}

# assembling plots
plot_list <- list(lamellae1_MOE = create_scatterplot(lamellae1, "MOE", 1, 9),
                  lamellae1_MOR = create_scatterplot(lamellae1, "MOR", 1, 8, include_title = FALSE),
                  lamellae2_MOE = create_scatterplot(lamellae2, "MOE", 2, 9),
                  lamellae2_MOR = create_scatterplot(lamellae2, "MOR", 2, 8, include_title = FALSE),
                  lamellae3_MOE = create_scatterplot(lamellae3, "MOE", 3, 9),
                  lamellae3_MOR = create_scatterplot(lamellae3, "MOR", 3, 8, include_title = FALSE)
                  )

# arragen plots in a grid
density_influence <- grid.arrange(plot_list$lamellae1_MOE, plot_list$lamellae1_MOR,
                                  plot_list$lamellae2_MOE, plot_list$lamellae2_MOR,
                                  plot_list$lamellae3_MOE, plot_list$lamellae3_MOR,
                                  ncol = 2
                                  )

# saving plot
ggsave("plots/density_influence.png", plot = density_influence,
       width = 14.5, height = 17.5, units = "in", dpi = 700)

