#####################################################
#####################################################
#### Influence of Knots on Mechanical Properties ####
#####################################################
#####################################################

# 1. Linear Regression - Knot Size vs. Strength Properties
#     a) Spearman Correlations
#     b) Regression Models
#     c) Visualization of Regression Models
# 2. Kruskal-Wallis Test and Dunn's Test
#     a) Knot-Size Classes
#     b) Kruskal-Wallis Test
#     c) Visualizing Kruskal-Wallis Results
#     d) Dunn's Test
# 3. Density Influence with Grouped Knot Sizes
#     a) Regression Models
#     b) Visualization of Regression Models

# Packages
library(dplyr)
library(FSA)
library(reshape2)
library(patchwork)
library(ggplot2)
library(gridExtra)
library(stringr)


#############################################################
### 1.Linear Regression - Knot Size ~ Strength Properties ###
#############################################################

lamellae <- readRDS("data/lamellae.rds")

# create individual data frames for the Quality classes with corresponding knot size
lamellae1 <- lamellae %>% filter(Quality == 1, max_knot <= 17)

lamellae2 <- lamellae %>% filter(Quality == 2, max_knot >=17, max_knot <= 34, knot_decisive == 1)

lamellae3 <- lamellae %>% filter(Quality == 3, max_knot >= 34, knot_decisive == 1)

#recombining the filtered data frames
lamellae_knots <- bind_rows(lamellae1, lamellae2, lamellae3)


##############################
## a) Spearman Correlations ##

props <- c("Density",
           "MOE",
           "MOR",
           "PrePD",
           "PostPD")

# Spearman Correlation Coefficients between the biggest knot and the lamellae properties
corr_knots <- cor(lamellae_knots$max_knot, lamellae_knots[, props], method = "spearman", use = "complete.obs")


##########################
## b) Regression Models ##

# Data frame to store regression results
lm_knots <- data.frame(Quality = character(),
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
                       stringsAsFactors = FALSE)

## MOE
lm_moe <- lm(MOE ~ max_knot, data = lamellae_knots)

s_moe <- summary(lm_moe)

row_moe <- data.frame(Response = "MOE",
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
                      stringsAsFactors = FALSE)

lm_knots <- rbind(lm_knots, row_moe)

## Regression for MOR ~ max_knot on the entire data frame
lm_mor <- lm(MOR ~ max_knot, data = lamellae_knots)

s_mor <- summary(lm_mor)

row_mor <- data.frame(Response = "MOR",
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
                      stringsAsFactors = FALSE)

lm_knots <- rbind(lm_knots, row_mor)


###########################################
## c) Visualization of Regression Models ##

# Helper function to create a scatterplot for a given response
scatterplot_lm_knots <- function(data, response, line_color, lab_y, point_shape) {
  
  # Fit the linear model: response ~ max_knot
  model <- lm(as.formula(paste(response, "~ max_knot")), data = data)
  s_model <- summary(model)
  
  # Format the regression annotation text: slope, p-value, R², Residual SE and F-statistic
  annot_text <- paste0("Slope = ", round(coef(model)[2], 3), "\n",
                       "p = ", signif(s_model$coefficients[2, 4], 2), "\n",
                       "R² = ", round(s_model$r.squared, 2), "\n",
                       "RSE = ", round(s_model$sigma, 2), "\n",
                       "F = ", round(s_model$fstatistic[1], 1))
  
  # Determine positions for annotations based on the data range
  x_min <- max(data$max_knot, na.rm = TRUE)
  x_range <- diff(range(data$max_knot, na.rm = TRUE))
  y_max <- max(data[[response]], na.rm = TRUE)
  y_range <- diff(range(data[[response]], na.rm = TRUE))
  
  # x_pos is calculated so that the annotation is slightly to the left of the maximum value
  x_pos <- x_min - 0.05 * x_range
  
  # y_pos for regression annotation: near the top of the plot 
  y_pos <- y_max - 0.05 * y_range
  
  # Compute Spearman correlation between max_knot and response
  corr_coeff <- cor(data$max_knot, data[[response]], method = "spearman", use = "complete.obs")
  corr_expr <- paste0("r[s]==", round(corr_coeff, 2))
  
  # Place the correlation annotation above the regression annotation:
  y_corr_pos <- y_pos + 0.05 * y_range
  
  # Build the plot with both annotations added
  p <- ggplot(data, aes(x = max_knot, y = .data[[response]])) +
    geom_point(color = line_color, shape = point_shape) +
    geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
    # Correlation annotation (above the regression annotation)
    annotate("label", x = x_pos, y = y_corr_pos, label = corr_expr,
             color = "navy", hjust = 1, vjust = 1, size = 5, fill = "white", alpha = 0.75, parse = TRUE) +
    # Regression annotation (below the correlation annotation)
    annotate("label", x = x_pos, y = y_pos, label = annot_text,
             color = "darkorange", hjust = 1, vjust = 1, size = 5, fill = "white", alpha = 0.75) +
    labs(x = "Maximum Knot Size [% of Lamella Width]",
         y = lab_y) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 16))
  
  return(p)
}

# Create scatterplots for MOE and MOR
p_MOE <- scatterplot_lm_knots(lamellae_knots, "MOE", line_color = "slategray3", lab_y = "MOE [GPa]", 9)
p_MOR <- scatterplot_lm_knots(lamellae_knots, "MOR", line_color = "slategray3", lab_y = "MOR [MPa]", 8)

# Arrange the two plots side by side
knot_lm <- grid.arrange(p_MOE, p_MOR, ncol = 2)

ggsave("plots/knot_lm.png", plot = knot_lm,
       width = 15, height = 8.5, units = "in", dpi = 700)



###########################################
# 2. Kruskal-Wallis Test and Dunn's Test  #
###########################################

lamellae <- readRDS("data/lamellae.rds")

# create individual data frames for the Quality classes with corresponding knot size
lamellae1 <- lamellae %>% filter(Quality == 1, max_knot <= 17)

lamellae2 <- lamellae %>% filter(Quality == 2, max_knot >=17, max_knot <= 34, knot_decisive == 1)

lamellae3 <- lamellae %>% filter(Quality == 3, max_knot >= 34, knot_decisive == 1)

#recombining the filtered data frames
lamellae_knots <- bind_rows(lamellae1, lamellae2, lamellae3)


##########################
## a) Knot-Size Classes ##

lamellae_knots$knot_class <- cut(lamellae_knots$max_knot,
                             breaks = c(0, 17, 34, 100),
                             include.lowest = TRUE,
                             right = TRUE,
                             labels = c("1", "2", "3")
                             )

lamellae_knots$knot_class2 <- cut(lamellae_knots$max_knot, 
                              breaks = c(0, 8.5, 17, 25.5, 34, 67, 100),
                              include.lowest = TRUE,
                              right = TRUE,
                              labels = c("1", "2", "3", "4", "5", "6")
                              )

strength_props <- c("MOE",
                    "MOR")

############################
## b) Kruskal-Wallis Test ##

# Defining function for Kruskal-Wallis Test
perform_kruskal_tests <- function(data, properties, group_col) {
  results <- data.frame(
    Kruskal_chisq = sapply(properties, function(col) {
      # Extract the un-named chi-squared statistic
      unname(kruskal.test(as.formula(paste(col, "~", group_col)), data = data)$statistic)
    }),
    Kruskal_p = sapply(properties, function(col) {
      kruskal.test(as.formula(paste(col, "~", group_col)), data = data)$p.value
    }),
    row.names = properties
  )
  return(results)
}

# Running the function and storing the results
# Normal knot-size classes
kruskal_knots <- perform_kruskal_tests(lamellae_knots, strength_props, "knot_class")

# Splitted knot-size classes
kruskal_knots2 <- perform_kruskal_tests(lamellae_knots, strength_props, "knot_class2")


###########################################
## c) Visualizing Kruskal-Wallis Results ##

## Defining colors and names for knot-size classes
# Normal knot-size classes
colors_knotcat <- c("1" = "darkgreen",
                    "2" = "forestgreen",
                    "3" = "palegreen3")

names_knotcat <- c("1" = "Small",
                   "2" = "Medium",
                   "3" = "Large")

# Splitted knot-size classes
colors_knotcat2 <- c("1" = "darkgreen", 
                      "2" = "darkgreen",
                      "3" = "forestgreen",
                      "4" = "forestgreen",
                      "5" = "palegreen3",
                      "6" = "palegreen3")

names_knotcat2 <- c("1" = "Small-A",
                     "2" = "Small-B",
                     "3" = "Medium-A",
                     "4" = "Medium-B",
                     "5" = "Large-A",
                     "6" = "Large-B")

# MOE and MOR with units
names_strength_props <- c("MOE" = "MOE [GPa]",
                          "MOR" = "MOR [MPa]")

## Grouping data by knot-size classes
# Normal knot-size classes
lamellae_knots_grouped <- melt(lamellae_knots, id.vars = "knot_class", measure.vars = strength_props)

# Splitted knot-size classes
lamellae_knots_grouped2 <- melt(lamellae_knots, id.vars = "knot_class2", measure.vars = strength_props)

## Calculating means for knot-size classes
# Normal knot-size classes
means_knotcat <- lamellae_knots_grouped %>%
  group_by(knot_class, variable) %>%
  summarise(mean_value = round(mean(value, na.rm = TRUE), 3), .groups = "drop")

# Splitted knot-size classes
means_knotcat2 <- lamellae_knots_grouped2 %>%
  group_by(knot_class2, variable) %>%
  summarise(mean_value = round(mean(value, na.rm = TRUE), 3), .groups = "drop")

# Combined boxplot function
create_boxplot_knots <- function(df, means_df, kruskal_df,
                                 group_var, fill_colors, x_labels,
                                 names_props, var) {
  # Subset data for the current variable
  df_subset <- df %>% filter(variable == var)
  means_subset <- means_df %>% filter(variable == var)
  
  # Extract Kruskal–Wallis values for the current variable
  kruskal_p_value <- kruskal_df[var, "Kruskal_p"]
  kruskal_chi_value <- kruskal_df[var, "Kruskal_chisq"]
  
  # Determine annotation positions based on the data's y-range
  x_pos <- 0.5   # fixed left edge for annotations
  y_min <- min(df_subset$value, na.rm = TRUE)
  # Multiply y_max by 1.2 so that the annotation has extra room at the top
  y_max <- max(df_subset$value, na.rm = TRUE) * 1.2
  y_pos_chi <- y_max - 0.075 * (y_max - y_min)
  y_pos_p <- y_pos_chi - 0.075 * (y_max - y_min)
  
  # Build the plot using aes_string() so that the grouping variable is flexible
  p <- ggplot(df_subset, aes_string(x = group_var, y = "value", fill = group_var)) +
    geom_boxplot() +
    scale_fill_manual(values = fill_colors) +
    scale_x_discrete(labels = x_labels) +
    labs(x = "Knot Size Group", 
         y = names_props[var]) +
    theme_minimal(base_family = "Arial") +
    theme(legend.position = "none",
          axis.text = element_text(size = 16),
          axis.title.y = element_text(size = 18, face = "bold"),
          axis.title.x = element_text(size = 16, face = "bold"),
          panel.background = element_rect(fill = "gray98", color = NA)) +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 4, color = "white") +
    geom_text(data = means_subset,
              aes_string(x = group_var, y = "mean_value", 
                         label = "round(mean_value, 1)"),
              vjust = -0.75, hjust = 0.5, size = 6.5, color = "white") +
    # Add Kruskal–Wallis annotations
    annotate("text", x = x_pos, y = y_pos_chi, hjust = 0,
             label = "Kruskal-Wallis Test:",
             size = 6, fontface = "bold", color = "darkorange") +
    annotate("text", x = x_pos, y = y_pos_p, hjust = 0,
             label = bquote(chi^2 ~ "=" ~ .(round(kruskal_chi_value, 1)) ~ "        p = " ~ .(signif(kruskal_p_value, 2))),
             size = 6, fontface = "bold", color = "darkorange")
  
  return(p)
}

# create box plots for the normal knot size groups
boxplots_knots <- lapply(strength_props, function(var) {
  create_boxplot_knots(
    df         = lamellae_knots_grouped,
    means_df   = means_knotcat,
    kruskal_df = kruskal_knots,
    group_var  = "knot_class",
    fill_colors = colors_knotcat,
    x_labels   = names_knotcat,        
    names_props = names_strength_props,
    var        = var
  )
})

# create box plots for the split knot size groups
boxplots_knots2 <- lapply(strength_props, function(var) {
  create_boxplot_knots(
    df         = lamellae_knots_grouped2,
    means_df   = means_knotcat2,
    kruskal_df = kruskal_knots2,
    group_var  = "knot_class2",
    fill_colors = colors_knotcat2,
    x_labels   = names_knotcat2,
    names_props = names_strength_props,
    var        = var
  )
})

knotclass_influence <- grid.arrange(grobs = boxplots_knots, ncol = 2)

knotclass_influence2 <- grid.arrange(grobs = boxplots_knots2, ncol = 2)

ggsave("plots/knotclass_influence.png", plot = knotclass_influence,
       width = 15, height = 8.5, units = "in", dpi = 1000)

ggsave("plots/knotclass_influence2.png", plot = knotclass_influence2,
       width = 15, height = 8.5, units = "in", dpi = 1000)


####################
## d) Dunn's Test ##

# function for Dunn's Test
perform_dunn_test <- function(data, properties, group_col) {
  results <- lapply(properties, function(col) {
    dunnTest(as.formula(paste(col, "~", group_col)), data = data, method = "bonferroni", list = TRUE, table = FALSE)
  })
  names(results) <- properties
  return(results)
}

# Running Dunn's Test
# Normal knot-size classes
dunn_knots <- perform_dunn_test(lamellae_knots, strength_props, "knot_class")

# Splitted knot-size classes
dunn_knots2 <- perform_dunn_test(lamellae_knots, strength_props, "knot_class2")

# Helper function to filter only direct neighbor comparisons
filter_direct_comparisons <- function(df) {
  df %>% 
    filter(sapply(Comparison, function(x) {
      groups <- as.numeric(str_split(x, " - ")[[1]])
      abs(diff(groups)) == 1
    }))
}

# Process normal knot-size classes:
dunn_normal_MOE <- as.data.frame(dunn_knots$MOE$res) %>% 
  filter_direct_comparisons() %>% 
  mutate(Outcome = "MOE", Type = "normal")
dunn_normal_MOR <- as.data.frame(dunn_knots$MOR$res) %>% 
  filter_direct_comparisons() %>% 
  mutate(Outcome = "MOR", Type = "normal")

# Process split knot-size classes:
dunn_split_MOE <- as.data.frame(dunn_knots2$MOE$res) %>% 
  filter_direct_comparisons() %>% 
  mutate(Outcome = "MOE", Type = "split")
dunn_split_MOR <- as.data.frame(dunn_knots2$MOR$res) %>% 
  filter_direct_comparisons() %>% 
  mutate(Outcome = "MOR", Type = "split")

# Combine the processed results and keep only the desired columns:
final_table <- bind_rows(dunn_normal_MOE, dunn_normal_MOR,
                         dunn_split_MOE, dunn_split_MOR) %>% 
  select(Outcome, Type, Comparison, Z, `P.adj`)

# Print the final table
print(final_table)



####################################################
### 3. Density Influence with Grouped Knot Sizes ###
####################################################

lamellae <- readRDS("data/lamellae.rds")

# create individual data frames for the Quality classes with corresponding knot size
lamellae1 <- lamellae %>% filter(Quality == 1, max_knot <= 17)

lamellae2 <- lamellae %>% filter(Quality == 2, max_knot >=17, max_knot <= 34, knot_decisive == 1)

lamellae3 <- lamellae %>% filter(Quality == 3, max_knot >= 34, knot_decisive == 1)


##########################
## a) Regression Models ##

# Initializing Data Frame
lm_knots_dens <- data.frame(Quality = character(),
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
                            stringsAsFactors = FALSE)

# Create a list of subsets for looping
list_dfs <- list(lamellae1, lamellae2, lamellae3)

# Loop over subsets and perform linear regressions
for (i in 1:3) {
  data_q <- list_dfs[[i]]
  q <- i  # Quality label
  
  ## MOE ~ Density
  lm_moe <- lm(MOE ~ Density, data = data_q)
  
  s_moe <- summary(lm_moe)
  
  row_moe <- data.frame(Quality = as.character(q),
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
                        stringsAsFactors = FALSE)
  
  lm_knots_dens <- rbind(lm_knots_dens, row_moe)
  
  ## MOR ~ Density
  lm_mor <- lm(MOR ~ Density, data = data_q)
  
  s_mor <- summary(lm_mor)
  
  row_mor <- data.frame(Quality = as.character(q),
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
                        stringsAsFactors = FALSE)
  
  lm_knots_dens <- rbind(lm_knots_dens, row_mor)
  
}

# Defining colors for the knot-size classes
colors_knotcat <- c("1" = "darkgreen",
                    "2" = "forestgreen",
                    "3" = "palegreen3")

# Defining names for the knot size classes
names_knotsize <- c("1" = "Small Knots (0-17% of Lamella Width)",
                    "2" = "Medium Knots (17-34% of Lamella Width)",
                    "3" = "Large Knots (34-100% of Lamella Width)")

name_dens <- c("Density" = "Density [kg/m³]")

names_strength <- c("MOR" = "MOR [MPa]",
                    "MOE" = "MOE [GPa]")

# Determine global min and max for Density, MOE, and MOR across ALL subsets
density_min <- min(c(lamellae1$Density, lamellae2$Density, lamellae3$Density), na.rm = TRUE)
density_max <- max(c(lamellae1$Density, lamellae2$Density, lamellae3$Density), na.rm = TRUE)

moe_min <- min(c(lamellae1$MOE, lamellae2$MOE, lamellae3$MOE), na.rm = TRUE)
moe_max <- max(c(lamellae1$MOE, lamellae2$MOE, lamellae3$MOE), na.rm = TRUE)

mor_min <- min(c(lamellae1$MOR, lamellae2$MOR, lamellae3$MOR), na.rm = TRUE)
mor_max <- max(c(lamellae1$MOR, lamellae2$MOR, lamellae3$MOR), na.rm = TRUE)

###########################################
## b) Visualization of Regression Models ##

scatterplot_lm_knot_dens <- function(data, response, Quality, y_pos, y_pos2, include_title = TRUE, point_shape) {
  
  # extracting linear regression results
  model_params <- lm_knots_dens[
    lm_knots_dens$Quality == as.character(Quality) & 
      lm_knots_dens$Response == response, 
  ]
  
  # Format the regression annotation text with the linear regression results
  annot_text <- paste0("Slope = ", round(model_params$Slope, 3), "\n",
                       "p = ", formatC(model_params$p_value, format = "e", digits = 1), "\n",
                       "R² = ", round(model_params$R_squared, 2), "\n",
                       "RSE = ", round(model_params$Residual_SE, 2), "\n",
                       "F = ", round(model_params$F_statistic, 1))
  
  # Compute coordinates for the regression annotation (upper left area)
  x_min_reg <- 350                   # fixed lower x position for annotation
  x_range <- diff(range(data$Density, na.rm = TRUE))
  y_max_reg <- y_pos                 # provided y maximum for annotation placement
  y_range <- diff(range(data[[response]], na.rm = TRUE))
  x_reg <- x_min_reg + 0.025 * x_range
  y_reg <- y_max_reg - 0.025 * y_range
  
  # Build the base plot with points, regression line, and regression annotation text
  p <- ggplot(data, aes(x = Density, y = .data[[response]])) +
    geom_point(color = colors_knotcat[as.character(Quality)], shape = point_shape) +
    labs(x = name_dens, y = names_strength[response]) +
    geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
    annotate("label", x = x_reg, y = y_reg, label = annot_text,
             color = "darkorange", hjust = 0, vjust = 1, size = 5, fill = "white", alpha = 0.75) +
    theme_minimal() +
    theme(axis.text = element_text(size = 14, color = "gray25"),
          axis.title = element_text(size = 16, color = "black", face = "bold"),
          title = element_text(size = 18, color = "black", face = "bold"))
  
  # Add title if requested
  if (include_title) {
    p <- p + ggtitle(names_knotsize[as.character(Quality)])
  } else {
    p <- p + ggtitle("")
  }
  
  # Apply consistent x-axis and y-axis limits across all plots
  if (response == "MOE") {
    p <- p + coord_cartesian(xlim = c(350, 525),
                             ylim = c(moe_min, moe_max))
  } else if (response == "MOR") {
    p <- p + coord_cartesian(xlim = c(350, 525),
                             ylim = c(mor_min, mor_max))
  }
  
  return(p)
}

# Plot list
plot_list <- list()

# Storing plots for knot-size class 0-17
plot_list[["lamellae1_MOE"]] <- scatterplot_lm_knot_dens(lamellae1, "MOE", 1, 12.5, 4, include_title = TRUE, 9)
plot_list[["lamellae1_MOR"]] <- scatterplot_lm_knot_dens(lamellae1, "MOR", 1, 90, 20, include_title = FALSE, 8)

# Storing plots for knot-size class 17-34
plot_list[["lamellae2_MOE"]] <- scatterplot_lm_knot_dens(lamellae2, "MOE", 2, 12.5, 4, include_title = TRUE, 9)
plot_list[["lamellae2_MOR"]] <- scatterplot_lm_knot_dens(lamellae2, "MOR", 2, 90, 20, include_title = FALSE, 8)

# Storing plots for knot-size class 34-100
plot_list[["lamellae3_MOE"]] <- scatterplot_lm_knot_dens(lamellae3, "MOE", 3, 12.5, 4, include_title = TRUE, 9)
plot_list[["lamellae3_MOR"]] <- scatterplot_lm_knot_dens(lamellae3, "MOR", 3, 90, 20, include_title = FALSE, 8)

# Arranging plots in a 2x3 grid
knotsize_density <- grid.arrange(plot_list[["lamellae1_MOE"]], plot_list[["lamellae1_MOR"]],
                                 plot_list[["lamellae2_MOE"]], plot_list[["lamellae2_MOR"]],
                                 plot_list[["lamellae3_MOE"]], plot_list[["lamellae3_MOR"]],
                                 ncol = 2)

# Saving Plot
ggsave("plots/knots_density.png", plot = knotsize_density,
       width = 14.5, height = 17.5, units = "in", dpi = 700)




