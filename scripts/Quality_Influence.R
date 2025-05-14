####################################################################
####################################################################
#### Influence of Quality Classification on Lamellae Properties ####
####################################################################
####################################################################

# 1. Spearman Correlation Coefficients
# 2. Kruskal-Wallis Significance Test
#     a) Kruskal-Wallis Test
#     b) Mean Calculation
# 3. Visualization of 1. + 2.
# 4. Dunn's Test + Table (Latex)
#     a) Dunn's Test
#     b) Saving Results in a Table

# Packages
library(dplyr)
library(reshape2)
library(ggplot2)
library(patchwork)
library(FSA)
library(tidyr)
library(knitr)
library(kableExtra)
library(xtable)



############################################
### 1. Spearman Correlation Coefficients ###
############################################

lamellae <- readRDS("data/lamellae.rds")

# define lamellae properties for correlation calculations
cols_props <- c("MOE", "MOR", "PrePD", "PostPD", "Density")

# Compute Spearman correlation coefficients
cor_quality <- data.frame(
  Variable = factor(cols_props, levels = cols_props),
  Spearman_Correlation = sapply(cols_props, function(col) cor(lamellae[[col]], lamellae$Quality, method = "spearman"))
)



############################################
### 2. Kruskal-Wallis Significance Tests ###
############################################

lamellae <- readRDS("data/lamellae.rds")


############################
## a) Kruskal-wallis Test ##

# Define function to perform Kruskal-Wallis test
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

# Run the function and store results
kruskal_results <- perform_kruskal_tests(lamellae, cols_props, "Quality")


#########################
## b) Mean Calculation ##

# group lamellae properties by quality class
lamellae_grouped <- melt(lamellae, id.vars = "Quality", measure.vars = cols_props)

# compute means
means_lamellae <- lamellae_grouped %>%
  group_by(Quality, variable) %>%
  summarise(mean_value = round(mean(value, na.rm = TRUE), 3), .groups = "drop")



###################################
### 3. Visualization of 1. + 2. ###
###################################

# NOTE: 1. and 2. have to be executed for this part!

# define colors for quality classes
colors_qual <- c("1" = "royalblue4", 
                 "2" = "steelblue3",
                 "3" = "turquoise3")

# numeric levels to descriptive labels
names_qual <- c("1" = "N",
                "2" = "I",
                "3" = "A")

# add units to lamellae property names
names_mechprop <- c("MOE" = "MOE [GPa]",
                    "MOR" = "MOR [MPa]",
                    "PrePD" = "PrePD [mm]",
                    "PostPD" = "PostPD [mm]",
                    "Density" = "Density [kg/m³]")

# Generate a list of individual plots with correlation annotations
plot_list <- lapply(unique(lamellae_grouped$variable), function(var) {
  
  # Subset data for the current variable
  df_subset <- lamellae_grouped %>% filter(variable == var)
  means_subset <- means_lamellae %>% filter(variable == var)
  
  # Get correlation coefficient for this variable
  cor_value <- cor_quality %>% filter(Variable == var) %>% pull(Spearman_Correlation)
  
  # Extract Kruskal p-value for the current variable
  kruskal_p_value <- kruskal_results[var, "Kruskal_p"]
  kruskal_chi_value <- kruskal_results[var, "Kruskal_chisq"]
  
  x_pos <- 0.5   # left edge
  
  y_min <- min(df_subset$value, na.rm = TRUE)
  y_max <- max(df_subset$value, na.rm = TRUE) * 1.2
  y_pos_cor <- y_max 
  y_pos_chi <- y_pos_cor - 0.075 * (y_max - y_min)  
  y_pos_p <-  y_pos_chi - 0.075 * (y_max - y_min)
  
  # Create individual plot
  p <- ggplot(df_subset, aes(x = as.factor(Quality), y = value, fill = as.factor(Quality))) +
    geom_boxplot() +
    scale_fill_manual(values = colors_qual) +
    scale_x_discrete(labels = names_qual) +
    labs(x = "", y = names_mechprop[var]) +  
    theme_minimal(base_family = "Arial") +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 16),
          axis.title.y = element_text(size = 18, face = "bold"),
          axis.text.x = element_text(size = 18, face = "bold"),
          panel.background = element_rect(fill = "gray98", color = NA)) + 
    stat_summary(fun = mean, geom = "point", shape = 17, size = 3, color = "white") +
    geom_text(data = means_subset, aes(x = Quality, y = mean_value, label = round(mean_value,1)),
              vjust = -0.5, hjust = -0.35, size = 5.5, color = "white") +
    # Add correlation coefficients
    annotate("text", x = x_pos, y = y_pos_cor, hjust = 0, 
             label = bquote(r[s] == .(round(cor_value, 2))), 
             size = 6, fontface = "bold", color = "navy") +
    # Add Kruskal-Wallis chi-squared values
    annotate("text", x = x_pos, y = y_pos_chi, hjust = 0,
             label = paste("Kruskal-Wallis"),
             size = 6, fontface = "bold", color = "darkorange") +
    # Add Kruskal-Wallis p-values
    annotate("text", x = x_pos, y = y_pos_p, hjust = 0,
             label = bquote(chi^2 ~ "=" ~ .(round(kruskal_chi_value, 1)) ~ "        p = " ~ .(signif(kruskal_p_value, 2))),
             size = 6, fontface = "bold", color = "darkorange")
  
  return(p)
})

# Arrange the plots and add title
quality_influence <- wrap_plots(plot_list, ncol = 2)

# Display the final plot
print(quality_influence)

ggsave("plots/quality_influence.png", plot = quality_influence,
       width = 14.5, height = 17.5, units = "in", dpi = 700)



##############################################################
### 3. Dunn's Test - Pairwise Comparison of Quality Groups ###
##############################################################

lamellae <- readRDS("data/lamellae.rds")

####################
## a) Dunn's Test ##

# Function to perform Dunn's Test
perform_dunn_test <- function(data, properties, group_col) {
  results <- lapply(properties, function(col) {
    dunnTest(as.formula(paste(col, "~", group_col)), data = data, method = "bonferroni", list = TRUE, table = FALSE)
  })
  names(results) <- properties
  return(results)
}

# Props with significant Kruskal-Wallis results
signif_prop <- c("MOE", "MOR", "PrePD", "Density")

# Run Dunn's Test for significant properties
dunn_results <- perform_dunn_test(lamellae, signif_prop, "Quality_Factor")

# store the dunn results in a data frame
dunn_df <- do.call(rbind, lapply(names(dunn_results), function(prop) {
  df <- dunn_results[[prop]]$res 
  df$Property <- prop
  return(df)
}))

# define offsets
unique_comparisons <- unique(dunn_df$Comparison)
offsets <- seq(-0.15, 0.15, length.out = length(unique_comparisons))

# Assign offsets to each comparison
dunn_df <- dunn_df %>%
  group_by(Property) %>%
  mutate(X_offset = offsets[match(Comparison, unique_comparisons)])

dunn_df <- dunn_df %>%
  mutate(P.adj_mod = ifelse(P.adj > 0.05, P.adj + 0.02, P.adj))


##################################
## b) Saving Results in a Table ##

# Convert the data frame to an xtable object
dunn_table <- xtable(dunn_df)

# Print the LaTeX code to a file
print(dunn_table, type = "latex", file = "tables/dunn_test_results.tex", include.rownames = FALSE)

# Subset the relevant columns from dunn_df data frame
dunn_subset <- dunn_df %>%
  select(Comparison, Z, P.adj, Property)

## Pivot the data to a wide format so that each property gets its own Z and P.adj columns
# rounding the Z-values in dunn_subset
dunn_subset <- dunn_subset %>%
  mutate(Z = round(Z, 2))

# pivot data to a wide format
dunn_wide <- dunn_subset %>%
  pivot_wider(
    names_from = Property, 
    values_from = c(Z, P.adj),
    names_glue = "{Property}_{.value}"
  )

# adjusted p-values in scientific notation
dunn_wide <- dunn_wide %>%
  mutate(across(ends_with("P.adj"), ~ format(.x, scientific = TRUE, digits = 3)))

# reorder columns to provide each property with adjacent tZ and P.adj columns
properties <- c("Density", "PrePD", "MOE", "MOR")
ordered_cols <- c("Comparison")
for(prop in properties) {
  ordered_cols <- c(ordered_cols, paste0(prop, "_Z"), paste0(prop, "_P.adj"))
}
dunn_wide <- dunn_wide[, ordered_cols]

# LaTeX table with a multicolumn header.
latex_table <- kable(
  dunn_wide, 
  "latex", 
  booktabs = TRUE, 
  longtable = TRUE,
  col.names = c("Comparison", rep(c("Z", "P.adj"), times = length(properties))),
  caption = "Dunn's Test Results"
) %>%
  add_header_above(c(" " = 1, "Density" = 2, "PrePD" = 2, "MOE" = 2, "MOR" = 2))

# saving LaTeX code to a .tex file
cat(latex_table, file = "tables/dunn_test_results_multicol.tex")



