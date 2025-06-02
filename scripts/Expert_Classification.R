################################################################
################################################################
#### Comparison Strict vs. Practical Quality Classification ####
################################################################
################################################################

# 1. Difference between Quality Classifications
#     a) McNemar Test
#     b) Confusion Matrix
# 2. Influence of Classification on Lamellae Properties
#     a) Correlation Coefficients
#     b) Kruskal-Wallis Test
#     c) Visualization
#     d) Dunn's Test

# Packages
library(ggplot2)
library(reshape2)
library(patchwork)
library(dplyr)
library(tidyr)
library(readr)
library(ggtext)
library(FSA)
library(knitr)
library(kableExtra)



####################################
### 1. Classification Difference ###
####################################

lamellae_experts <- readRDS("data/lamellae_experts.rds")


#####################
## a) McNemar Test ##

# McNemar Test - Significance Test of the difference between Strict and Expert Classification
mcnemar_qual_diff <- mcnemar.test(table(lamellae_experts$Quality, lamellae_experts$quality_experts))

# store p-value of McNemar Test
p_value <- mcnemar_qual_diff$p.value


#########################
## b) Confusion Matrix ##

# Calculate the difference of the assigned qualities
lamellae_experts$qual_diff <- as.factor(as.numeric(lamellae_experts$quality_experts) - as.numeric(lamellae_experts$Quality))

# numeric levels to descriptive labels
names_qual  <- c("1" = "N",
                 "2" = "I",
                 "3" = "A")

# ensuring both quality columns are factors
lamellae_experts$Quality <- factor(lamellae_experts$Quality, levels = c("1","2","3"), labels = names_qual)
lamellae_experts$quality_experts <- factor(lamellae_experts$quality_experts, levels = c("1","2","3"), labels = names_qual)

# computing confusion matrix
conf_matrix_named <- table(Own_Classification = lamellae_experts$Quality,
                           Expert_Classification = lamellae_experts$quality_experts)

# Convert the confusion matrix into a data frame
conf_matrix_df <- as.data.frame(as.table(conf_matrix_named))

# Create a heatmap-like plot of the confusion matrix
diff_matrix <- ggplot(conf_matrix_df, aes(x = Own_Classification, y = Expert_Classification, fill = Freq)) +
                      geom_tile(color = "white") +
                      geom_text(aes(label = Freq), size = 5) +
                      scale_fill_gradient(low = "lavenderblush", high = "steelblue3") +
                      scale_y_discrete(limits = rev(levels(conf_matrix_df$Expert_Classification))) +
                      labs(title = "",
                           x = "Strict Classification",
                           y = "Practical Classification") +
                      theme_minimal() +
                      theme(axis.text = element_text(size = 14, color = "gray25"),
                            axis.title = element_text(size = 16, color= "black", face = "bold"),
                            legend.position = "none")
                      

print(diff_matrix)

ggsave("plots/difference matrix.png", plot = diff_matrix,
       width = 6, height = 6, units = "in", dpi = 1000)



###########################################################################
### 2. Influence of Classification Difference on Mechanical Properties  ###
###########################################################################

lamellae_experts <- readRDS("data/lamellae_experts.rds")

lamellae_experts$qual_diff <- as.factor(as.numeric(lamellae_experts$quality_experts) - as.numeric(lamellae_experts$Quality))


#################################
## a) Correlation Coefficients ##

# Define the columns for the correlation coefficients
props <- c("MOE",
           "MOR",
           "PrePD",
           "PostPD",
           "Density")

# Combine both results in one data frame
cor_coeffs <- data.frame(MechProp = props,
                         Quality = sapply(props,
                                          function(col) cor(lamellae_experts[[col]],
                                                            as.numeric(lamellae_experts$Quality),
                                                            method = "spearman")),
                         Quality_Experts = sapply(props,
                                                  function(col) cor(lamellae_experts[[col]],
                                                                    as.numeric(lamellae_experts$quality_experts),
                                                                    method = "spearman"))
                         )


############################
## b) Kruskal-Wallis Test ##

# Function for Kruskal-Wallis test
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

# Perform the Kruskal-Wallis test
kruskal_results <- data.frame(MechProp = props,
                              Quality = perform_kruskal_tests(lamellae_experts,
                                                              props,
                                                              "Quality"),
                              Quality_Experts = perform_kruskal_tests(lamellae_experts,
                                                                      props,
                                                                      "quality_experts")
                              )


######################
## c) Visualization ##

names_qual <- c("1" = "N",
                "2" = "I",
                "3" = "A")

names_props <- c("MOE" = "MOE [GPa]",
                 "MOR" = "MOR [MPa]",
                 "PrePD" = "PrePD [mm]",
                 "PostPD" = "PostPD [mm]",
                 "Density" = "Density [kg/m³]")

# Reshape data for plotting
df_long <- lamellae_experts %>%
  pivot_longer(cols = all_of(props), names_to = "MechProp", values_to = "Value") %>%
  pivot_longer(cols = c("Quality", "quality_experts"), names_to = "Group", values_to = "Category") %>%
  mutate(
    Category = as.factor(Category),  # Ensure it's a factor
    Group = dplyr::recode(Group, "Quality" = "Quality", "quality_experts" = "Quality Experts")
  )

# color map to distinct between the strict and practical quality classes
color_map <- c("Quality_1" = "darkgreen", "Quality_2" = "darkgreen", "Quality_3" = "darkgreen",
               "Quality Experts_1" = "sienna3", "Quality Experts_2" = "sienna3", "Quality Experts_3" = "sienna3"
               )

# grouping for box plot visualization
df_long$Group_Category <- paste(df_long$Group, df_long$Category, sep = "_")

# converting Group_category into a factor
df_long$Group_Category <- factor(df_long$Group_Category, levels = c("Quality_1", "Quality_2", "Quality_3", "Quality Experts_1", "Quality Experts_2", "Quality Experts_3"))

# prepare annotation data
annotation_data <- full_join(
  cor_coeffs %>% 
    rename(Corr_Strict = Quality, Corr_Practical = Quality_Experts),
  kruskal_results %>% 
    rename(Chi_Strict = Quality.Kruskal_chisq,
           P_Strict   = Quality.Kruskal_p,
           Chi_Prac   = Quality_Experts.Kruskal_chisq,
           P_Prac     = Quality_Experts.Kruskal_p),
  by = "MechProp"
  ) %>%
  mutate(
    lbl_strict = paste0(
      "paste(",
      "'Strict Classification:\\n',",
      "'r'[s]==",  signif(Corr_Strict,2), ", '\\n',",
      "chi^2==",    signif(Chi_Strict,3),   ", '\\n',",
      "p==",        signif(P_Strict,3),
      ")"
    ),
    lbl_prac = paste0(
      "paste(",
      "'Practical Classification:\\n',",
      "'r'[s]==",  signif(Corr_Practical,2), ", '\\n',",
      "chi^2==",    signif(Chi_Prac,3),       ", '\\n',",
      "p==",        signif(P_Prac,3),
      ")"
    )
  )

# merging correlation and kruskal-wallis test values
annotation_data <- full_join(
  cor_coeffs %>% 
    rename(Corr_Strict    = Quality,
           Corr_Practical = Quality_Experts),
  kruskal_results %>% 
    rename(Chi_Strict  = Quality.Kruskal_chisq,
           P_Strict    = Quality.Kruskal_p,
           Chi_Prac    = Quality_Experts.Kruskal_chisq,
           P_Prac      = Quality_Experts.Kruskal_p),
  by = "MechProp"
)

# helper function to create boxplots
create_boxplots <- function(mech) {
  dfp <- df_long       %>% filter(MechProp == mech)
  ann <- annotation_data %>% filter(MechProp == mech)
  
  # pull out the six numbers
  cs <- ann$Corr_Strict
  chis_strict <- ann$Chi_Strict
  p_strict    <- ann$P_Strict
  cp <- ann$Corr_Practical
  chis_prac   <- ann$Chi_Prac
  p_prac      <- ann$P_Prac
  
  p_prac_sci <- formatC(p_prac, format = "e", digits = 1)
  p_strict_sci <- formatC(p_strict, format = "e", digits = 1)
  
  # compute a little vertical ladder
  y_min <- min(dfp$Value, na.rm = TRUE)
  y_max <- max(dfp$Value, na.rm = TRUE) * 1.15
  rng   <- y_max - y_min
  
  y_head        <- y_max
  y_rsp         <- y_head        - 0.075 * rng
  y_kw_head     <- y_rsp         - 0.06 * rng
  y_chi         <- y_kw_head     - 0.065 * rng
  y_p           <- y_chi         - 0.065 * rng
  
  # plot function
  ggplot(dfp, aes(x = Category, y = Value, fill = Group_Category)) +
    geom_boxplot(alpha = 0.8, position = position_dodge(0.8)) +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 3, color = "white",
                 position = position_dodge(0.8)) +
    stat_summary(fun = mean, geom = "text",
                 aes(label = sprintf("%.1f", ..y..)),
                 position = position_dodge(0.8),
                 vjust = -0.8, size = 3.5, color = "white") +
    annotate("rect",
             # these x‐limits frame everything around x=0.5
             xmin = 0.45, xmax = 1.55,
             # these y‐limits go from the bottom text up to the top text
             ymin = y_p   - 0.05*rng,
             ymax = y_head + 0.02*rng,
             fill   = "white",
             color  = "darkgreen",
             alpha  = 0.75,
             size   = 0.25) +
    annotate("text",
             x        = 0.5, y = y_head,
             label    = "Strict Classification:",
             hjust    = 0, vjust = 1,
             fontface = "bold",
             size     = 4,
             colour = "darkgreen") +
    annotate("text",
             x     = 0.5, y = y_rsp,
             label = bquote(r[s] == .(signif(cs,2))),
             parse = FALSE, hjust = 0, size = 4,
             colour = "darkgreen") +
    annotate("text",
             x        = 0.5, y = y_kw_head,
             label    = "Kruskal-Wallis",
             hjust    = 0, vjust = 1,
             size     = 4,
             colour = "darkgreen") +
    annotate("text",
             x     = 0.5, y = y_chi,
             label = bquote(chi^2 == .(signif(chis_strict,2))),
             parse = FALSE, hjust = 0, size = 4,
             colour = "darkgreen") +
    annotate("text",
             x     = 0.5, y = y_p,
             label = paste0("p =", p_strict_sci),
             parse = FALSE, hjust = 0, size = 4,
             colour = "darkgreen") +
    
    annotate("rect",
             # these x‐limits frame everything around x=0.5
             xmin = 2.275, xmax = 3.55,
             # these y‐limits go from the bottom text up to the top text
             ymin = y_p   - 0.05*rng,
             ymax = y_head + 0.02*rng,
             fill   = "white",
             color  = "sienna3",
             alpha  = 0.75,
             size   = 0.25) +
    annotate("text",
             x        = 3.5, y = y_head,
             label    = "Practical Classification:",
             hjust    = 1, vjust = 1,
             fontface = "bold",
             size     = 4,
             colour = "sienna3") +
    annotate("text",
             x     = 3.5, y = y_rsp,
             label = bquote(r[s] == .(signif(cp,2))),
             parse = FALSE, hjust = 1, size = 4,
             colour = "sienna3") +
    annotate("text",
             x        = 3.5, y = y_kw_head,
             label    = "Kruskal-Wallis",
             hjust    = 1, vjust = 1,
             size     = 4,
             colour = "sienna3") +
    annotate("text",
             x     = 3.5, y = y_chi,
             label = bquote(chi^2 == .(signif(chis_prac,2))),
             parse = FALSE, hjust = 1, size = 4,
             colour = "sienna3") +
    annotate("text",
             x     = 3.5, y = y_p,
             label = paste0("p =", p_prac_sci),
             parse = FALSE, hjust = 1, size = 4,
             colour = "sienna3") +
    
    scale_fill_manual(values = color_map) +
    scale_x_discrete(labels = names_qual) +
    labs(y = names_props[mech], x = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x      = element_text(face = "bold", size = 16, color = "black"),
      axis.text.y      = element_text(size = 14, color = "gray25"),
      axis.title.y     = element_text(face = "bold", size = 16, color = "black"),
      legend.position  = "none",
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
}

# build plot list and arragen them
plot_list  <- setNames(lapply(props, create_boxplots), props)
final_plot <- wrap_plots(plot_list, ncol = 2)
print(final_plot)

ggsave("plots/influence_classification.png", plot = final_plot,
       width = 11.6, height = 14, units = "in", dpi = 700)


####################
## d) Dunn's Test ##

# Example function for Dunn's test
perform_dunn_test <- function(data,
                              properties,
                              group_col
                              ) {
                        results <- lapply(properties,
                                        function(col) {
                                          dunnTest(as.formula(paste(col,
                                                                    "~",
                                                                    group_col)
                                                              ),
                                                   data = data,
                                                   method = "bonferroni",
                                                   list = TRUE,
                                                   table = FALSE
                                                   )  }
                                          )
                         names(results) <- properties
                         return(results)
                                            }

# define the lamellae props for the dunns test
props <- c("MOE",
           "MOR",
           "PrePD",
           "PostPD",
           "Density")


# Strict classification
dunn_results_strict <- perform_dunn_test(lamellae_experts, props, "Quality")

# Convert to data frame
dunn_df_strict <- do.call(rbind,
                          lapply(names(dunn_results_strict),
                                 function(prop) {
                                   df <- dunn_results_strict[[prop]]$res
                                   df$Property <- prop
                                   df$Classification <- "Strict"
                                   df }
                                 )
                          )

# Practical classification
dunn_results_practical <- perform_dunn_test(lamellae_experts, props, "quality_experts")

# Convert to data frame
dunn_df_practical <- do.call(rbind,
                             lapply(names(dunn_results_practical),
                                    function(prop) {
                                      df <- dunn_results_practical[[prop]]$res
                                      df$Property <- prop
                                      df$Classification <- "Practical"
                                      df }
                                    )
                             )

# Combine both into one data frame
dunn_df_combined <- rbind(dunn_df_strict, dunn_df_practical)

# transforming test results into a wide format for better readability
dunn_df_wide <- dunn_df_combined %>% select(Comparison,
                                            Classification,
                                            Property, Z, P.adj) %>%
  # Pivot so each property becomes its own pair of columns
  pivot_wider(
    names_from  = Property,
    values_from = c(Z, P.adj),
    names_sep   = "_"
  ) %>%
  select(
    Comparison, Classification,
    Z_Density,  P.adj_Density,
    Z_PrePD,    P.adj_PrePD,
    Z_PostPD,   P.adj_PostPD,
    Z_MOE,      P.adj_MOE,
    Z_MOR,      P.adj_MOR
  )

# format Z-scores and P.adj values
dunn_df_wide_formatted <- dunn_df_wide %>%
  mutate(
    across(starts_with("Z_"), ~ round(., 2)),
    across(starts_with("P.adj_"), ~ formatC(., format = "e", digits = 2))
  )

# create LaTeX output
kbl(dunn_df_wide_formatted, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position", "striped")) %>%
  add_header_above(c(" " = 2, "Density" = 2, "PrePD" = 2,"PostPD" = 2, "MOE" = 2, "MOR" = 2)) %>%  cat(., file = "tables/dunn_classification_comparison.tex")

