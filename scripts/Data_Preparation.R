##########################
##########################
#### DATA PREPARATION ####
##########################
##########################

# 1. Lamellae Data Frame
#     a) renaming columns for readability
#     b) combining front and back side data of the lamellae
#     c) correcting typos that occurred in data acquisition
#     d) changing units for readability
#     e) removing columns that aren't used in the analysis
# 2. Knot Data Frame
#     a) checking for lamellae data that are missing front or back entry
#     b) removing columns that aren't used in the analysis
#     c) renaming columns to shorter names
#     d) transforming values to the same unit
#     e) combining front and back side entry of each lamella
#     f) adding a column that combines all knot types
#     g) combining the knot and lamellae data frame
#     h) saving the final lamellae data frame
# 3. Strict vs. Practical Classification Data Frame
#     a) preparing practical classification data frame for merging
#     b) preparing strict classification data frame for merging
#     c) merging data frames
#     d) removing rows with empty quality_experts fields
#     e) removing columns that aren't used in the analysis
#     f) renaming columns to shorter names
#     g) saving the final lamellae_experts data frame

# packages
library(dplyr)
library(tibble)
library(stringr)
library(tidyr)



##############################
### 1. Lamellae Data Frame ###
##############################

# read input data
df <- read.csv("data/dataset_cleaned.csv", header=TRUE, sep=";")


#########################################
## a) renaming columns for readability ##

df <- df %>%
  rename(pitch_pocket = pitch.pocket,
         reaction_wood = reaction.wood,
         quality_long = quality,
         Density = density,
         PrePD = pre_peak_displ,
         PostPD = post_peak_displ,
         PrePD_Class =pre_peak_cls,
         PostPD_Class = post_peak_cls,
         MOE = moe,
         MOR = mor)


###########################################################
## b) combining front and back side data of the lamellae ##

# combining the whole and cropped quality (cropped > whole)
df <- df %>%
  add_column(quality_combined = NA, .before = "quality_cropped") %>%
  mutate(quality_combined = ifelse(is.na(quality_cropped), quality_long, quality_cropped))

# list of wood defect columns
col <- c("knot_healthy", "knot_dead", "knot_cluster", "rot", "blue_stain", 
                         "grain", "pitch_pocket", "pith", "crack", "bark", "reaction_wood")

# function to combine rows, keeping the 1's but not duplicating them
combine_rows <- function(x) {
  ifelse(any(x == 1, na.rm = TRUE), 1, NA)
  }

# grouping by sample_name and combining the rows
df_halved <- df %>%
  group_by(sample_name) %>%
  mutate(across(all_of(col), combine_rows)) %>%
  # Create a new column for the highest value of quality_combined
  mutate(quality = max(quality_combined, na.rm = TRUE)) %>%
  ungroup()

# filter out rows where up_pos == 0 and keep the row with the highest quality value
l <- df_halved %>%
  group_by(sample_name) %>%
  slice_max(quality, n = 1) %>%
  filter(up_pos == 0) %>%
  ungroup()

l <- l %>%
  rename(Quality = quality)

###########################################################
## c) correcting typos that occurred in data acquisition ##

l$length[335] <- 600

l$Density[335] <- 0.0003979264

l$Density[2082] <- 0.000456

l$MOE[2082] <- 6.891675

l$MOR[2082] <- 50.41017


#######################################
## d) changing units for readability ##

# Density from [g/mm3] to [kg/m3]
l$Density <- l$Density * 1000000

# removing one low Density value (possible error in weight measurement)
l <- l[-c(204),]

##########################################################
## e) removing columns that aren't used in the analysis ##

l <- subset(l, select = -c(filename,
                           up_pos,
                           length,
                           width, 
                           height,
                           weight,
                           invalid,
                           PrePD_Class,
                           PostPD_Class, 
                           quality_combined,
                           quality_cropped,
                           quality_long,
                           rot,
                           blue_stain,
                           grain,
                           pitch_pocket,
                           pith,
                           crack,
                           bark,
                           wane,
                           borehole,
                           reaction_wood,
                           yellowish,
                           split)
            )



##########################
### 2. Knot Data Frame ###
##########################

# read original data
k <- read.csv("data/output_finalV2.csv", header=TRUE, sep= ";")

# extract sample name from filename
k <- k %>% mutate(sample_name = str_remove(filename, "^\\d{4}_\\d{2}_\\d{2}-")
                  %>% str_remove("\\.bmp$"))

k <- k %>% relocate(sample_name)


########################################################################
## a) checking for lamellae data that are missing front or back entry ##

k %>%
  mutate(base_name = sub("\\.[0-9]+$", "", sample_name)) %>%
  group_by(base_name) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(count != 2) %>%
  print()

# removing single data entries
k <- k[-c(925, 4864),]

##########################################################
## b) removing columns that aren't used in the analysis ##

k <- subset(k, select = -c(filename,
                           num_resin,
                           num_resin_bigger_20,
                           num_resin_bigger_80,
                           overgrown_bool,
                           overgrown_area,
                           marrow_bool,
                           blue_stain_area,
                           rot_bool,
                           rot_area,
                           crack_max_dia,
                           num_live_knots,
                           num_dead_knots,
                           max_dia_dead_knots,
                           max_width_knot_collection)
            )


##########################################
## c) renaming columns to shorter names ##

k <- k %>% rename(width_kh = max_width_live_knots,
                  width_kd = max_width_dead_knots)


#############################################
## d) transforming values to the same unit ##

# dead knots [mm] to [%]
k$width_kd <- k$width_kd * 100

# healthy knot * 100
k$width_kh <- k$width_kh * 100


############################################################
## e) combining front and back side entry of each lamella ##

# helper functions to handle NA values
max_na <- function(x) {
  if (all(is.na(x))) NA else max(x, na.rm = TRUE)
}

# processing data frame
k2 <- k %>%
  # Remove the ending (.1, .2, etc.) from the name:
  mutate(base_name = sub("\\.[0-9]+$", "", sample_name)) %>%
  group_by(base_name) %>%
  summarise(width_kh = max_na(width_kh),
            width_kd   = max_na(width_kd)
            ) %>%
  rename(sample_name = base_name)


#####################################################
## f) adding a column that combines all knot types ##

k2 <- k2 %>%
  mutate(max_knot = pmax(width_kh, width_kd, na.rm = TRUE))

# setting NA-values to 0
k2$max_knot[is.na(k2$max_knot)] <- 0

# remove individual dead and healthy knot columns
k2 <- subset(k2, select = -c(width_kh,
                             width_kd)
             )

###################################################
## g) combining the knot and lamellae data frame ##

lamellae <- merge(l, k2[, c("sample_name", "max_knot")],
            by = "sample_name", all.x = TRUE)

# setting Quality as numeric
lamellae$Quality <- as.numeric(lamellae$Quality)

# adding column with Quality as a factor
lamellae$Quality_Factor <- as.factor(lamellae$Quality)

# moving the Quality columns next to sample_name
lamellae <- lamellae %>% select(sample_name, Quality, Quality_Factor, everything())

# combining the indication whether a healthy or dead knot was decisive for the quality classification
lamellae <- lamellae %>%
  rowwise() %>%
  mutate(knot_decisive = max_na(c(knot_healthy, knot_dead))) %>%
  ungroup()

# removing the columns indicating which knot type was decisive for the quality classification
lamellae <- subset(lamellae, select = -c(knot_healthy,
                                         knot_dead,
                                         knot_cluster)
                   )


#############################################
## h) saving the final lamellae data frame ##

saveRDS(lamellae, "data/lamellae.rds")

write.csv(lamellae, "data/lamellae.csv", row.names = FALSE)



#########################################################
### 3. Strict vs. Practical Classification Data Frame ###
#########################################################

# reading results of practical quality classification
pc <- read.csv("data/expert_cls.csv", header=TRUE, sep=";")

# reading original lamellae data
l2 <- read.csv("data/dataset_cleaned.csv", header = TRUE, sep = ";")


##################################################################
## a) preparing practical classification data frame for merging ##

# rename columns
pc <- pc %>%
  rename(sample_name = Sample.Name,
         quality_experts = Ergebnis)

# setting quality as a factor and sample name as a character
pc$quality_experts <- as.factor(pc$quality_experts)
pc$sample_name <- as.character(pc$sample_name)


###############################################################
## b) preparing strict classification data frame for merging ##

# filling empty column fields: length, weight and density
l2 <- l2 %>%
  group_by(sample_name) %>%
  fill(length, weight, density,
       .direction = "downup") %>%
  ungroup()

# renaming columns
l2 <- l2 %>%
  rename(pitch_pocket = pitch.pocket,
         reaction_wood = reaction.wood)

# removing sample_name column (missing up/down indication)
l2 <- subset(l2, select = -c(sample_name))

# extracting complete sample name out of file name
l2 <- l2 %>%
  mutate(sample_name = str_remove(filename, "^\\d{4}_\\d{2}_\\d{2}-") %>%
           str_remove("\\.bmp$"))


############################
## c) merging data frames ##

pc2 <- merge(l2, pc[, c("sample_name", "quality_experts")],
             by = "sample_name", all.x = TRUE)


#######################################################
# d) removing rows with empty quality_experts fields ##

pc2 <- pc2 %>%
  filter(!is.na(quality_experts))


##########################################################
## e) removing columns that aren't used in the analysis ##

pc2 <- subset(pc2, select = -c(filename,
                               up_pos,
                               length,
                               width,
                               height,
                               weight,
                               invalid,
                               pre_peak_cls,
                               post_peak_cls,
                               quality_cropped,
                               knot_healthy,
                               knot_dead,
                               knot_cluster,
                               rot,
                               blue_stain,
                               grain,
                               pitch_pocket,
                               pith,
                               crack,
                               bark,
                               wane,
                               borehole,
                               reaction_wood,
                               yellowish,
                               split))


##########################################
## f) renaming columns to shorter names ##

pc2 <- pc2 %>%
  rename(Quality = quality,
         Density = density,
         PrePD = pre_peak_displ,
         PostPD = post_peak_displ,
         MOE = moe,
         MOR = mor)


# setting Quality as a factor
pc2$Quality <- as.factor(pc2$Quality)

# Density from [g/mm3] to [kg/m3]
pc2$Density <- pc2$Density * 1000000


#####################################################
## g) saving the final lamellae_experts data frame ##

saveRDS(pc2, "data/lamellae_experts.rds")

write.csv(pc2, "data/lamellae_experts.csv", row.names = FALSE)

