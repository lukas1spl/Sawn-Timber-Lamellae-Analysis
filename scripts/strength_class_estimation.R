#####################################################
#####################################################
#### Influence of Density on Lamellae Properties ####
#####################################################
#####################################################

# 1. Mean Determination - MOE
# 2. 5th Percentile Determination - MOR
#     Method 1 - quantile() function
#     Method 2 - EN 14358 (non parametric)
# 3. 5th Percentile Determination - Density

# Packages
library(dplyr)



###################################
### 1. Mean Determination - MOE ###
###################################

# read input data
lamellae <- readRDS("data/lamellae.rds")

# split data into quality classes
lamellae1 <- lamellae %>% filter(Quality == 1)
lamellae2 <- lamellae %>% filter(Quality == 2)
lamellae3 <- lamellae %>% filter(Quality == 3)

mean_moe1 <- mean(lamellae1$MOE)
mean_moe2 <- mean(lamellae2$MOE)
mean_moe3 <- mean(lamellae3$MOE)



#############################################
### 2. 5th Percentile Determination - MOR ###
#############################################

# read input data
lamellae <- readRDS("data/lamellae.rds")

# split data into quality classes
lamellae1 <- lamellae %>% filter(Quality == 1)
lamellae2 <- lamellae %>% filter(Quality == 2)
lamellae3 <- lamellae %>% filter(Quality == 3)

####################################
## Method 1 - quantile() function ##

quant_mor1 <- quantile(lamellae1$MOR, probs = 0.05)
quant_mor2 <- quantile(lamellae2$MOR, probs = 0.05)
quant_mor3 <- quantile(lamellae3$MOR, probs = 0.05)


##########################################
## Method 2 - EN 14358 (non-parametric) ##

# number of samples per quality class
n1 <- 633
n2 <- 915
n3 <- 976

# means of MOR-values per quality class
mean1 <- mean(lamellae1$MOR)
mean2 <- mean(lamellae2$MOR)
mean3 <- mean(lamellae3$MOR)

# standard deviations of MOR-values per quality class
std1 <- sd(lamellae1$MOR)
std2 <- sd(lamellae2$MOR)
std3 <- sd(lamellae3$MOR)

# variation coefficient for quantile estimation of each quality class
var_coef1 <- std1 / mean1
var_coef2 <- std2 / mean2
var_coef3 <- std3 / mean3

# k0.5,0.75 for quantile estimation of each quality class
k1 <- (0.49 * n1 + 17) / (0.28 * n1 + 7.1)
k2 <- (0.49 * n2 + 17) / (0.28 * n2 + 7.1)
k3 <- (0.49 * n3 + 17) / (0.28 * n3 + 7.1)

# 5th percentile determination
EN14358_mor1 <- quant_mor1 * (1 - ((k1 * var_coef1) / (sqrt(n1))))
EN14358_mor2 <- quant_mor2 * (1 - ((k2 * var_coef2) / (sqrt(n2))))
EN14358_mor3 <- quant_mor3 * (1 - ((k3 * var_coef3) / (sqrt(n3))))


#################################################
### 3. 5th Percentile Determination - Density ###
#################################################

# read input data
lamellae <- readRDS("data/lamellae.rds")

# split data into quality classes
lamellae1 <- lamellae %>% filter(Quality == 1)
lamellae2 <- lamellae %>% filter(Quality == 2)
lamellae3 <- lamellae %>% filter(Quality == 3)

# quantile estimation
quant_dens1 <- quantile(lamellae1$Density, probs = 0.05)
quant_dens2 <- quantile(lamellae2$Density, probs = 0.05)
quant_dens3 <- quantile(lamellae3$Density, probs = 0.05)



