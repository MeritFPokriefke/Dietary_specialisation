
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 04#Plot_ppp_exp

### Prediction 1: Association prey preference and personality
### Regression line plot using model coefficients to show relationship between proportion of preferred prey and exploration behaviour

### Accessed last: January 2026
######################################################

### packages and df
library(dplyr)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(posterior)
library(brms)

df <- read.csv("Data/Extracted/birds4_030925.csv", header=TRUE) # fitness df
df3 <- read.csv("Data/Extracted/personality2_280825.csv", header=TRUE) # personality df


md_B <- readRDS("Output/Models/M_B_Pers_diet_040925.rds") # model output B
md_G <- readRDS("Output/Models/M_G_Pers_diet_040925.rds") # model output G


##################################################################################################### colors ----
### table 10, Okabe-ito colour friendly palette

"#4E79A7"
"#F28E2B"
"#E15759"
"#76B7B2"
"#59A14F"
"#EDC948"
"#B07AA1"
"#FF9DA7"
"#9C755F"
"#BAB0AC"
"#2E5C85"

### additional colours
"#26456E"
"#244A26"
"#3E7A3A"
"#8E857F"


###################################################################################################### A & C ----
### data frame bird type of adult and nestling for both species (A, C)

df <- df %>%
  mutate(LifeStage = case_when(
    Type %in% c("AF", "AM")   ~ "A",
    Type %in% c("chick1", "chick2") ~ "C",
    TRUE ~ NA_character_  
  ))


df <- df[!is.na(df$propp1_all), ] # 25 obs with no estimate excluded


### remove NA ( 1 double obs)
df3 <- df3[!is.na(df3$BroodID), ]

################################################################################################## females B ----
### data frame for blue tit females (females, B) with behaviour (z) and diet (w) df
df_w <- subset(df, df$Species == "B" )


### match exploration score individual IDs with diet data frame
df_z <- subset(df3, df3$Species == 0 )
df_z <- df_z %>% 
  filter(RingNumber %in% df_w$RingNumber) ## only keep identity if individual exist in both data frames

df_z$ID <- as.integer(factor(df_z$RingNumber))
df_zIDs <- df_z[!duplicated(df_z$ID), ]

### match indices with ring numbers in diet data 
df_w$ID <- df_zIDs$ID[match(df_w$RingNumber, df_zIDs$RingNumber)]

### take out 1 indivdual do not have exp score
df_B <- subset(df_w, !is.na(ID))


########################################################################################################## G ----
### data frame for great tit females (females, G) with behaviour (z) and diet (w) df

df_w <- subset(df, df$Species == "G" )

df_z <- subset(df3, df3$Species == 1 )
df_z <- df_z %>% 
  filter(RingNumber %in% df_w$RingNumber) 

df_z$ID <- as.integer(factor(df_z$RingNumber))
df_zIDs <- df_z[!duplicated(df_z$ID), ]

 
df_w$ID <- df_zIDs$ID[match(df_w$RingNumber, df_zIDs$RingNumber)]

df_G <- subset(df_w, !is.na(ID))

########################################################################################## scaled exp values ----

###B
### extract posterior samples from stan model
post <- rstan::extract(md_B)

### compute mean random effects (blups) per ID
blups <- data.frame(ID = 1:dim(post$I)[2], BLUP_Exp_zB = apply(post$I, 2, mean))

### calculate standard deviation for scaling
SD_B <- sd(blups$BLUP_Exp_zB) # divide by SD to get var to be one

### standardize blups to have variance ~1
blups$BLUP_Exp_zB <- blups$BLUP_Exp_zB / SD_B

### merge blups back into original data frame
df_B1 <- merge(df_B, blups, by = "ID", all.x = TRUE)

### create sequence of x values for prediction line
x_seq_B <- seq(min(df_B1$BLUP_Exp_zB, na.rm = TRUE),
               max(df_B1$BLUP_Exp_zB, na.rm = TRUE),
               length.out = 200)


###G
post <- rstan::extract(md_G)

blups <- data.frame(ID = 1:dim(post$I)[2], BLUP_Exp_zG = apply(post$I, 2, mean))

SD_G <- sd(blups$BLUP_Exp_zG) 

blups$BLUP_Exp_zG <- blups$BLUP_Exp_zG / SD_G

df_G1 <- merge(df_G, blups, by = "ID", all.x = TRUE)

x_seq_G <- seq(min(df_G1$BLUP_Exp_zG, na.rm = TRUE),
               max(df_G1$BLUP_Exp_zG, na.rm = TRUE),
               length.out = 200)



####################################################################################################### plot ----
### B
### coefficients
beta0 <- 0.717
b1    <- 0.011 * SD_B
b2    <- -0.087
bint  <- 0.062 * SD_B

### prediction grid for two treatments
pred_B <- data.frame(
  BLUP_Exp_zB = rep(x_seq_B, 2),
  TreatmentB  = factor(rep(c(0, 1), each = length(x_seq_B)))
)

### linear predictor 
# TreatmentB == 0: beta0 + b1*x
# TreatmentB == 1: beta0 + b2 + (b1 + bint)*x
pred_B$eta <- ifelse(
  pred_B$TreatmentB == 0,
  beta0 + b1 * pred_B$BLUP_Exp_zB,
  beta0 + b2 + (b1 + bint) * pred_B$BLUP_Exp_zB
)

# transform to marginal mean including zeros (because beta1_propp includes zeros)
post <- rstan::extract(md_B)
j <- 1  # set correctly
pi_hat <- mean(post$pi[, j])

pred_B$fit <- pi_hat * plogis(pred_B$eta)


### plot
a <- ggplot(df_B1, aes(x = BLUP_Exp_zB, y = propp1_B)) +
  geom_point(aes(color = factor(TreatmentB)), alpha = 0.7, size = 3.5) +
  geom_line(data = pred_B,
            aes(x = BLUP_Exp_zB, y = fit, color = factor(TreatmentB)),
            linewidth = 2, show.legend = FALSE) +
  # adjust limits/breaks as appropriate for proportion scale
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_continuous(limits = c(-3.5, 3.5), breaks = seq(-3, 3, by = 1)) +
  scale_color_manual(
    name   = "Treatment (blue tit)",
    values = c("0" = "darkgrey", "1" = "black"), #"0" = "#9C755F", "1" = "#4E79A7"
    labels = c("0" = "Low", "1" = "High")
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.line  = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 1)
  ) +
  labs(
    x = "Individual mean exploration (BLUP)",
    y = "Proportion of preferred prey"
  )

a


### G
# Coefficients
beta0 <- 0.494
b1    <- -0.057 * SD_G
b2    <- 0.255
bint  <- 0.021 * SD_G

# prediction grid for two treatments
pred_G <- data.frame(
  BLUP_Exp_zG = rep(x_seq_G, 2),
  TreatmentG  = factor(rep(c(0, 1), each = length(x_seq_G)))
)

# linear predictor 
# TreatmentB == 0: beta0 + b1*x
# TreatmentB == 1: beta0 + b2 + (b1 + bint)*x
pred_G$eta <- ifelse(
  pred_G$TreatmentG == 0,
  beta0 + b1 * pred_G$BLUP_Exp_zG,
  beta0 + b2 + (b1 + bint) * pred_G$BLUP_Exp_zG
)

# transform to marginal mean including zeros (because beta1_propp includes zeros)
post <- rstan::extract(md_G)
j <- 1  # set correctly
pi_hat <- mean(post$pi[, j])

pred_G$fit <- pi_hat * plogis(pred_G$eta)

### plot
b <- ggplot(df_G1, aes(x = BLUP_Exp_zG, y = propp1_G)) +
  geom_point(aes(color = factor(TreatmentG)), alpha = 0.7, size = 3.5) +
  geom_line(data = pred_G,
            aes(x = BLUP_Exp_zG, y = fit, color = factor(TreatmentG)),
            linewidth = 2, show.legend = FALSE) +
  # adjust limits/breaks as appropriate for proportion scale
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_continuous(limits = c(-3.5, 3.5), breaks = seq(-3, 3, by = 1)) +
  scale_color_manual(
    name   = "Treatment (great tit)",
    values = c("0" = "darkgrey", "1" = "black"), #("0" = "#BAB0AC", "1" = "#59A14F")
    labels = c("0" = "Low", "1" = "High")
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.line  = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 1)
  ) +
  labs(
    x = "Individual mean exploration (BLUP)",
    y = "Proportion of preferred prey",
    title = "Great tits")

b

##################################################################################################### figure ----
### patchwork plots side by side

PPPEXPA <- (a|b)














####################################################################################### BLUP z standardized -----
#now x axis not on scale of SD from teh whole phenotype, but of the spread of the BLUP


# Extract posterior draws blue tits
post <- rstan::extract(md_B)

Posterior_I <- as.data.frame(rstan::extract(md_B, pars = "I")) # posterior extract
Posterior_I$N_iter <- seq_along(Posterior_I[,1])

Posterior_I <- Posterior_I %>% # posterior BLUP in long format
  pivot_longer(cols = starts_with("I"),
               names_to = "ID",
               values_to = "Posterior",
               names_prefix = "I.",) 


Posterior_SD_I <- as.data.frame(rstan::extract(md_B, pars = "sigma_I")) # extract the SD

Posterior_SD_I$N_iter <- seq_along(Posterior_SD_I[,1]) # add identifier od the iteration to SD


Posterior_I$BLUP_scaled <- Posterior_I$Posterior / Posterior_SD_I$sigma_I[Posterior_I$N_iter] # scale it based on the spread of the BLUP

BLUP_scale_mean <- Posterior_I %>% #extract the mean
  group_by(ID) %>%
  summarise(BLUP = mean(BLUP_scaled))


df_B1 <- merge(df_B, BLUP_scale_mean, by = "ID", all.x = TRUE) # merge df


x_seq_B <- seq(min(df_B1$BLUP, na.rm = TRUE), # use points to draw a regression line
               max(df_B1$BLUP, na.rm = TRUE),
               length.out = 200)



median_Posterior_SD_I <- median(Posterior_SD_I$sigma_I) 

# divide them by tehir SD



###B
# Coefficients
beta0 <- 0.717
b1    <- 0.011 * median_Posterior_SD_I # scale only slopes
b2    <- -0.087 
bint  <- 0.062 * median_Posterior_SD_I

# Prediction grid for two treatments
pred_B <- data.frame(
  BLUP = rep(x_seq_B, 2),
  TreatmentB  = factor(rep(c(0, 1), each = length(x_seq_B)))
)

# Linear predictor 
# TreatmentB == 0: beta0 + b1*x
# TreatmentB == 1: beta0 + b2 + (b1 + bint)*x
pred_B$eta <- ifelse(
  pred_B$TreatmentB == 0,
  beta0 + b1 * pred_B$BLUP,
  beta0 + b2 + (b1 + bint) * pred_B$BLUP
)

# Transform to marginal mean including zeros (because beta1_propp includes zeros)
post <- rstan::extract(md_B)
j <- 1  # set correctly
pi_hat <- mean(post$pi[, j])

pred_B$fit <- pi_hat * plogis(pred_B$eta)



# Plot
c <- ggplot(df_B1, aes(x = BLUP, y = propp1_B)) +
  geom_point(aes(color = factor(TreatmentB)), alpha = 0.7, size = 3.5) +
  geom_line(data = pred_B,
            aes(x = BLUP, y = fit, color = factor(TreatmentB)),
            linewidth = 2, show.legend = FALSE) +
  # adjust limits/breaks as appropriate for proportion scale
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, by = 1)) +
  scale_color_manual(
    name   = "Treatment (blue tit)",
    values = c("0" = "darkgrey", "1" = "black"), #"0" = "#9C755F", "1" = "#4E79A7"
    labels = c("0" = "Low", "1" = "High")
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.line  = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 1)
  ) +
  labs(
    x = "Individual mean exploration (BLUP)",
    y = "Proportion of preferred prey"
  )

c



### Extract posterior draws great tits
post <- rstan::extract(md_G)

Posterior_I <- as.data.frame(rstan::extract(md_G, pars = "I")) # posterior extract
Posterior_I$N_iter <- seq_along(Posterior_I[,1])

Posterior_I <- Posterior_I %>% # posterior BLUP in long format
  pivot_longer(cols = starts_with("I"),
               names_to = "ID",
               values_to = "Posterior",
               names_prefix = "I.",) 


Posterior_SD_I <- as.data.frame(rstan::extract(md_G, pars = "sigma_I")) # extract the SD

Posterior_SD_I$N_iter <- seq_along(Posterior_SD_I[,1]) # add identifier od the iteration to SD


Posterior_I$BLUP_scaled <- Posterior_I$Posterior / Posterior_SD_I$sigma_I[Posterior_I$N_iter] # scale it based on the spread of the BLUP

BLUP_scale_mean <- Posterior_I %>% #extract the mean
  group_by(ID) %>%
  summarise(BLUP = mean(BLUP_scaled))


df_G1 <- merge(df_G, BLUP_scale_mean, by = "ID", all.x = TRUE) # merge df


x_seq_G <- seq(min(df_G1$BLUP, na.rm = TRUE), # use points to draw a regression line
               max(df_G1$BLUP, na.rm = TRUE),
               length.out = 200)



median_Posterior_SD_I <- median(Posterior_SD_I$sigma_I) 



###G
# Coefficients
beta0 <- 0.494
b1    <- -0.057 * median_Posterior_SD_I # scale only slopes
b2    <- 0.255
bint  <- 0.021 * median_Posterior_SD_I

# Prediction grid for two treatments
pred_G <- data.frame(
  BLUP = rep(x_seq_G, 2),
  TreatmentG  = factor(rep(c(0, 1), each = length(x_seq_G)))
)

# Linear predictor 
pred_G$eta <- ifelse(
  pred_G$TreatmentG == 0,
  beta0 + b1 * pred_G$BLUP,
  beta0 + b2 + (b1 + bint) * pred_G$BLUP
)

# Transform to marginal mean including zeros (because beta1_propp includes zeros)
post <- rstan::extract(md_G)
j <- 1  # set correctly
pi_hat <- mean(post$pi[, j])

pred_G$fit <- pi_hat * plogis(pred_G$eta)



# Plot
d <- ggplot(df_G1, aes(x = BLUP, y = propp1_G)) +
  geom_point(aes(color = factor(TreatmentG)), alpha = 0.7, size = 3.5) +
  geom_line(data = pred_G,
            aes(x = BLUP, y = fit, color = factor(TreatmentG)),
            linewidth = 2, show.legend = FALSE) +
  # adjust limits/breaks as appropriate for proportion scale
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, by = 1)) +
  scale_color_manual(
    name   = "Treatment",
    values = c("0" = "darkgrey", "1" = "black"), #"0" = "#9C755F", "1" = "#4E79A7"
    labels = c("0" = "Low", "1" = "High")
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.line  = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 1)
  ) +
  labs(
    x = "Individual mean exploration (BLUP)",
    y = "Proportion of preferred prey",
    title = "Great tits"
  )

d


##################################################################################################### figure ----


PPPEXPA2 <- (c|d)
