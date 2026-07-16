
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 04#M_Exp_Diet

### Prediction 1: Association prey preference and personality
### Bayesian STAN error-in variable models to determine if an individuals mean behaviour is related to its dietary specialisation
### and the interaction with competitive intensity


### Accessed last: September 2025
######################################################


### packages and data

library(rstan)
library(dplyr)
library(shinystan)

options(scipen = 999)  # turn off scientific notation

df <- read.csv("Data/Extracted/birds4_030925.csv", header=TRUE) # diet data
df3 <- read.csv("Data/Extracted/personality2_280825.csv", header=TRUE) # behaviour data


##################################################################################################### all, A ----
### model with bird type of adult for both species (all; A)

df <- df %>%
  mutate(LifeStage = case_when(
    Type %in% c("AF", "AM")   ~ "A",
    Type %in% c("chick1", "chick2") ~ "C",
    TRUE ~ NA_character_   
  ))


df <- df[!is.na(df$propp1_all), ]  # 25 obs with no estimate excluded


### remove NA (double obs)
df3 <- df3[!is.na(df3$BroodID), ] # 1 double obs due to naming of brood IDs


##################################################################################################### all df ----
### create data frame for errors-in variables model with behaviour (z) and diet (w) data frames

df_w <- subset(df, df$LifeStage == "A") # select adults only


### match exploration score individual IDs with diet data frame

df_z <- df3
df_z <- df_z %>% 
  filter(RingNumber %in% df_w$RingNumber) ## only keep identity if individual exist in both data frames

df_z$ID <- as.integer(factor(df_z$RingNumber))
df_zIDs <- df_z[!duplicated(df_z$ID), ]

### match indices with ring numbers in diet data 
df_w$ID <- df_zIDs$ID[match(df_w$RingNumber, df_zIDs$RingNumber)]

### take out 1 individual does not have exp score
df_w <- subset(df_w, !is.na(ID))


################################################################################################## model all ----
### prep proportion loop (in case more than one proportion then loop is already in place)

proportion <- c("propp1_all") # proportion for both species combined

J <- length(proportion) # number proportions

w_mat <- as.matrix(df_w[paste0(proportion)])  # N_w x J, matrix


### prepare data for stan
dfl<-list(N_z      = nrow(df_z),
          z        = df_z$ExpScore,
          Seq2     = as.integer(df_z$Sequence == 2),
          Seq3     = as.integer(df_z$Sequence == 3),
          Seq4     = as.integer(df_z$Sequence == 4),
          Time     = as.vector(scale(df_z$TimeAfterSunrise, scale=F)),
          Sex      = df_z$Sex,
          G        = as.integer(df_z$Species == 1),
          N_I      = length(unique(df_z$ID)),
          ind_z    = df_z$ID,
          N_spy_z  = length(unique(df_z$SubplotYear)),
          spy_z    = as.integer(factor(df_z$SubplotYear)),
          J        = J,
          N_w      = nrow(df_w),
          w        = w_mat,
          TMB      = df_w$TreatmentB,
          TMG      = df_w$TreatmentG,
          G_w      = as.integer(df_w$Species == "G"),
          Y2024    = as.integer(df_w$year == "2024"),
          Date_w   = as.numeric(scale(df_w$AdateSample, scale = FALSE)),
          ind_w    = df_w$ID,
          N_spy_w  = length(unique(df_w$SPY)),
          spy_w    = as.integer(factor(df_w$SPY)))


### fit bayesian model using STAN
md <- stan("Code/Models/Mod4.stan", data = dfl, seed = 1070401745,
           chains = 4, iter = 6000,  warmup = 3000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

### save output locally
saveRDS(md, file = "Output/Models/M_All_Pers_diet_040925.rds")

### extract model coefficients
sum_all <- summary(md, pars = c("mu_u", "be_u",
                                "V_i_u","V_spy_u","V_e_u",
                                "VC_i","VC_spy","VC_e",
                                "mu_w","mean_w","mean_w_zi",
                                "b1_u","b",
                                "b_int_u", "b_int2_u","b_int4_u", "b_int",
                                "V_spy_w","V_i_w","V_e_w",
                                "VC_spy_w","VC_i_w","VC_e_w",
                                "phi","pi",
                                "bluetit_effect", "greattit_effect",
                                "estimate1", "estimate2", "estimate3", "estimate4",
                                "exploration_bluetit_effect", "exploration_greattit_effect",
                                "exploration_estimate1", "exploration_estimate2", "exploration_estimate3",
                                "exploration_estimate4"))$summary

sum_sel <- sum_all[, c(1,4,6,8,9,10)]
dat_zip <- as.data.frame(t(round(sum_sel, 3)))

### relabel phenotype (behaviour model) betas
covariates <- c("Seq2","Seq3","Seq4","Time","Sex", "Species")
for (k in seq_along(covariates)) {
  dat_zip <- dplyr::rename_with(dat_zip,
                                ~ gsub(paste0("be_u\\[", k, "\\]"),
                                       paste0("be_u_", covariates[k]), .x))
}

# relabel diet parameters (w) by order
for (j in seq_along(proportion)) {
  dat_zip <- dplyr::rename_with(dat_zip,
                                ~ gsub(paste0("\\[", j, "\\]"),
                                       paste0("_", proportion[j]), .x))
}

#rstan::get_seed(md) # get model seed
#launch_shinystan(md) # check output
#dat_plot <- as.data.frame(round(summary(md)$summary[,c(1,4,6,8,9,10)],3))


### save output
write.csv(dat_zip, "Output/Models/M_All_Pers_diet_040925.csv")
write.csv(dat_plot, "Output/Plots/Plot_M_All_Pers_diet_040925.csv")


########################################################################################################## B ----
### filter data for blue tits only (B)

df_w <- subset(df, df$Species == "B" )


df_z <- subset(df3, df3$Species == 0 )
df_z <- df_z %>% 
  filter(RingNumber %in% df_w$RingNumber) 

df_z$ID <- as.integer(factor(df_z$RingNumber))
df_zIDs <- df_z[!duplicated(df_z$ID), ]


df_w$ID <- df_zIDs$ID[match(df_w$RingNumber, df_zIDs$RingNumber)]


df_w <- subset(df_w, !is.na(ID))

### check repeated individual observations
#summary_z <- df_z %>%
#  group_by(Sex) %>%
#  summarise(
#    n_obs = n(),                     # total number of rows / observations
#    n_individuals = n_distinct(ID)   # number of unique individuals
#  )

#print(summary_z)

#################################################################################################### model B ----


proportion <- c("propp1_B")

J <- length(proportion)

w_mat <- as.matrix(df_w[paste0(proportion)]) 



dfl<-list(N_z      = nrow(df_z),
          z        = df_z$ExpScore,
          Seq2     = as.integer(df_z$Sequence == 2),
          Seq3     = as.integer(df_z$Sequence == 3),
          Seq4     = as.integer(df_z$Sequence == 4),
          Time     = as.vector(scale(df_z$TimeAfterSunrise, scale=F)),
          Sex      = df_z$Sex,
          N_I      = length(unique(df_z$ID)),
          ind_z    = df_z$ID,
          N_spy_z  = length(unique(df_z$SubplotYear)),
          spy_z    = as.integer(factor(df_z$SubplotYear)),
          J        = J,
          N_w      = nrow(df_w),
          w        = w_mat,
          TMB      = df_w$TreatmentB,
          TMG      = df_w$TreatmentG,
          Y2024    = as.integer(df_w$year == "2024"),
          Date_w   = as.numeric(scale(df_w$AdateSample, scale = FALSE)),
          ind_w    = df_w$ID,
          N_spy_w  = length(unique(df_w$SPY)),
          spy_w    = as.integer(factor(df_w$SPY)))


md <- stan("Code/Models/Mod4.1.stan", data = dfl, seed = 715958732,
           chains = 4, iter = 6000,  warmup = 3000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

saveRDS(md, file = "Output/Models/M_B_Pers_diet_040925.rds")


sum_all <- summary(md, pars = c("mu_u", "be_u",
                                "V_i_u","V_spy_u","V_e_u",
                                "VC_i","VC_spy","VC_e",
                                "mu_w","mean_w","mean_w_zi",
                                "b1_u","b",
                                "b_int_u", "b_int2_u","b_int4_u", "b_int",
                                "V_spy_w","V_i_w","V_e_w",
                                "VC_spy_w","VC_i_w","VC_e_w",
                                "phi","pi",
                                "bluetit_effect", "greattit_effect",
                                "estimate1", "estimate2", "estimate3", "estimate4",
                                "exploration_bluetit_effect", "exploration_greattit_effect",
                                "exploration_estimate1", "exploration_estimate2", "exploration_estimate3",
                                "exploration_estimate4"))$summary

sum_sel <- sum_all[, c(1,4,6,8,9,10)]
dat_zip <- as.data.frame(t(round(sum_sel, 3)))


covariates <- c("Seq2","Seq3","Seq4","Time","Sex")
for (k in seq_along(covariates)) {
  dat_zip <- dplyr::rename_with(dat_zip,
                                ~ gsub(paste0("be_u\\[", k, "\\]"),
                                       paste0("be_u_", covariates[k]), .x))
}


for (j in seq_along(proportion)) {
  dat_zip <- dplyr::rename_with(dat_zip,
                                ~ gsub(paste0("\\[", j, "\\]"),
                                       paste0("_", proportion[j]), .x))
}

#rstan::get_seed(md)
#launch_shinystan(md)
#dat_plot <- as.data.frame(round(summary(md)$summary[,c(1,4,6,8,9,10)],3))

### save output
write.csv(dat_zip, "Output/Models/M_B_Pers_diet_040925.csv")
write.csv(dat_plot, "Output/Plots/Plot_M_B_Pers_diet_040925.csv")

########################################################################################################## G ----
### filter data for great tits only (G)

df_w <- subset(df, df$Species == "G" )


df_z <- subset(df3, df3$Species == 1 )
df_z <- df_z %>% 
  filter(RingNumber %in% df_w$RingNumber) 

df_z$ID <- as.integer(factor(df_z$RingNumber))
df_zIDs <- df_z[!duplicated(df_z$ID), ]

 
df_w$ID <- df_zIDs$ID[match(df_w$RingNumber, df_zIDs$RingNumber)]


df_w <- subset(df_w, !is.na(ID))


#################################################################################################### model G ----


proportion <- c("propp1_G")

J <- length(proportion)

w_mat <- as.matrix(df_w[paste0(proportion)]) 


dfl<-list(N_z      = nrow(df_z),
          z        = df_z$ExpScore,
          Seq2     = as.integer(df_z$Sequence == 2),
          Seq3     = as.integer(df_z$Sequence == 3),
          Seq4     = as.integer(df_z$Sequence == 4),
          Time     = as.vector(scale(df_z$TimeAfterSunrise, scale=F)),
          Sex      = df_z$Sex,
          N_I      = length(unique(df_z$ID)),
          ind_z    = df_z$ID,
          N_spy_z  = length(unique(df_z$SubplotYear)),
          spy_z    = as.integer(factor(df_z$SubplotYear)),
          J        = J,
          N_w      = nrow(df_w),
          w        = w_mat,
          TMB      = df_w$TreatmentB,
          TMG      = df_w$TreatmentG,
          Y2024    = as.integer(df_w$year == "2024"),
          Date_w   = as.numeric(scale(df_w$AdateSample, scale = FALSE)),
          ind_w    = df_w$ID,
          N_spy_w  = length(unique(df_w$SPY)),
          spy_w    = as.integer(factor(df_w$SPY)))


md <- stan("Code/Models/Mod4.1.stan", data = dfl, seed = 995243548,
           chains = 4, iter = 7000,  warmup = 3000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

saveRDS(md, file = "Output/Models/M_G_Pers_diet_040925.rds")


sum_all <- summary(md, pars = c("mu_u", "be_u",
                                "V_i_u","V_spy_u","V_e_u",
                                "VC_i","VC_spy","VC_e",
                                "mu_w","mean_w","mean_w_zi",
                                "b1_u","b",
                                "b_int_u", "b_int2_u","b_int4_u", "b_int",
                                "V_spy_w","V_i_w","V_e_w",
                                "VC_spy_w","VC_i_w","VC_e_w",
                                "phi","pi",
                                "bluetit_effect", "greattit_effect",
                                "estimate1", "estimate2", "estimate3", "estimate4",
                                "exploration_bluetit_effect", "exploration_greattit_effect",
                                "exploration_estimate1", "exploration_estimate2", "exploration_estimate3",
                                "exploration_estimate4"))$summary

sum_sel <- sum_all[, c(1,4,6,8,9,10)]
dat_zip <- as.data.frame(t(round(sum_sel, 3)))


covariates <- c("Seq2","Seq3","Seq4","Time","Sex")
for (k in seq_along(covariates)) {
  dat_zip <- dplyr::rename_with(dat_zip,
                                ~ gsub(paste0("be_u\\[", k, "\\]"),
                                       paste0("be_u_", covariates[k]), .x))
}


for (j in seq_along(proportion)) {
  dat_zip <- dplyr::rename_with(dat_zip,
                                ~ gsub(paste0("\\[", j, "\\]"),
                                       paste0("_", proportion[j]), .x))
}

#rstan::get_seed(md)
#launch_shinystan(md)
#dat_plot <- as.data.frame(round(summary(md)$summary[,c(1,4,6,8,9,10)],3))


write.csv(dat_zip, "Output/Models/M_G_Pers_diet_040925.csv")
write.csv(dat_plot, "Output/Plots/Plot_M_G_Pers_diet_040925.csv")


