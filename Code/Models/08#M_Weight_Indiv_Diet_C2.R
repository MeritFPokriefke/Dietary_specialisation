
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 08#M_Weight_Diet_C2

### Prediction 3: Individual fledging mass and dietary specialisation
### Bayesian STAN error-in variable models to test if nestlings with higher dietary specialisation show higher
### weight


### Accessed last: June 2026
######################################################


### packages and data

library(rstan)
library(dplyr)
library(shinystan)
library(posterior)

options(scipen = 999)  # turn off scientific notation

df1 <- read.csv("Data/Extracted/weight_indiv_d15_230626.csv", header=TRUE)
df2 <- read.csv("Data/Extracted/birds4_030925.csv", header=TRUE)


################################################################################################# nestling B ----
### data frame for blue tits nestlings only (nestling B) for errors-in variables model with diet (z) and fledging success (w)


df_z <- subset(df2, Species == "B" & Type %in% c("chick1", "chick2"))
df_z <- df_z[!is.na(df_z$propp1_B), ]  # 3 obs with no estimate excluded
df_w <- df1 %>%
  filter(!is.na(weight), Species == "B") # take out 4 observations where no weight data, choose blue tits


### match ppp broodIDs with weight data frame


df_z <- df_z %>% 
  filter(BroodID %in% df_w$BroodID) ## only keep identity if brood exist in both data frames

df_z$ID <- as.integer(factor(df_z$BroodID))
df_zIDs <- df_z[!duplicated(df_z$ID), ]####

### match indices with ring numbers in diet data 
df_w$ID <- df_zIDs$ID[match(df_w$BroodID, df_zIDs$BroodID)]

### take out 1 individual does not have exp score
df_w <- subset(df_w, !is.na(ID))

#################################################################################################### model B ----

### prepare data for stan
dfl<-list(N_z      = nrow(df_z),
          z        = df_z$propp1_B,
          #TMB      = df_z$TreatmentB,
          #TMG      = df_z$TreatmentG,
          #Box      = df_z$DoubleBox,
          #Y2024    = as.integer(df_z$year == "2024"),
          N_B      = length(unique(df_z$ID)),
          b_z      = df_z$ID,
          #N_spy    = length(unique(df_z$SPY)),
          #spy      = as.integer(factor(df_z$SPY)),
          N_wnr    = length(unique(df_z$WithinNestRep)),
          wnr      = as.integer(factor(df_z$WithinNestRep)),
        ### fitness (w)
          N_w      = nrow(df_w),
          w        = df_w$weight,
          TMB_w    = df_w$TreatmentB,
          TMG_w    = df_w$TreatmentG,
          Box_w    = df_w$Doublebox,
          Y2024_w  = as.integer(df_w$year == "2024"),
          time_w   = as.vector(scale(df_w$TimeAfterSunrise, scale=F)),
          size_w   = as.integer(df_w$brood_size),
          b_w      = as.integer(factor(df_w$ID)),
          N_spy_w  = length(unique(df_w$SPY)),
          spy_w    = as.integer(factor(df_w$SPY)))


### fit bayesian model using STAN
md <- stan("Code/Models/Mod8.stan", data = dfl, seed = 1025093769,
           chains = 4, iter = 6000,  warmup = 3000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

### save output locally
saveRDS(md, file = "Output/Models/M_B_Indiv_Weight_Diet_240626.rds")


### extract model coefficients
dat_zip <- as.data.frame(t(round(summary(md, pars = c("beta_0", "beta_0_prop",
                                "V_brood",
                                "V_wnr",
                                "V_e",
                                "VC_brood","VC_wnr","VC_e",
                                "phi","pi",
                                # fitness (w)
                                "beta_0_w_u",
                                "b_u",
                                "b_int_u",
                                "V_spy_w", 
                                "V_brood_w",
                                "V_e_w",
                                "VC_brood_w",
                                "VC_spy_w","VC_e_w",
                                "bluetit_effect", "greattit_effect",
                                "estimate1", "estimate2", "estimate3", "estimate4",
                                "sel_bluetit_effect", "sel_greattit_effect",
                                "sel_1", "sel_2", "sel_3",
                                "sel_4"
                                ))$summary[,c(1,4,6,8,9,10)],3)))



#rstan::get_seed(md) # model seed

#sum_md <- summary(md)$summary
#launch_shinystan(md) # check model output
#dat_diets <- as.data.frame(t(round(summary(md, pars = c("true_diets"))$summary[,c(1,4,6,8,9,10)],3)))
#pairs(md,pars = c("sigma_B","sigma_wnr","sigma_spy_w","sigma_e_w","beta_0_w","b[1]","lp__" ))


### save output
write.csv(dat_zip, "Output/Models/M_B_Indiv_Weight_Diet_240626.csv")


################################################################################################# nestling G ----
### data frame for great tits nestlings only (nestling G) for errors-in variables model with diet (z) and fledging success (w)


df_z <- subset(df2, Species == "G" & Type %in% c("chick1", "chick2"))
df_z <- df_z[!is.na(df_z$propp1_G), ]  # 3 obs with no estimate excluded
df_w <- df1 %>%
  filter(!is.na(weight), Species == "G") # take out 4 observations where no weight data, choose blue tits


### match ppp broodIDs with weight data frame


df_z <- df_z %>% 
  filter(BroodID %in% df_w$BroodID) ## only keep identity if brood exist in both data frames

df_z$ID <- as.integer(factor(df_z$BroodID))
df_zIDs <- df_z[!duplicated(df_z$ID), ]####

### match indices with ring numbers in diet data 
df_w$ID <- df_zIDs$ID[match(df_w$BroodID, df_zIDs$BroodID)]

### take out 1 individual does not have exp score
df_w <- subset(df_w, !is.na(ID))

#################################################################################################### model G ----

### prepare data for stan
dfl<-list(N_z      = nrow(df_z),
          z        = df_z$propp1_G,
          #TMB      = df_z$TreatmentB,
          #TMG      = df_z$TreatmentG,
          #Box      = df_z$DoubleBox,
          #Y2024    = as.integer(df_z$year == "2024"),
          N_B      = length(unique(df_z$ID)),
          b_z      = df_z$ID,
          #N_spy    = length(unique(df_z$SPY)),
          #spy      = as.integer(factor(df_z$SPY)),
          N_wnr    = length(unique(df_z$WithinNestRep)),
          wnr      = as.integer(factor(df_z$WithinNestRep)),
          ### fitness (w)
          N_w      = nrow(df_w),
          w        = df_w$weight,
          TMB_w    = df_w$TreatmentB,
          TMG_w    = df_w$TreatmentG,
          Box_w    = df_w$Doublebox,
          Y2024_w  = as.integer(df_w$year == "2024"),
          time_w   = as.vector(scale(df_w$TimeAfterSunrise, scale=F)),
          size_w   = as.integer(df_w$brood_size),
          b_w      = as.integer(factor(df_w$ID)),
          N_spy_w  = length(unique(df_w$SPY)),
          spy_w    = as.integer(factor(df_w$SPY)))


### fit bayesian model using STAN
md <- stan("Code/Models/Mod8.stan", data = dfl, seed = 22384268,
           chains = 4, iter = 6000,  warmup = 3000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))


### save output locally
saveRDS(md, file = "Output/Models/M_G_Indiv_Weight_Diet_240626.rds") # Bulk and tail EES low, but parameters checked


### extract model coefficients
dat_zip <- as.data.frame(t(round(summary(md, pars = c("beta_0", "beta_0_prop",
                                                      "V_brood",
                                                      "V_wnr",
                                                      "V_e",
                                                      "VC_brood","VC_wnr","VC_e",
                                                      "phi","pi",
                                                      # fitness (w)
                                                      "beta_0_w_u",
                                                      "b_u",
                                                      "b_int_u",
                                                      "V_spy_w", 
                                                      "V_brood_w",
                                                      "V_e_w",
                                                      "VC_brood_w",
                                                      "VC_spy_w","VC_e_w",
                                                      "bluetit_effect", "greattit_effect",
                                                      "estimate1", "estimate2", "estimate3", "estimate4",
                                                      "sel_bluetit_effect", "sel_greattit_effect",
                                                      "sel_1", "sel_2", "sel_3",
                                                      "sel_4"
))$summary[,c(1,4,6,8,9,10)],3)))



#rstan::get_seed(md) # model seed

#sum_md <- summary(md)$summary
#launch_shinystan(md) # check model output
#dat_diets <- as.data.frame(t(round(summary(md, pars = c("true_diets"))$summary[,c(1,4,6,8,9,10)],3)))
#pairs(md,pars = c("sigma_B","sigma_wnr","sigma_spy_w","sigma_e_w","beta_0_w","b[1]","lp__" ))


### save output
write.csv(dat_zip, "Output/Models/M_G_Indiv_Weight_Diet_240626.csv")
