
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 05#M_Suc_Diet_C1

### Prediction 3: Fledging success and dietary specialisation
### Bayesian STAN error-in variable models to test if nestlings with higher dietary specialisation show higher
### fledging success


### Accessed last: September 2025
######################################################


### packages and data

library(rstan)
library(dplyr)
library(shinystan)

options(scipen = 999)  # turn off scientific notation

df <- read.csv("Data/Extracted/birds4_030925.csv", header=TRUE)
df$Success <- ifelse(df$NumberFledged == 0, 0, 1) # binary fledging success variable

###################################################################################################### A & C ----
### data frame for adults and nestlings (A&C)

df <- df %>%
  mutate(LifeStage = case_when(
    Type %in% c("AF", "AM")   ~ "A",
    Type %in% c("chick1", "chick2") ~ "C",
    TRUE ~ NA_character_   # catch anything else
  ))


df <- df[!is.na(df$propp1_all), ]  # 25 obs with no estimate excluded


###################################################################################################### count ----
### generate Species-Level Counts by Type and Life Stage (with Combined Summary Table)

### count by Type and Species
#type_counts <- df %>%
 # group_by(Species, Type) %>%
 # summarise(n = n(), .groups = "drop")

### count by LifeStage and Species
#stage_counts <- df %>%
 # group_by(Species, LifeStage) %>%
 # summarise(n = n(), .groups = "drop")

### combine into one table
#combined_counts <- df %>%
  #group_by(Species) %>%
  #summarise(
    #n_AF = sum(Type == "AF", na.rm = TRUE),
    #n_AM = sum(Type == "AM", na.rm = TRUE),
    #n_chick1 = sum(Type == "chick1", na.rm = TRUE),
    #n_chick2 = sum(Type == "chick2", na.rm = TRUE),
    #n_A = sum(LifeStage == "A", na.rm = TRUE),
    #n_C = sum(LifeStage == "C", na.rm = TRUE),
    #.groups = "drop"
 # )

#combined_counts


################################################################################################# nestling B ----
### data frame for blue tits nestlings only (nestling B) for errors-in variables model with diet (z) and fledging success (w)


df1 <- subset(df, df$Species == "B" & df$LifeStage == "C")

df_z <- subset(df1, !is.na(Success)) # take out 4 observations where it in unknown if they have fledged or not


df_w <- df_z %>%
  distinct(BroodID, .keep_all = TRUE) # create fledging success data frame for nestlings

#################################################################################################### model B ----

### prepare data for stan
dfl<-list(N_z      = nrow(df_z),
          z        = df_z$propp1_B,
          #TMB      = df_z$TreatmentB,
          #TMG      = df_z$TreatmentG,
          #Box      = df_z$DoubleBox,
          #Y2024    = as.integer(df_z$year == "2024"),
          N_B      = length(unique(df_z$BroodID)),
          b_z      = as.integer(factor(df_z$BroodID)),
          #N_spy    = length(unique(df_z$SPY)),
          #spy      = as.integer(factor(df_z$SPY)),
          N_wnr    = length(unique(df_z$WithinNestRep)),
          wnr      = as.integer(factor(df_z$WithinNestRep)),
          ## fitness (w)
          N_w      = nrow(df_w),
          w        = df_w$Success,
          TMB_w    = df_w$TreatmentB,
          TMG_w    = df_w$TreatmentG,
          Box_w    = df_w$Doublebox,
          Y2024_w  = as.integer(df_w$year == "2024"),
          b_w      = as.integer(factor(df_w$BroodID)),
          N_spy_w  = length(unique(df_w$SPY)),
          spy_w    = as.integer(factor(df_w$SPY)))


### fit bayesian model using STAN
md <- stan("Code/Models/Mod5.stan", data = dfl, seed = 272245533,
           chains = 4, iter = 5000,  warmup = 2000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

### save output locally
saveRDS(md, file = "Output/Models/M_B_Suc_Diet_240925.rds")

### extract model coefficients
dat_zip <- as.data.frame(t(round(summary(md, pars = c("beta_0", "beta_0_prop",
                                "V_brood",
                                "V_wnr",
                                "V_e",
                                "VC_brood","VC_wnr","VC_e",
                                "phi","pi",
                                # fitness (w)
                                "beta_0_w", "beta_0_w_prob",
                                "b",
                                "b_int",
                                "V_spy_w", 
                                "V_e_w",
                                "VC_spy_w","VC_e_w",
                                "bluetit_effect", "greattit_effect",
                                "estimate1", "estimate2", "estimate3", "estimate4",
                                "sel_bluetit_effect", "sel_greattit_effect",
                                "sel_1", "sel_2", "sel_3","sel_4"
                                ))$summary[,c(1,4,6,8,9,10)],3)))



#rstan::get_seed(md) # model seed
#launch_shinystan(md) # check model output
#dat_diets <- as.data.frame(t(round(summary(md, pars = c("true_diets"))$summary[,c(1,4,6,8,9,10)],3)))


### save output
write.csv(dat_zip, "Output/Models/M_B_Suc_Diet_240925.csv")


################################################################################################# nestling G ----
### data frame for great tits nestlings only (nestling B) for errors-in variables model with diet (z) and fledging success (w)

df1 <- subset(df, df$Species == "G" & df$LifeStage == "C")

df_z <- subset(df1, !is.na(Success)) 

df_w <- df_z %>%
  distinct(BroodID, .keep_all = TRUE)

#################################################################################################### model G ----

dfl<-list(N_z      = nrow(df_z),
          z        = df_z$propp1_G,
          #TMB      = df_z$TreatmentB,
          #TMG      = df_z$TreatmentG,
          #Box      = df_z$DoubleBox,
          #Y2024    = as.integer(df_z$year == "2024"),
          N_B      = length(unique(df_z$BroodID)),
          b_z      = as.integer(factor(df_z$BroodID)),
          #N_spy    = length(unique(df_z$SPY)),
          #spy      = as.integer(factor(df_z$SPY)),
          N_wnr    = length(unique(df_z$WithinNestRep)),
          wnr      = as.integer(factor(df_z$WithinNestRep)),
          ## fitness (w)
          N_w      = nrow(df_w),
          w        = df_w$Success,
          TMB_w    = df_w$TreatmentB,
          TMG_w    = df_w$TreatmentG,
          Box_w    = df_w$Doublebox,
          Y2024_w  = as.integer(df_w$year == "2024"),
          b_w      = as.integer(factor(df_w$BroodID)),
          N_spy_w  = length(unique(df_w$SPY)),
          spy_w    = as.integer(factor(df_w$SPY)))


md <- stan("Code/Models/Mod5.stan", data = dfl, #seed = 2078559131,
           chains = 4, iter = 5000,  warmup = 2000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.99))

saveRDS(md, file = "Output/Models/M_G_Suc_Diet_240925.rds")

dat_zip <- as.data.frame(t(round(summary(md, pars = c("beta_0", "beta_0_prop",
                                                      "V_brood",
                                                      "V_wnr",
                                                      "V_e",
                                                      "VC_brood","VC_wnr","VC_e",
                                                      "phi","pi",
                                                      # fitness (w)
                                                      "beta_0_w", "beta_0_w_prob",
                                                      "b",
                                                      "b_int",
                                                      "V_spy_w", "V_e_w",
                                                      "VC_spy_w","VC_e_w",
                                                      "bluetit_effect", "greattit_effect",
                                                      "estimate1", "estimate2", "estimate3", "estimate4",
                                                      "sel_bluetit_effect", "sel_greattit_effect",
                                                      "sel_1", "sel_2", "sel_3","sel_4"))$summary[,c(1,4,6,8,9,10)],3)))


#rstan::get_seed(md)
#launch_shinystan(md)
#dat_diets <- as.data.frame(t(round(summary(md, pars = c("true_diets"))$summary[,c(1,4,6,8,9,10)],3)))


write.csv(dat_zip, "Output/Models/M_G_Suc_Diet_240925.csv")



