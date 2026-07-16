#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 01#M3_GDiet_var

### Variation in general diet: Variation in dietary composition 
### Refitted Bayesian STAN models to determine variation in dietary composition at multiple hierarchical levels,
### models are refitted for orders based on differences among bird types (adult, nestling 10, nestling 15)

### Accessed last: September 2025
######################################################

### packages and data

library(rstan)
library(dplyr)
library(shinystan)

options(scipen = 999)  # turn off scientific notation

df <- read.csv("Data/Extracted/birds2_280825.csv", header=TRUE)

############################################################################################## A & C10 & C15 ----
### model with bird type of adult, nestling 10 and nestling 15 for both species (A,C10, C15; all)

df <- df %>%
  mutate(LifeStage = case_when(
    Type %in% c("AF", "AM")   ~ "A",
    Type %in% c("chick1") ~ "C1", 
    Type %in% c("chick2") ~ "C2",
    TRUE ~ NA_character_
    )) 

################################################################################################## all birds ----

df_w <- df # rename data frame

################################################################################################## model all ----
### prepare order loop

orders <- c("Hymenoptera") # orders with bird type differences in adult, nestling 10 and nestling 15

J <- length(orders) # number orders

w_mat <- as.matrix(df_w[paste0("RRA_", orders)])  # N_w x J, matrix of relative abundances for the model loop

### prepare data for stan
dfl <- list(J         = J,
            N_w       = nrow(df_w),
            w         = w_mat,
            TMB       = df_w$TreatmentB,
            TMG       = df_w$TreatmentG,
            Y2024     = as.integer(df_w$year == "2024"),
            Size      = as.numeric(scale(df_w$Ha, scale = FALSE)),
            Box       = df_w$Doublebox,
            LSC1      = as.integer(df_w$LifeStage == "C1"),
            LSC2      = as.integer(df_w$LifeStage == "C2"),
            Species   = df_w$Species,
            Date_w    = as.numeric(scale(df_w$AdateSample, scale = FALSE)),
            N_brood_w = length(unique(df_w$BroodID)),
            brood_w   = as.integer(factor(df_w$BroodID)),
            N_wnr_w   = length(unique(df_w$WithinNestRep)),
            wnr_w     = as.integer(factor(df_w$WithinNestRep)),
            N_spy_w   = length(unique(df_w$SPY)),
            spy_w     = as.integer(factor(df_w$SPY)),
            N_sp_w    = length(unique(df_w$Subplot)),
            sp_w      = as.integer(factor(df_w$Subplot)))


### fit Bayesian model using STAN
md <- stan("Code/Models/Mod1.3.stan", data = dfl, seed = 1537465866,
           chains = 4, iter = 5000,  warmup = 2000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

### save output locally
saveRDS(md, file = "Output/Models/M3_all_diet_var_050925.rds")


### extract model coefficients
sum_all <- summary(md,
                   pars = c("beta_0w", "mean_w", "mean_w_zi",
                            "bw","bw_int",
                            "V_brood_w", "V_wnr_w", "V_spy_w", "V_sp_w",  "V_e_w",
                            "VC_brood_w","VC_wnr_w", "VC_spy_w","VC_sp_w", "VC_e_w",
                            "phi_w", "pi_w",
                            "bluetit_effect", "greattit_effect",
                            "estimate1", "estimate2", "estimate3", "estimate4")
)$summary

sum_sel <- sum_all[, c(1,4,6,8,9,10)]
dat_zip <- as.data.frame(t(round(sum_sel, 3)))

# relabel [j] with order names
for (j in seq_along(orders)) {
  dat_zip <- dplyr::rename_with(dat_zip,
                                ~ gsub(paste0("\\[", j, "\\]"), paste0("_", orders[j]), .x))
}


#rstan::get_seed(md) # get model seed
#launch_shinystan(md) # check output
#dat_plot <- as.data.frame(round(summary(md)$summary[,c(1,4,6,8,9,10)],3))

## save output
write.csv(dat_zip, "Output/Models/M3_all_diet_var_050925.csv")



########################################################################################################## B ----
### filter data for blue tits only (B)

df_w <- subset(df, df$Species == 0)

#################################################################################################### model B ----

orders <- c("Lepidoptera", "Hymenoptera") # orders with bird type differences in adult, nestling 10 and nestling 15

J <- length(orders)

w_mat <- as.matrix(df_w[paste0("RRA_", orders)])


dfl <- list(J         = J,
            N_w       = nrow(df_w),
            w         = w_mat,
            TMB       = df_w$TreatmentB,
            TMG       = df_w$TreatmentG,
            Y2024     = as.integer(df_w$year == "2024"),
            Size      = as.numeric(scale(df_w$Ha, scale = FALSE)),
            Box       = df_w$Doublebox,
            LSC1      = as.integer(df_w$LifeStage == "C1"),
            LSC2      = as.integer(df_w$LifeStage == "C2"),
            Date_w    = as.numeric(scale(df_w$AdateSample, scale = FALSE)),
            N_brood_w = length(unique(df_w$BroodID)),
            brood_w   = as.integer(factor(df_w$BroodID)),
            N_wnr_w   = length(unique(df_w$WithinNestRep)),
            wnr_w     = as.integer(factor(df_w$WithinNestRep)),
            N_spy_w   = length(unique(df_w$SPY)),
            spy_w     = as.integer(factor(df_w$SPY)),
            N_sp_w    = length(unique(df_w$Subplot)),
            sp_w      = as.integer(factor(df_w$Subplot)))


md <- stan("Code/Models/Mod1.1.3.stan", data = dfl, seed = 972254738,
           chains = 4, iter = 3000,  warmup = 2000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

saveRDS(md, file = "Output/Models/M3_B_diet_var_050925.rds")



sum_all <- summary(md,
                   pars = c("beta_0w", "mean_w", "mean_w_zi",
                            "bw","bw_int",
                            "V_brood_w", "V_wnr_w", "V_spy_w", "V_sp_w",  "V_e_w",
                            "VC_brood_w","VC_wnr_w", "VC_spy_w","VC_sp_w", "VC_e_w",
                            "phi_w", "pi_w",
                            "bluetit_effect", "greattit_effect",
                            "estimate1", "estimate2", "estimate3", "estimate4")
)$summary

sum_sel <- sum_all[, c(1,4,6,8,9,10)]
dat_zip <- as.data.frame(t(round(sum_sel, 3)))


for (j in seq_along(orders)) {
  dat_zip <- dplyr::rename_with(dat_zip,
                                ~ gsub(paste0("\\[", j, "\\]"), paste0("_", orders[j]), .x))
}


#rstan::get_seed(md)
#launch_shinystan(md) 
#dat_plot <- as.data.frame(round(summary(md)$summary[,c(1,4,6,8,9,10)],3))


write.csv(dat_zip, "Output/Models/M3_B_diet_var_050925.csv")


