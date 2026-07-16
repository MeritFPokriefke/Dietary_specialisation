
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 01#M_GDiet_var

### Variation in general diet: Variation in dietary composition 
### Bayesian STAN models to determine variation in dietary composition at multiple hierarchical levels,
### models are looped over orders and applied to all hierarchical levels (female, male, nestling 10, nestling 15)

### Accessed last: October 2025
######################################################


### packages and data

library(rstan)
library(dplyr)
library(shinystan)

options(scipen = 999)  #turn off scientific notation

df <- read.csv("Data/Extracted/birds2_280825.csv", header=TRUE)


################################################################################################## all birds ----
### model with both species combined (all)

df_w <- df #rename data frame

#################################################################################################### repeats ----
### count how many individual repeats

df_w %>%
  count(RingNumber) %>%
  filter(n > 1)

################################################################################################## model all ----
### prepare order loop

orders <- c("Lepidoptera", "Diptera", "Araneae" , "Hymenoptera", "Coleoptera") # names five most abundant orders

J <- length(orders) # number of orders

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
            TypeAM    = as.integer(df_w$Type == "AM"),
            TypeC10   = as.integer(df_w$Type == "chick1"),
            TypeC15   = as.integer(df_w$Type == "chick2"),
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

### fit bayesian model using STAN
md <- stan("Code/Models/Mod1.stan", data = dfl, seed = 2073548014,
           chains = 4, iter = 3000,  warmup = 2000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

### save output locally
saveRDS(md, file = "Output/Models/M_all_diet_var_050925.rds")


### extract model coefficients
sum_all <- summary(md,
                   pars = c("beta_0w", "mean_w", "mean_w_zi",
                            "bw","bw_int",
                            "V_brood_w", "V_wnr_w", "V_spy_w", "V_sp_w",  "V_e_w",
                            "VC_brood_w","VC_wnr_w", "VC_spy_w","VC_sp_w", "VC_e_w",
                            "phi_w", "pi_w",
                            "bluetit_effect", "greattit_effect",
                            "estimate1", "estimate2", "estimate3", "estimate4",
                            "estimateF","estimateM","estimateC10", "estimateC15", "estimateA","estimateC",
                            "diffA", "diffC","diffT"))$summary

sum_sel <- sum_all[, c(1,4,6,8,9,10)]
dat_zip <- as.data.frame(t(round(sum_sel, 3)))

### relabel [j] with order names
for (j in seq_along(orders)) {
  dat_zip <- dplyr::rename_with(dat_zip,
                                ~ gsub(paste0("\\[", j, "\\]"), paste0("_", orders[j]), .x))
}


#rstan::get_seed(md) #get models seed
#launch_shinystan(md) #to check details
#dat_plot <- as.data.frame(round(summary(md)$summary[,c(1,4,6,8,9,10)],3)) #to check details

### save output
write.csv(dat_zip, "Output/Models/M_all_diet_var_050925.csv")



########################################################################################################## B ----
### filter data for blue tits only (B)

df_w <- subset(df, df$Species == 0)

#################################################################################################### Model B ----

orders <- c("Lepidoptera", "Diptera", "Araneae" , "Hymenoptera", "Coleoptera")

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
            TypeAM    = as.integer(df_w$Type == "AM"),
            TypeC10   = as.integer(df_w$Type == "chick1"),
            TypeC15   = as.integer(df_w$Type == "chick2"),
            Date_w    = as.numeric(scale(df_w$AdateSample, scale = FALSE)),
            N_brood_w = length(unique(df_w$BroodID)),
            brood_w   = as.integer(factor(df_w$BroodID)),
            N_wnr_w   = length(unique(df_w$WithinNestRep)),
            wnr_w     = as.integer(factor(df_w$WithinNestRep)),
            N_spy_w   = length(unique(df_w$SPY)),
            spy_w     = as.integer(factor(df_w$SPY)),
            N_sp_w    = length(unique(df_w$Subplot)),
            sp_w      = as.integer(factor(df_w$Subplot)))


md <- stan("Code/Models/Mod1.1.stan", data = dfl, seed = 1342409424,
           chains = 4, iter = 3000,  warmup = 2000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

saveRDS(md, file = "Output/Models/M_B_diet_var_050925.rds")



sum_all <- summary(md,
                   pars = c("beta_0w", "mean_w", "mean_w_zi",
                            "bw","bw_int",
                            "V_brood_w", "V_wnr_w", "V_spy_w", "V_sp_w",  "V_e_w",
                            "VC_brood_w","VC_wnr_w", "VC_spy_w","VC_sp_w", "VC_e_w",
                            "phi_w", "pi_w",
                            "bluetit_effect", "greattit_effect",
                            "estimate1", "estimate2", "estimate3", "estimate4",
                            "estimateF","estimateM","estimateC10", "estimateC15", "estimateA","estimateC",
                            "diffA", "diffC","diffT")
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


write.csv(dat_zip, "Output/Models/M_B_diet_var_050925.csv")

########################################################################################################## G ----
### filter data for great tits only (G)

df_w <- subset(df, df$Species == 1)

#################################################################################################### Model G ----

orders <- c("Lepidoptera", "Diptera", "Araneae" , "Hymenoptera", "Coleoptera")

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
            TypeAM    = as.integer(df_w$Type == "AM"),
            TypeC10   = as.integer(df_w$Type == "chick1"),
            TypeC15   = as.integer(df_w$Type == "chick2"),
            Date_w    = as.numeric(scale(df_w$AdateSample, scale = FALSE)),
            N_brood_w = length(unique(df_w$BroodID)),
            brood_w   = as.integer(factor(df_w$BroodID)),
            N_wnr_w   = length(unique(df_w$WithinNestRep)),
            wnr_w     = as.integer(factor(df_w$WithinNestRep)),
            N_spy_w   = length(unique(df_w$SPY)),
            spy_w     = as.integer(factor(df_w$SPY)),
            N_sp_w    = length(unique(df_w$Subplot)),
            sp_w      = as.integer(factor(df_w$Subplot)))


md <- stan("Code/Models/Mod1.1.stan", data = dfl, seed = 1792763905,
           chains = 4, iter = 3000,  warmup = 2000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

saveRDS(md, file = "Output/Models/M_G_diet_var_050925.rds")



sum_all <- summary(md,
                   pars = c("beta_0w", "mean_w", "mean_w_zi",
                            "bw","bw_int",
                            "V_brood_w", "V_wnr_w", "V_spy_w", "V_sp_w",  "V_e_w",
                            "VC_brood_w","VC_wnr_w", "VC_spy_w","VC_sp_w", "VC_e_w",
                            "phi_w", "pi_w",
                            "bluetit_effect", "greattit_effect",
                            "estimate1", "estimate2", "estimate3", "estimate4",
                            "estimateF","estimateM","estimateC10", "estimateC15", "estimateA","estimateC",
                            "diffA", "diffC","diffT")
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


write.csv(dat_zip, "Output/Models/M_G_diet_var_050925.csv")
