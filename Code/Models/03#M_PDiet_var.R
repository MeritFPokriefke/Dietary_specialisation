
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 03#M_PDiet_var

### Variation in preferred prey: Variation in prey preferences
### Bayesian STAN models to determine variation in prey preferences at multiple hierarchical levels,
### models are applied to all hierarchical levels (female, male, nestling 10, nestling 15)

### Accessed last: September 2025
######################################################


### packages and data

library(rstan)
library(dplyr)
library(shinystan)

options(scipen = 999)  # turn off scientific notation

df <- read.csv("Data/Extracted/birds4_030925.csv", header=TRUE)

######################################################################################################## all ----
### model with both species combined (all)

df_w <- df[!is.na(df$propp1_all), ] # 25 obs with no estimate excluded

df_w$Species[df_w$Species == "B"] <- 0 # change B to 0 so intercept is blue tit value
df_w$Species[df_w$Species == "G"] <- 1 

################################################################################################## model all ----
### prep proportion loop (in case more than one proportion then loop is already in place)

proportion <- c("propp1_all") # proportion for both species combined

J <- length(proportion) # number proportions

w_mat <- as.matrix(df_w[paste0(proportion)])  # N_w x J, matrix 

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
            Species   = as.numeric(df_w$Species),
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
md <- stan("Code/Models/Mod3.stan", data = dfl, seed = 430274149,
           chains = 4, iter = 5000,  warmup = 2000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

### save output locally
saveRDS(md, file = "Output/Models/M_All_pdiet_var_040925.rds")


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
                            "diffA", "diffC","diffT")
)$summary

sum_sel <- sum_all[, c(1,4,6,8,9,10)]
dat_zip <- as.data.frame(t(round(sum_sel, 3)))

# relabel [j] with order names
for (j in seq_along(proportion )) {
  dat_zip <- dplyr::rename_with(dat_zip,
                                ~ gsub(paste0("\\[", j, "\\]"), paste0("_", proportion [j]), .x))
}




#rstan::get_seed(md) # model seed
#launch_shinystan(md) # check model output
#dat_plot <- as.data.frame(round(summary(md)$summary[,c(1,4,6,8,9,10)],3))

### save output
write.csv(dat_zip, "Output/Models/M_All_pdiet_var_040925.csv")


########################################################################################################## B ----
### filter data for blue tits only (B)

df_w <- subset(df, df$Species == "B")

df_w <- df_w[!is.na(df_w$propp1_B), ] # 5 obs with no estimates excluded

#################################################################################################### model B ----

proportion <- c("propp1_B")

J <- length(proportion)

w_mat <- as.matrix(df_w[paste0(proportion)])  


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


md <- stan("Code/Models/Mod3.1.stan", data = dfl, seed = 1776842595,
           chains = 4, iter = 5000,  warmup = 2000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

saveRDS(md, file = "Output/Models/M_B_pdiet_var_040925.rds")



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


for (j in seq_along(proportion )) {
  dat_zip <- dplyr::rename_with(dat_zip,
                                ~ gsub(paste0("\\[", j, "\\]"), paste0("_", proportion [j]), .x))
}




#rstan::get_seed(md)
#launch_shinystan(md) 
#dat_plot <- as.data.frame(round(summary(md)$summary[,c(1,4,6,8,9,10)],3))


write.csv(dat_zip, "Output/Models/M_B_pdiet_var_040925.csv")


########################################################################################################## G ----
### filter data for great tits only (G)

df_w <- subset(df, df$Species == "G")

df_w <- df_w[!is.na(df_w$propp1_G), ] # 20 obs with no estimate excluded

#################################################################################################### model G ----

proportion <- c("propp1_G")

J <- length(proportion)

w_mat <- as.matrix(df_w[paste0(proportion)])


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


md <- stan("Code/Models/Mod3.1.stan", data = dfl, seed = 137344816,
           chains = 4, iter = 4000,  warmup = 2000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

saveRDS(md, file = "Output/Models/M_G_pdiet_var_040925.rds")



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


for (j in seq_along(proportion )) {
  dat_zip <- dplyr::rename_with(dat_zip,
                                ~ gsub(paste0("\\[", j, "\\]"), paste0("_", proportion[j]), .x))
}


#rstan::get_seed(md)
#launch_shinystan(md) 
#dat_plot <- as.data.frame(round(summary(md)$summary[,c(1,4,6,8,9,10)],3))


write.csv(dat_zip, "Output/Models/M_G_pdiet_var_040925.csv")
