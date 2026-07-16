#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 02#M_availability

### Identification prefered prey in diet: Existence of prey preferences 
### Bayesian STAN models to estimate which arthropod families are overrepresented in the blue and great tit diet,
### compared to the environment


### Accessed last: September 2025
######################################################

### packages and data

library(rstan)
library(dplyr)
library(shinystan)

options(scipen = 999)  # turn off scientific notation

df <- read.csv("Data/Extracted/availabiliy_020925.csv", header=TRUE)


################################################################################################## all birds ----
### model with both species combined (all)

df_w <- df # rename data frame

################################################################################################## model all ----
### prepare order loop

families <- c("RRA_Noctuidae","RRA_Erebidae","RRA_Geometridae","RRA_Tineidae","RRA_Lasiocampidae",
              "RRA_Gelechiidae","RRA_Ypsolophidae","RRA_Tortricidae","RRA_Lycaenidae","RRA_Lypusidae",
              "RRA_Notodontidae","RRA_Psychidae","RRA_Nolidae","RRA_Glyphipterigidae","RRA_Drepanidae",
              "RRA_Pyralidae","RRA_Eriocraniidae","RRA_Argyresthiidae","RRA_Adelidae","RRA_Coleophoridae",
              "RRA_Hesperiidae","RRA_Crambidae", "RRA_Oecophoridae",
              "RRA_Argidae","RRA_Pamphiliidae","RRA_Tenthredinidae",
              "RRA_Chrysomelidae","RRA_Curculionidae") # names detected canopy defoliator families

J <- length(families) # number families

w_mat <- as.matrix(df_w[paste0(families)])  # N_w x J, matrix of relative abundances for the model loop



### prepare data for stan
dfl <- list(J           = J,
            N_w         = nrow(df_w),
            w           = w_mat,
            TypeB       = as.integer(df_w$Types == "B"),
            TypeG       = as.integer(df_w$Types == "G"),
            Y2024       = as.integer(df_w$Year == "2024"),
            Date_w      = as.numeric(scale(df_w$Date, scale = FALSE)),
            N_locyear   = length(unique(df_w$LocYear)),
            locyear_w   = as.integer(factor(df_w$LocYear)),
            N_spy       = length(unique(df_w$SPY)),
            spy_w       = as.integer(factor(df_w$SPY)))


### fit Bayesian model using STAN
md <- stan("Code/Models/Mod2.stan", data = dfl, #seed = 139917900,
           chains = 4, iter = 5000,  warmup = 2000, 
           thin = 1, cores = 4, control = list(adapt_delta = 0.95))

### save output locally
saveRDS(md, file = "Output/Models/M_avail_020925.rds")


families <- c( "Noctuidae","Erebidae","Geometridae","Tineidae","Lasiocampidae",
               "Gelechiidae","Ypsolophidae","Tortricidae","Lycaenidae","Lypusidae",
               "Notodontidae","Psychidae","Nolidae","Glyphipterigidae","Drepanidae",
               "Pyralidae","Eriocraniidae","Argyresthiidae","Adelidae","Coleophoridae",
               "Hesperiidae","Crambidae","Oecophoridae",
               "Argidae","Pamphiliidae","Tenthredinidae",
               "Chrysomelidae","Curculionidae" # names without RRA abbreviation
)

### summarize model loop coefficient names
sum_all <- summary(md,
                   pars = c("beta0", "mean_w", "mean_w_zi",
                            "bw",           
                            "V_spy", "V_locyear", "V_e", 
                            "VC_spy", "VC_locyear", "VC_e", 
                            "phi", "pi")
)$summary

### select columns
sum_sel <- sum_all[, c(1,4,6,8,9,10)]
dat_zip <- as.data.frame(t(round(sum_sel, 3)))

### relabel [j] with family names
for (j in seq_along(families)) {
  dat_zip <- dplyr::rename_with(dat_zip,
                                ~ gsub(paste0("\\[", j, "\\]"), paste0("_", families[j]), .x))
}


#rstan::get_seed(md) # get model seed
#launch_shinystan(md) # check output
#dat_plot <- as.data.frame(round(summary(md)$summary[,c(1,4,6,8,9,10)],3))

## save output
write.csv(dat_zip, "Output/Models/M_avail_020925.csv")







