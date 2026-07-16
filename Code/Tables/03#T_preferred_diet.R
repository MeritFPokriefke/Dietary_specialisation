
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 03#T_preferred_diet

### Variation in preferred prey: Variation in prey preferences
### Latex ready tables for models 03_PDiet

### Accessed last: September 2025
######################################################

### packages and data

library(dplyr)
library(tidyr)


### read in model outputs per bird type
### 4 bird types (female, male, nestling 10, nestling 15)
m1 <- read.csv("Output/Models/M_All_pdiet_var_040925.csv", header=TRUE)
m2 <- read.csv("Output/Models/M_B_pdiet_var_040925.csv", header=TRUE)
m3 <- read.csv("Output/Models/M_G_pdiet_var_040925.csv", header=TRUE)


### 2 bird types (adult, nestling)
m4 <- read.csv("Output/Models/M2_All_pdiet_var_040925.csv", header=TRUE)
m5 <- read.csv("Output/Models/M2_B_pdiet_var_040925.csv", header=TRUE)
m6 <- read.csv("Output/Models/M2_G_pdiet_var_040925.csv", header=TRUE)


################################################################################################ annotations ----

### ALL: both species
### B: blue tits
### G: great tits

######################################################################################################## ALL ----
### select rows 2 to 4
m1 <- m1[c(2:4),]

### transpose matrix
m1_transposed <- t(m1)

colnames(m1_transposed) <- m1_transposed[1, ]# take the first row as column names

m1_transposed <- m1_transposed[-1, ] # remove the first row (which is now the column names)

### convert to data frame
m1_df <- as.data.frame(m1_transposed)

m1_df$rownames_column <- rownames(m1_df) # make row names into a column

m1_df <- m1_df %>%
  mutate(across(c(`2.5%`, `50%`, `97.5%`), ~ format(round(as.numeric(.), 3), nsmall = 3))) # concerts to numeric then rounds to 3rd decimal

### create formatted estimate string
m1_column <- m1_df %>%
  mutate(EstimateH = paste(`50%`, "[", `2.5%`, ";", `97.5%`, "]", sep = " ")) %>%
  select(EstimateH, rownames_column)

### rearrange df

### define row order for estimates
estimate_w_rows <- c("mean_w_propp1_all",
                     "bw.1.1.",
                     "bw.1.2.",
                     "bw.1.3.",
                     "bw.1.4.",
                     "bw.1.5.",
                     "bw.1.6.",
                     "bw.1.7.",
                     "bw.1.8.",
                     "bw.1.9.",
                     "bw.1.10.",
                     "bw_int.1.1.",
                     "V_brood_w_propp1_all", "V_sp_w_propp1_all", "V_spy_w_propp1_all",  "V_wnr_w_propp1_all",  "V_e_w_propp1_all",
                     "VC_brood_w_propp1_all", "VC_sp_w_propp1_all", "VC_spy_w_propp1_all", "VC_wnr_w_propp1_all", "VC_e_w_propp1_all",
                     "phi_w_propp1_all", "pi_w_propp1_all",
                     "bluetit_effect_propp1_all", "greattit_effect_propp1_all",
                     "estimate1_propp1_all", "estimate2_propp1_all", "estimate3_propp1_all","estimate4_propp1_all",
                     "estimateF_propp1_all","estimateM_propp1_all","estimateC10_propp1_all", "estimateC15_propp1_all",
                     "estimateA_propp1_all","estimateC_propp1_all",
                     "diffA_propp1_all", "diffC_propp1_all","diffT_propp1_all")

### subset and order rows
# W1 - Lepidoptera
m1_combined1 <- m1_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

### define latex labels
m1_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Size subplot}",
                              "\\text{Doublebox}",
                              "\\text{Male}",
                              "\\text{Chick10}",
                              "\\text{Chick15}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{Great tit}",
                              "\\text{TMB x TMG}",
                              "\\text{Brood}", "\\text{SubPlot}","\\text{SubPlotYear}", "\\text{Within nest box}", "\\text{Residual}",
                              "\\text{Brood VC}", "\\text{SubPlot VC}", "\\text{SubPlotYear VC}", "\\text{Within nest box VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\\text{B}_\\text{L}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{L}:\\text{G}_\\text{H}$",
                              "$\\text{B}_\\text{H}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{H}:\\text{G}_\\text{H}$",
                              "\\text{Female}", "\\text{Male}", "\\text{Chick 10}", "\\text{Chick 15}",
                              "\\text{Adult}", "\\text{Chick}",
                              "\\text{Diff adults}", "\\text{Diff chicks}",  "\\text{Diff life stages}")

### combine labels with estimates
m1_combined1$Latex_format <- paste(
  m1_combined1$Latex_names,  # latex names column
  " & ",                    
  m1_combined1$EstimateW1,
  " \\"
)

### keep only formatted column
m1_Latex <- m1_combined1 %>%
  select(Latex_format)

### remove rownames
rownames(m1_Latex) <- NULL

### print latex table
print(m1_Latex, quote = FALSE, row.names = FALSE)


########################################################################################################## B ----
m2 <- m2[c(2:4),]

m2_transposed <- t(m2)


colnames(m2_transposed) <- m2_transposed[1, ]# take the first row as column names
m2_transposed <- m2_transposed[-1, ] # remove the first row (which is now the column names)

m2_df <- as.data.frame(m2_transposed)

m2_df$rownames_column <- rownames(m2_df) # make row names into a column

m2_df <- m2_df %>%
  mutate(across(c(`2.5%`, `50%`, `97.5%`), ~ format(round(as.numeric(.), 3), nsmall = 3))) # concerts to numeric then rounds to 3rd decimal

m2_column <- m2_df %>%
  mutate(EstimateH = paste(`50%`, "[", `2.5%`, ";", `97.5%`, "]", sep = " ")) %>%
  select(EstimateH, rownames_column)

### rearrange df

estimate_w_rows <- c("mean_w_propp1_B",
                     "bw.1.1.",
                     "bw.1.2.",
                     "bw.1.3.",
                     "bw.1.4.",
                     "bw.1.5.",
                     "bw.1.6.",
                     "bw.1.7.",
                     "bw.1.8.",
                     "bw.1.9.",
                     "bw_int.1.1.",
                     "V_brood_w_propp1_B", "V_sp_w_propp1_B", "V_spy_w_propp1_B",  "V_wnr_w_propp1_B",  "V_e_w_propp1_B",
                     "VC_brood_w_propp1_B", "VC_sp_w_propp1_B", "VC_spy_w_propp1_B", "VC_wnr_w_propp1_B", "VC_e_w_propp1_B",
                     "phi_w_propp1_B", "pi_w_propp1_B",
                     "bluetit_effect_propp1_B", "greattit_effect_propp1_B",
                     "estimate1_propp1_B", "estimate2_propp1_B", "estimate3_propp1_B","estimate4_propp1_B",
                     "estimateF_propp1_B","estimateM_propp1_B","estimateC10_propp1_B", "estimateC15_propp1_B",
                     "estimateA_propp1_B","estimateC_propp1_B",
                     "diffA_propp1_B", "diffC_propp1_B","diffT_propp1_B")



# W1 - Lepidoptera
m2_combined1 <- m2_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)


m2_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Size subplot}",
                              "\\text{Doublebox}",
                              "\\text{Male}",
                              "\\text{Chick10}",
                              "\\text{Chick15}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{TMB x TMG}",
                              "\\text{Brood}", "\\text{SubPlot}","\\text{SubPlotYear}", "\\text{Within nest box}", "\\text{Residual}",
                              "\\text{Brood VC}", "\\text{SubPlot VC}", "\\text{SubPlotYear VC}", "\\text{Within nest box VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$",
                              "\\text{Female}", "\\text{Male}", "\\text{Chick 10}", "\\text{Chick 15}",
                              "\\text{Adult}", "\\text{Chick}",
                              "\\text{Diff adults}", "\\text{Diff chicks}",  "\\text{Diff life stages}")



m2_combined1$Latex_format <- paste(
  m2_combined1$Latex_names,  # Latex names column
  " & ",                    
  m2_combined1$EstimateW1,
  " \\"
)




m2_Latex <- m2_combined1 %>%
  select(Latex_format)
rownames(m2_Latex) <- NULL
print(m2_Latex, quote = FALSE, row.names = FALSE)


########################################################################################################## G ----
m3 <- m3[c(2:4),]

m3_transposed <- t(m3)


colnames(m3_transposed) <- m3_transposed[1, ]# take the first row as column names
m3_transposed <- m3_transposed[-1, ] # remove the first row (which is now the column names)

m3_df <- as.data.frame(m3_transposed)

m3_df$rownames_column <- rownames(m3_df) # make row names into a column

m3_df <- m3_df %>%
  mutate(across(c(`2.5%`, `50%`, `97.5%`), ~ format(round(as.numeric(.), 3), nsmall = 3))) # concerts to numeric then rounds to 3rd decimal

m3_column <- m3_df %>%
  mutate(EstimateH = paste(`50%`, "[", `2.5%`, ";", `97.5%`, "]", sep = " ")) %>%
  select(EstimateH, rownames_column)

### rearrange df


estimate_w_rows <- c("mean_w_propp1_G",
                     "bw.1.1.",
                     "bw.1.2.",
                     "bw.1.3.",
                     "bw.1.4.",
                     "bw.1.5.",
                     "bw.1.6.",
                     "bw.1.7.",
                     "bw.1.8.",
                     "bw.1.9.",
                     "bw_int.1.1.",
                     "V_brood_w_propp1_G", "V_sp_w_propp1_G", "V_spy_w_propp1_G",  "V_wnr_w_propp1_G",  "V_e_w_propp1_G",
                     "VC_brood_w_propp1_G", "VC_sp_w_propp1_G", "VC_spy_w_propp1_G", "VC_wnr_w_propp1_G", "VC_e_w_propp1_G",
                     "phi_w_propp1_G", "pi_w_propp1_G",
                     "bluetit_effect_propp1_G", "greattit_effect_propp1_G",
                     "estimate1_propp1_G", "estimate2_propp1_G", "estimate3_propp1_G","estimate4_propp1_G",
                     "estimateF_propp1_G","estimateM_propp1_G","estimateC10_propp1_G", "estimateC15_propp1_G",
                     "estimateA_propp1_G","estimateC_propp1_G",
                     "diffA_propp1_G", "diffC_propp1_G","diffT_propp1_G")




# W1 - Lepidoptera
m3_combined1 <- m3_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)



m3_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Size subplot}",
                              "\\text{Doublebox}",
                              "\\text{Male}",
                              "\\text{Chick10}",
                              "\\text{Chick15}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{TMB x TMG}",
                              "\\text{Brood}", "\\text{SubPlot}","\\text{SubPlotYear}", "\\text{Within nest box}", "\\text{Residual}",
                              "\\text{Brood VC}", "\\text{SubPlot VC}", "\\text{SubPlotYear VC}", "\\text{Within nest box VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$",
                              "\\text{Female}", "\\text{Male}", "\\text{Chick 10}", "\\text{Chick 15}",
                              "\\text{Adult}", "\\text{Chick}",
                              "\\text{Diff adults}", "\\text{Diff chicks}",  "\\text{Diff life stages}")



m3_combined1$Latex_format <- paste(
  m3_combined1$Latex_names,  # Latex names column
  " & ",                    
  m3_combined1$EstimateW1,
  " \\"
)



m3_Latex <- m3_combined1 %>%
  select(Latex_format)
rownames(m3_Latex) <- NULL
print(m3_Latex, quote = FALSE, row.names = FALSE)

####################################################################################################### ALL2 ----
m4 <- m4[c(2:4),]

m4_transposed <- t(m4)


colnames(m4_transposed) <- m4_transposed[1, ]# take the first row as column names
m4_transposed <- m4_transposed[-1, ] # remove the first row (which is now the column names)

m4_df <- as.data.frame(m4_transposed)

m4_df$rownames_column <- rownames(m4_df) # make row names into a column

m4_df <- m4_df %>%
  mutate(across(c(`2.5%`, `50%`, `97.5%`), ~ format(round(as.numeric(.), 3), nsmall = 3))) # concerts to numeric then rounds to 3rd decimal

m4_column <- m4_df %>%
  mutate(EstimateH = paste(`50%`, "[", `2.5%`, ";", `97.5%`, "]", sep = " ")) %>%
  select(EstimateH, rownames_column)

### rearrange df



estimate_w_rows <- c("mean_w_propp1_all",
                     "bw.1.1.",
                     "bw.1.2.",
                     "bw.1.3.",
                     "bw.1.4.",
                     "bw.1.5.",
                     "bw.1.6.",
                     "bw.1.7.",
                     "bw.1.8.",
                     "bw_int.1.1.",
                     "V_brood_w_propp1_all", "V_sp_w_propp1_all", "V_spy_w_propp1_all",  "V_wnr_w_propp1_all",  "V_e_w_propp1_all",
                     "VC_brood_w_propp1_all", "VC_sp_w_propp1_all", "VC_spy_w_propp1_all", "VC_wnr_w_propp1_all", "VC_e_w_propp1_all",
                     "phi_w_propp1_all", "pi_w_propp1_all",
                     "bluetit_effect_propp1_all", "greattit_effect_propp1_all",
                     "estimate1_propp1_all", "estimate2_propp1_all", "estimate3_propp1_all","estimate4_propp1_all")



# W1 - Lepidoptera
m4_combined1 <- m4_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)


m4_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Size subplot}",
                              "\\text{Doublebox}",
                              "\\text{Chick}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{Great tit}",
                              "\\text{TMB x TMG}",
                              "\\text{Brood}", "\\text{SubPlot}","\\text{SubPlotYear}", "\\text{Within nest box}", "\\text{Residual}",
                              "\\text{Brood VC}", "\\text{SubPlot VC}", "\\text{SubPlotYear VC}", "\\text{Within nest box VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$")



m4_combined1$Latex_format <- paste(
  m4_combined1$Latex_names,  # Latex names column
  " & ",                    
  m4_combined1$EstimateW1,
  " \\"
)




m4_Latex <- m4_combined1 %>%
  select(Latex_format)
rownames(m4_Latex) <- NULL
print(m4_Latex, quote = FALSE, row.names = FALSE)



######################################################################################################### B2 ----
m5 <- m5[c(2:4),]

m5_transposed <- t(m5)


colnames(m5_transposed) <- m5_transposed[1, ]# take the first row as column names
m5_transposed <- m5_transposed[-1, ] # remove the first row (which is now the column names)

m5_df <- as.data.frame(m5_transposed)

m5_df$rownames_column <- rownames(m5_df) # make row names into a column

m5_df <- m5_df %>%
  mutate(across(c(`2.5%`, `50%`, `97.5%`), ~ format(round(as.numeric(.), 3), nsmall = 3))) # concerts to numeric then rounds to 3rd decimal

m5_column <- m5_df %>%
  mutate(EstimateH = paste(`50%`, "[", `2.5%`, ";", `97.5%`, "]", sep = " ")) %>%
  select(EstimateH, rownames_column)

### rearrange df



estimate_w_rows <- c("mean_w_propp1_B",
                     "bw.1.1.",
                     "bw.1.2.",
                     "bw.1.3.",
                     "bw.1.4.",
                     "bw.1.5.",
                     "bw.1.6.",
                     "bw.1.7.",
                     "bw_int.1.1.",
                     "V_brood_w_propp1_B", "V_sp_w_propp1_B", "V_spy_w_propp1_B",  "V_wnr_w_propp1_B",  "V_e_w_propp1_B",
                     "VC_brood_w_propp1_B", "VC_sp_w_propp1_B", "VC_spy_w_propp1_B", "VC_wnr_w_propp1_B", "VC_e_w_propp1_B",
                     "phi_w_propp1_B", "pi_w_propp1_B",
                     "bluetit_effect_propp1_B", "greattit_effect_propp1_B",
                     "estimate1_propp1_B", "estimate2_propp1_B", "estimate3_propp1_B","estimate4_propp1_B")



# W1 - Lepidoptera
m5_combined1 <- m5_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)


m5_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Size subplot}",
                              "\\text{Doublebox}",
                              "\\text{Chick}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{TMB x TMG}",
                              "\\text{Brood}", "\\text{SubPlot}","\\text{SubPlotYear}", "\\text{Within nest box}", "\\text{Residual}",
                              "\\text{Brood VC}", "\\text{SubPlot VC}", "\\text{SubPlotYear VC}", "\\text{Within nest box VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$")



m5_combined1$Latex_format <- paste(
  m5_combined1$Latex_names,  # Latex names column
  " & ",                    
  m5_combined1$EstimateW1,
  " \\"
)




m5_Latex <- m5_combined1 %>%
  select(Latex_format)
rownames(m5_Latex) <- NULL
print(m5_Latex, quote = FALSE, row.names = FALSE)


######################################################################################################### G2 ----
m6 <- m6[c(2:4),]

m6_transposed <- t(m6)


colnames(m6_transposed) <- m6_transposed[1, ]# take the first row as column names
m6_transposed <- m6_transposed[-1, ] # remove the first row (which is now the column names)

m6_df <- as.data.frame(m6_transposed)

m6_df$rownames_column <- rownames(m6_df) # make row names into a column

m6_df <- m6_df %>%
  mutate(across(c(`2.5%`, `50%`, `97.5%`), ~ format(round(as.numeric(.), 3), nsmall = 3))) # concerts to numeric then rounds to 3rd decimal

m6_column <- m6_df %>%
  mutate(EstimateH = paste(`50%`, "[", `2.5%`, ";", `97.5%`, "]", sep = " ")) %>%
  select(EstimateH, rownames_column)

### rearrange df


estimate_w_rows <- c("mean_w_propp1_G",
                     "bw.1.1.",
                     "bw.1.2.",
                     "bw.1.3.",
                     "bw.1.4.",
                     "bw.1.5.",
                     "bw.1.6.",
                     "bw.1.7.",
                     "bw_int.1.1.",
                     "V_brood_w_propp1_G", "V_sp_w_propp1_G", "V_spy_w_propp1_G",  "V_wnr_w_propp1_G",  "V_e_w_propp1_G",
                     "VC_brood_w_propp1_G", "VC_sp_w_propp1_G", "VC_spy_w_propp1_G", "VC_wnr_w_propp1_G", "VC_e_w_propp1_G",
                     "phi_w_propp1_G", "pi_w_propp1_G",
                     "bluetit_effect_propp1_G", "greattit_effect_propp1_G",
                     "estimate1_propp1_G", "estimate2_propp1_G", "estimate3_propp1_G","estimate4_propp1_G")




# W1 - Lepidoptera
m6_combined1 <- m6_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)



m6_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Size subplot}",
                              "\\text{Doublebox}",
                              "\\text{Chick}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{TMB x TMG}",
                              "\\text{Brood}", "\\text{SubPlot}","\\text{SubPlotYear}", "\\text{Within nest box}", "\\text{Residual}",
                              "\\text{Brood VC}", "\\text{SubPlot VC}", "\\text{SubPlotYear VC}", "\\text{Within nest box VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$")



m6_combined1$Latex_format <- paste(
  m6_combined1$Latex_names,  # Latex names column
  " & ",                    
  m6_combined1$EstimateW1,
  " \\"
)



m6_Latex <- m6_combined1 %>%
  select(Latex_format)
rownames(m6_Latex) <- NULL
print(m6_Latex, quote = FALSE, row.names = FALSE)
