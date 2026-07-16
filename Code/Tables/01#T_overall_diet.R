
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 01#T_overall_diet

### Variation in general diet: Variation in dietary composition 
### Latex ready tables for models 01_GDiet

### Accessed last: September 2025
######################################################

### packages and data

library(dplyr)
library(tidyr)

### read in model outputs per bird type
### 4 bird types (female, male, nestling 10, nestling 15)
m1 <- read.csv("Output/Models/M_all_diet_var_050925.csv", header=TRUE)
m2 <- read.csv("Output/Models/M_B_diet_var_050925.csv", header=TRUE)
m3 <- read.csv("Output/Models/M_G_diet_var_050925.csv", header=TRUE)

### 2 bird types (adult, nestling)
m4 <- read.csv("Output/Models/M2_all_diet_var_290825.csv", header=TRUE)
m5 <- read.csv("Output/Models/M2_B_diet_var_290825.csv", header=TRUE)
m6 <- read.csv("Output/Models/M2_G_diet_var_290825.csv", header=TRUE)

### 3 bird types (adult, nestling 10, nestling 15)
m7 <- read.csv("Output/Models/M3_all_diet_var_050925.csv", header=TRUE)
m8 <- read.csv("Output/Models/M3_B_diet_var_050925.csv", header=TRUE)

### 1 bird type (no type differences)
m9 <- read.csv("Output/Models/M4_all_diet_var_090925.csv", header=TRUE)
m10 <- read.csv("Output/Models/M4_G_diet_var_090925.csv", header=TRUE)

################################################################################################ annotations ----

### ALL: both species
### B: blue tits
### G: great tits

####################################################################################################### ALL4 ----
### select only rows 2 to 4 from m1
m1 <- m1[c(2:4),]

### transpose the matrix (rows become columns)
m1_transposed <- t(m1)

### set first row as column names
colnames(m1_transposed) <- m1_transposed[1, ]# take the first row as column names

### remove first row (now used as column names)
m1_transposed <- m1_transposed[-1, ] # remove the first row (which is now the column names)

### convert to data frame
m1_df <- as.data.frame(m1_transposed)

### move rownames into a new column
m1_df$rownames_column <- rownames(m1_df) # make row names into a column

### convert to numeric and round values to 3 decimals
m1_df <- m1_df %>%
  mutate(across(c(`2.5%`, `50%`, `97.5%`), ~ format(round(as.numeric(.), 3), nsmall = 3))) # concerts to numeric then rounds to 3rd decimal

### combine median and interval into one formatted string
m1_column <- m1_df %>%
  mutate(EstimateH = paste(`50%`, "[", `2.5%`, ";", `97.5%`, "]", sep = " ")) %>%
  select(EstimateH, rownames_column)

### rearrange df
### define row order for lepidoptera estimates
estimate_w_rows <- c("mean_w_Lepidoptera",
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
                     "V_brood_w_Lepidoptera", "V_sp_w_Lepidoptera", "V_spy_w_Lepidoptera",  "V_wnr_w_Lepidoptera",  "V_e_w_Lepidoptera",
                     "VC_brood_w_Lepidoptera", "VC_sp_w_Lepidoptera", "VC_spy_w_Lepidoptera", "VC_wnr_w_Lepidoptera", "VC_e_w_Lepidoptera",
                     "phi_w_Lepidoptera", "pi_w_Lepidoptera",
                     "bluetit_effect_Lepidoptera", "greattit_effect_Lepidoptera",
                     "estimate1_Lepidoptera", "estimate2_Lepidoptera", "estimate3_Lepidoptera","estimate4_Lepidoptera",
                     "estimateF_Lepidoptera","estimateM_Lepidoptera","estimateC10_Lepidoptera", "estimateC15_Lepidoptera",
                     "estimateA_Lepidoptera","estimateC_Lepidoptera",
                     "diffA_Lepidoptera", "diffC_Lepidoptera","diffT_Lepidoptera")

### define row order for diptera estimates
estimate_w2_rows <- c("mean_w_Diptera",
                      "bw.2.1.",
                      "bw.2.2.",
                      "bw.2.3.",
                      "bw.2.4.",
                      "bw.2.5.",
                      "bw.2.6.",
                      "bw.2.7.",
                      "bw.2.8.",
                      "bw.2.9.",
                      "bw.2.10.",
                      "bw_int.2.1.",
                      "V_brood_w_Diptera","V_sp_w_Diptera", "V_spy_w_Diptera", "V_wnr_w_Diptera", "V_e_w_Diptera",
                      "VC_brood_w_Diptera", "VC_sp_w_Diptera", "VC_spy_w_Diptera", "VC_wnr_w_Diptera", "VC_e_w_Diptera",
                      "phi_w_Diptera", "pi_w_Diptera",
                      "bluetit_effect_Diptera", "greattit_effect_Diptera",
                      "estimate1_Diptera", "estimate2_Diptera", "estimate3_Diptera","estimate4_Diptera",
                      "estimateF_Diptera","estimateM_Diptera","estimateC10_Diptera", "estimateC15_Diptera",
                      "estimateA_Diptera","estimateC_Diptera",
                      "diffA_Diptera", "diffC_Diptera","diffT_Diptera")

### define row order for araneae estimates
estimate_w3_rows <- c("mean_w_Araneae",
                      "bw.3.1.",
                      "bw.3.2.",
                      "bw.3.3.",
                      "bw.3.4.",
                      "bw.3.5.",
                      "bw.3.6.",
                      "bw.3.7.",
                      "bw.3.8.",
                      "bw.3.9.",
                      "bw.3.10.",
                      "bw_int.3.1.",
                      "V_brood_w_Araneae", "V_sp_w_Araneae", "V_spy_w_Araneae", "V_wnr_w_Araneae", "V_e_w_Araneae",
                      "VC_brood_w_Araneae", "VC_sp_w_Araneae",  "VC_spy_w_Araneae", "VC_wnr_w_Araneae","VC_e_w_Araneae",
                      "phi_w_Araneae", "pi_w_Araneae",
                      "bluetit_effect_Araneae", "greattit_effect_Araneae",
                      "estimate1_Araneae", "estimate2_Araneae", "estimate3_Araneae","estimate4_Araneae",
                      "estimateF_Araneae","estimateM_Araneae","estimateC10_Araneae", "estimateC15_Araneae",
                      "estimateA_Araneae","estimateC_Araneae",
                      "diffA_Araneae", "diffC_Araneae","diffT_Araneae")

### define row order for hymenoptera estimates
estimate_w4_rows <- c("mean_w_Hymenoptera",
                      "bw.4.1.",
                      "bw.4.2.",
                      "bw.4.3.",
                      "bw.4.4.",
                      "bw.4.5.",
                      "bw.4.6.",
                      "bw.4.7.",
                      "bw.4.8.",
                      "bw.4.9.",
                      "bw.4.10.",
                      "bw_int.4.1.",
                      "V_brood_w_Hymenoptera", "V_sp_w_Hymenoptera", "V_spy_w_Hymenoptera", "V_wnr_w_Hymenoptera", "V_e_w_Hymenoptera",
                      "VC_brood_w_Hymenoptera","VC_sp_w_Hymenoptera",  "VC_spy_w_Hymenoptera", "VC_wnr_w_Hymenoptera", "VC_e_w_Hymenoptera",
                      "phi_w_Hymenoptera", "pi_w_Hymenoptera",
                      "bluetit_effect_Hymenoptera", "greattit_effect_Hymenoptera",
                      "estimate1_Hymenoptera", "estimate2_Hymenoptera", "estimate3_Hymenoptera","estimate4_Hymenoptera",
                      "estimateF_Hymenoptera","estimateM_Hymenoptera","estimateC10_Hymenoptera", "estimateC15_Hymenoptera",
                      "estimateA_Hymenoptera","estimateC_Hymenoptera",
                      "diffA_Hymenoptera", "diffC_Hymenoptera","diffT_Hymenoptera")

### define row order for coleoptera estimates
estimate_w5_rows <- c("mean_w_Coleoptera",
                      "bw.5.1.",
                      "bw.5.2.",
                      "bw.5.3.",
                      "bw.5.4.",
                      "bw.5.5.",
                      "bw.5.6.",
                      "bw.5.7.",
                      "bw.5.8.",
                      "bw.5.9.",
                      "bw.5.10.",
                      "bw_int.5.1.",
                      "V_brood_w_Coleoptera","V_sp_w_Coleoptera", "V_spy_w_Coleoptera", "V_wnr_w_Coleoptera", "V_e_w_Coleoptera",
                      "VC_brood_w_Coleoptera", "VC_sp_w_Coleoptera", "VC_spy_w_Coleoptera", "VC_wnr_w_Coleoptera", "VC_e_w_Coleoptera",
                      "phi_w_Coleoptera", "pi_w_Coleoptera",
                      "bluetit_effect_Coleoptera", "greattit_effect_Coleoptera",
                      "estimate1_Coleoptera", "estimate2_Coleoptera", "estimate3_Coleoptera","estimate4_Coleoptera",
                      "estimateF_Coleoptera","estimateM_Coleoptera","estimateC10_Coleoptera", "estimateC15_Coleoptera",
                      "estimateA_Coleoptera","estimateC_Coleoptera",
                      "diffA_Coleoptera", "diffC_Coleoptera","diffT_Coleoptera")

### subset and order lepidoptera estimates
# W1 - Lepidoptera
m1_combined1 <- m1_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

### subset and order diptera estimates
# W2 - Diptera
m1_combined2 <- m1_column %>%
  filter(rownames_column %in% estimate_w2_rows) %>%
  slice(match(estimate_w2_rows, rownames_column)) %>%
  rename(EstimateW2 = EstimateH)

### subset and order araneae estimates
# W3 - Araneae
m1_combined3 <- m1_column %>%
  filter(rownames_column %in% estimate_w3_rows) %>%
  slice(match(estimate_w3_rows, rownames_column)) %>%
  rename(EstimateW3 = EstimateH)

### subset and order hymenoptera estimates
# W4 - Hymenoptera
m1_combined4 <- m1_column %>%
  filter(rownames_column %in% estimate_w4_rows) %>%
  slice(match(estimate_w4_rows, rownames_column)) %>%
  rename(EstimateW4 = EstimateH)

### subset and order coleoptera estimates
# W5 - Coleoptera
m1_combined5 <- m1_column %>%
  filter(rownames_column %in% estimate_w5_rows) %>%
  slice(match(estimate_w5_rows, rownames_column)) %>%
  rename(EstimateW5 = EstimateH)

### define latex labels for rows
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
                              "\\text{Species}",
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

### combine latex names with estimates into table rows
m1_combined1$Latex_format <- paste(
  m1_combined1$Latex_names,  
  " & ",                    
  m1_combined1$EstimateW1, " & ",
  m1_combined2$EstimateW2, " & ",
  m1_combined3$EstimateW3, " & ",
  m1_combined4$EstimateW4, " & ",
  m1_combined5$EstimateW5,
  " \\"
)

### keep only latex formatted column
m1_Latex <- m1_combined1 %>%
  select(Latex_format)

### remove row names
rownames(m1_Latex) <- NULL

### print latex table output
print(m1_Latex, quote = FALSE, row.names = FALSE)



######################################################################################################### B4 ----
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

estimate_w_rows <- c("mean_w_Lepidoptera",
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
                     "V_brood_w_Lepidoptera", "V_sp_w_Lepidoptera", "V_spy_w_Lepidoptera",  "V_wnr_w_Lepidoptera",  "V_e_w_Lepidoptera",
                     "VC_brood_w_Lepidoptera", "VC_sp_w_Lepidoptera", "VC_spy_w_Lepidoptera", "VC_wnr_w_Lepidoptera", "VC_e_w_Lepidoptera",
                     "phi_w_Lepidoptera", "pi_w_Lepidoptera",
                     "bluetit_effect_Lepidoptera", "greattit_effect_Lepidoptera",
                     "estimate1_Lepidoptera", "estimate2_Lepidoptera", "estimate3_Lepidoptera","estimate4_Lepidoptera",
                     "estimateF_Lepidoptera","estimateM_Lepidoptera","estimateC10_Lepidoptera", "estimateC15_Lepidoptera",
                     "estimateA_Lepidoptera","estimateC_Lepidoptera",
                     "diffA_Lepidoptera", "diffC_Lepidoptera","diffT_Lepidoptera")


estimate_w2_rows <- c("mean_w_Diptera",
                      "bw.2.1.",
                      "bw.2.2.",
                      "bw.2.3.",
                      "bw.2.4.",
                      "bw.2.5.",
                      "bw.2.6.",
                      "bw.2.7.",
                      "bw.2.8.",
                      "bw.2.9.",
                      "bw_int.2.1.",
                      "V_brood_w_Diptera","V_sp_w_Diptera", "V_spy_w_Diptera", "V_wnr_w_Diptera", "V_e_w_Diptera",
                      "VC_brood_w_Diptera", "VC_sp_w_Diptera", "VC_spy_w_Diptera", "VC_wnr_w_Diptera", "VC_e_w_Diptera",
                      "phi_w_Diptera", "pi_w_Diptera",
                      "bluetit_effect_Diptera", "greattit_effect_Diptera",
                      "estimate1_Diptera", "estimate2_Diptera", "estimate3_Diptera","estimate4_Diptera",
                      "estimateF_Diptera","estimateM_Diptera","estimateC10_Diptera", "estimateC15_Diptera",
                      "estimateA_Diptera","estimateC_Diptera",
                      "diffA_Diptera", "diffC_Diptera","diffT_Diptera")


estimate_w3_rows <- c("mean_w_Araneae",
                      "bw.3.1.",
                      "bw.3.2.",
                      "bw.3.3.",
                      "bw.3.4.",
                      "bw.3.5.",
                      "bw.3.6.",
                      "bw.3.7.",
                      "bw.3.8.",
                      "bw.3.9.",
                      "bw_int.3.1.",
                      "V_brood_w_Araneae", "V_sp_w_Araneae", "V_spy_w_Araneae", "V_wnr_w_Araneae", "V_e_w_Araneae",
                      "VC_brood_w_Araneae", "VC_sp_w_Araneae",  "VC_spy_w_Araneae", "VC_wnr_w_Araneae","VC_e_w_Araneae",
                      "phi_w_Araneae", "pi_w_Araneae",
                      "bluetit_effect_Araneae", "greattit_effect_Araneae",
                      "estimate1_Araneae", "estimate2_Araneae", "estimate3_Araneae","estimate4_Araneae",
                      "estimateF_Araneae","estimateM_Araneae","estimateC10_Araneae", "estimateC15_Araneae",
                      "estimateA_Araneae","estimateC_Araneae",
                      "diffA_Araneae", "diffC_Araneae","diffT_Araneae")

estimate_w4_rows <- c("mean_w_Hymenoptera",
                      "bw.4.1.",
                      "bw.4.2.",
                      "bw.4.3.",
                      "bw.4.4.",
                      "bw.4.5.",
                      "bw.4.6.",
                      "bw.4.7.",
                      "bw.4.8.",
                      "bw.4.9.",
                      "bw_int.4.1.",
                      "V_brood_w_Hymenoptera", "V_sp_w_Hymenoptera", "V_spy_w_Hymenoptera", "V_wnr_w_Hymenoptera", "V_e_w_Hymenoptera",
                      "VC_brood_w_Hymenoptera","VC_sp_w_Hymenoptera",  "VC_spy_w_Hymenoptera", "VC_wnr_w_Hymenoptera", "VC_e_w_Hymenoptera",
                      "phi_w_Hymenoptera", "pi_w_Hymenoptera",
                      "bluetit_effect_Hymenoptera", "greattit_effect_Hymenoptera",
                      "estimate1_Hymenoptera", "estimate2_Hymenoptera", "estimate3_Hymenoptera","estimate4_Hymenoptera",
                      "estimateF_Hymenoptera","estimateM_Hymenoptera","estimateC10_Hymenoptera", "estimateC15_Hymenoptera",
                      "estimateA_Hymenoptera","estimateC_Hymenoptera",
                      "diffA_Hymenoptera", "diffC_Hymenoptera","diffT_Hymenoptera")

estimate_w5_rows <- c("mean_w_Coleoptera",
                      "bw.5.1.",
                      "bw.5.2.",
                      "bw.5.3.",
                      "bw.5.4.",
                      "bw.5.5.",
                      "bw.5.6.",
                      "bw.5.7.",
                      "bw.5.8.",
                      "bw.5.9.",
                      "bw_int.5.1.",
                      "V_brood_w_Coleoptera","V_sp_w_Coleoptera", "V_spy_w_Coleoptera", "V_wnr_w_Coleoptera", "V_e_w_Coleoptera",
                      "VC_brood_w_Coleoptera", "VC_sp_w_Coleoptera", "VC_spy_w_Coleoptera", "VC_wnr_w_Coleoptera", "VC_e_w_Coleoptera",
                      "phi_w_Coleoptera", "pi_w_Coleoptera",
                      "bluetit_effect_Coleoptera", "greattit_effect_Coleoptera",
                      "estimate1_Coleoptera", "estimate2_Coleoptera", "estimate3_Coleoptera","estimate4_Coleoptera",
                      "estimateF_Coleoptera","estimateM_Coleoptera","estimateC10_Coleoptera", "estimateC15_Coleoptera",
                      "estimateA_Coleoptera","estimateC_Coleoptera",
                      "diffA_Coleoptera", "diffC_Coleoptera","diffT_Coleoptera")



# W1 - Lepidoptera
m2_combined1 <- m2_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

# W2 - Diptera
m2_combined2 <- m2_column %>%
  filter(rownames_column %in% estimate_w2_rows) %>%
  slice(match(estimate_w2_rows, rownames_column)) %>%
  rename(EstimateW2 = EstimateH)

# W3 - Araneae
m2_combined3 <- m2_column %>%
  filter(rownames_column %in% estimate_w3_rows) %>%
  slice(match(estimate_w3_rows, rownames_column)) %>%
  rename(EstimateW3 = EstimateH)

# W4 - Hymenoptera
m2_combined4 <- m2_column %>%
  filter(rownames_column %in% estimate_w4_rows) %>%
  slice(match(estimate_w4_rows, rownames_column)) %>%
  rename(EstimateW4 = EstimateH)

# W5 - Coleoptera
m2_combined5 <- m2_column %>%
  filter(rownames_column %in% estimate_w5_rows) %>%
  slice(match(estimate_w5_rows, rownames_column)) %>%
  rename(EstimateW5 = EstimateH)




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
  m2_combined1$EstimateW1, " & ",
  m2_combined2$EstimateW2, " & ",
  m2_combined3$EstimateW3, " & ",
  m2_combined4$EstimateW4, " & ",
  m2_combined5$EstimateW5,
  " \\"
)



m2_Latex <- m2_combined1 %>%
  select(Latex_format)
rownames(m2_Latex) <- NULL
print(m2_Latex, quote = FALSE, row.names = FALSE)


######################################################################################################### G4 ----
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


estimate_w_rows <- c("mean_w_Lepidoptera",
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
                     "V_brood_w_Lepidoptera", "V_sp_w_Lepidoptera", "V_spy_w_Lepidoptera",  "V_wnr_w_Lepidoptera",  "V_e_w_Lepidoptera",
                     "VC_brood_w_Lepidoptera", "VC_sp_w_Lepidoptera", "VC_spy_w_Lepidoptera", "VC_wnr_w_Lepidoptera", "VC_e_w_Lepidoptera",
                     "phi_w_Lepidoptera", "pi_w_Lepidoptera",
                     "bluetit_effect_Lepidoptera", "greattit_effect_Lepidoptera",
                     "estimate1_Lepidoptera", "estimate2_Lepidoptera", "estimate3_Lepidoptera","estimate4_Lepidoptera",
                     "estimateF_Lepidoptera","estimateM_Lepidoptera","estimateC10_Lepidoptera", "estimateC15_Lepidoptera",
                     "estimateA_Lepidoptera","estimateC_Lepidoptera",
                     "diffA_Lepidoptera", "diffC_Lepidoptera","diffT_Lepidoptera")


estimate_w2_rows <- c("mean_w_Diptera",
                      "bw.2.1.",
                      "bw.2.2.",
                      "bw.2.3.",
                      "bw.2.4.",
                      "bw.2.5.",
                      "bw.2.6.",
                      "bw.2.7.",
                      "bw.2.8.",
                      "bw.2.9.",
                      "bw_int.2.1.",
                      "V_brood_w_Diptera","V_sp_w_Diptera", "V_spy_w_Diptera", "V_wnr_w_Diptera", "V_e_w_Diptera",
                      "VC_brood_w_Diptera", "VC_sp_w_Diptera", "VC_spy_w_Diptera", "VC_wnr_w_Diptera", "VC_e_w_Diptera",
                      "phi_w_Diptera", "pi_w_Diptera",
                      "bluetit_effect_Diptera", "greattit_effect_Diptera",
                      "estimate1_Diptera", "estimate2_Diptera", "estimate3_Diptera","estimate4_Diptera",
                      "estimateF_Diptera","estimateM_Diptera","estimateC10_Diptera", "estimateC15_Diptera",
                      "estimateA_Diptera","estimateC_Diptera",
                      "diffA_Diptera", "diffC_Diptera","diffT_Diptera")


estimate_w3_rows <- c("mean_w_Araneae",
                      "bw.3.1.",
                      "bw.3.2.",
                      "bw.3.3.",
                      "bw.3.4.",
                      "bw.3.5.",
                      "bw.3.6.",
                      "bw.3.7.",
                      "bw.3.8.",
                      "bw.3.9.",
                      "bw_int.3.1.",
                      "V_brood_w_Araneae", "V_sp_w_Araneae", "V_spy_w_Araneae", "V_wnr_w_Araneae", "V_e_w_Araneae",
                      "VC_brood_w_Araneae", "VC_sp_w_Araneae",  "VC_spy_w_Araneae", "VC_wnr_w_Araneae","VC_e_w_Araneae",
                      "phi_w_Araneae", "pi_w_Araneae",
                      "bluetit_effect_Araneae", "greattit_effect_Araneae",
                      "estimate1_Araneae", "estimate2_Araneae", "estimate3_Araneae","estimate4_Araneae",
                      "estimateF_Araneae","estimateM_Araneae","estimateC10_Araneae", "estimateC15_Araneae",
                      "estimateA_Araneae","estimateC_Araneae",
                      "diffA_Araneae", "diffC_Araneae","diffT_Araneae")

estimate_w4_rows <- c("mean_w_Hymenoptera",
                      "bw.4.1.",
                      "bw.4.2.",
                      "bw.4.3.",
                      "bw.4.4.",
                      "bw.4.5.",
                      "bw.4.6.",
                      "bw.4.7.",
                      "bw.4.8.",
                      "bw.4.9.",
                      "bw_int.4.1.",
                      "V_brood_w_Hymenoptera", "V_sp_w_Hymenoptera", "V_spy_w_Hymenoptera", "V_wnr_w_Hymenoptera", "V_e_w_Hymenoptera",
                      "VC_brood_w_Hymenoptera","VC_sp_w_Hymenoptera",  "VC_spy_w_Hymenoptera", "VC_wnr_w_Hymenoptera", "VC_e_w_Hymenoptera",
                      "phi_w_Hymenoptera", "pi_w_Hymenoptera",
                      "bluetit_effect_Hymenoptera", "greattit_effect_Hymenoptera",
                      "estimate1_Hymenoptera", "estimate2_Hymenoptera", "estimate3_Hymenoptera","estimate4_Hymenoptera",
                      "estimateF_Hymenoptera","estimateM_Hymenoptera","estimateC10_Hymenoptera", "estimateC15_Hymenoptera",
                      "estimateA_Hymenoptera","estimateC_Hymenoptera",
                      "diffA_Hymenoptera", "diffC_Hymenoptera","diffT_Hymenoptera")

estimate_w5_rows <- c("mean_w_Coleoptera",
                      "bw.5.1.",
                      "bw.5.2.",
                      "bw.5.3.",
                      "bw.5.4.",
                      "bw.5.5.",
                      "bw.5.6.",
                      "bw.5.7.",
                      "bw.5.8.",
                      "bw.5.9.",
                      "bw_int.5.1.",
                      "V_brood_w_Coleoptera","V_sp_w_Coleoptera", "V_spy_w_Coleoptera", "V_wnr_w_Coleoptera", "V_e_w_Coleoptera",
                      "VC_brood_w_Coleoptera", "VC_sp_w_Coleoptera", "VC_spy_w_Coleoptera", "VC_wnr_w_Coleoptera", "VC_e_w_Coleoptera",
                      "phi_w_Coleoptera", "pi_w_Coleoptera",
                      "bluetit_effect_Coleoptera", "greattit_effect_Coleoptera",
                      "estimate1_Coleoptera", "estimate2_Coleoptera", "estimate3_Coleoptera","estimate4_Coleoptera",
                      "estimateF_Coleoptera","estimateM_Coleoptera","estimateC10_Coleoptera", "estimateC15_Coleoptera",
                      "estimateA_Coleoptera","estimateC_Coleoptera",
                      "diffA_Coleoptera", "diffC_Coleoptera","diffT_Coleoptera")

# W1 - Lepidoptera
m3_combined1 <- m3_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

# W2 - Diptera
m3_combined2 <- m3_column %>%
  filter(rownames_column %in% estimate_w2_rows) %>%
  slice(match(estimate_w2_rows, rownames_column)) %>%
  rename(EstimateW2 = EstimateH)

# W3 - Araneae
m3_combined3 <- m3_column %>%
  filter(rownames_column %in% estimate_w3_rows) %>%
  slice(match(estimate_w3_rows, rownames_column)) %>%
  rename(EstimateW3 = EstimateH)

# W4 - Hymenoptera
m3_combined4 <- m3_column %>%
  filter(rownames_column %in% estimate_w4_rows) %>%
  slice(match(estimate_w4_rows, rownames_column)) %>%
  rename(EstimateW4 = EstimateH)

# W5 - Coleoptera
m3_combined5 <- m3_column %>%
  filter(rownames_column %in% estimate_w5_rows) %>%
  slice(match(estimate_w5_rows, rownames_column)) %>%
  rename(EstimateW5 = EstimateH)



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
  m3_combined1$EstimateW1, " & ",
  m3_combined2$EstimateW2, " & ",
  m3_combined3$EstimateW3, " & ",
  m3_combined4$EstimateW4, " & ",
  m3_combined5$EstimateW5,
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


estimate_w2_rows <- c("mean_w_Diptera",
                      "bw.2.1.",
                      "bw.2.2.",
                      "bw.2.3.",
                      "bw.2.4.",
                      "bw.2.5.",
                      "bw.2.6.",
                      "bw.2.7.",
                      "bw.2.8.",
                      "bw_int.2.1.",
                      "V_brood_w_Diptera","V_sp_w_Diptera", "V_spy_w_Diptera", "V_wnr_w_Diptera", "V_e_w_Diptera",
                      "VC_brood_w_Diptera", "VC_sp_w_Diptera", "VC_spy_w_Diptera", "VC_wnr_w_Diptera", "VC_e_w_Diptera",
                      "phi_w_Diptera", "pi_w_Diptera",
                      "bluetit_effect_Diptera", "greattit_effect_Diptera",
                      "estimate1_Diptera", "estimate2_Diptera", "estimate3_Diptera","estimate4_Diptera")


estimate_w3_rows <- c("mean_w_Araneae",
                      "bw.3.1.",
                      "bw.3.2.",
                      "bw.3.3.",
                      "bw.3.4.",
                      "bw.3.5.",
                      "bw.3.6.",
                      "bw.3.7.",
                      "bw.3.8.",
                      "bw_int.3.1.",
                      "V_brood_w_Araneae", "V_sp_w_Araneae", "V_spy_w_Araneae", "V_wnr_w_Araneae", "V_e_w_Araneae",
                      "VC_brood_w_Araneae", "VC_sp_w_Araneae",  "VC_spy_w_Araneae", "VC_wnr_w_Araneae","VC_e_w_Araneae",
                      "phi_w_Araneae", "pi_w_Araneae",
                      "bluetit_effect_Araneae", "greattit_effect_Araneae",
                      "estimate1_Araneae", "estimate2_Araneae", "estimate3_Araneae","estimate4_Araneae")

estimate_w5_rows <- c("mean_w_Coleoptera",
                      "bw.5.1.",
                      "bw.5.2.",
                      "bw.5.3.",
                      "bw.5.4.",
                      "bw.5.5.",
                      "bw.5.6.",
                      "bw.5.7.",
                      "bw.5.8.",
                      "bw_int.5.1.",
                      "V_brood_w_Coleoptera","V_sp_w_Coleoptera", "V_spy_w_Coleoptera", "V_wnr_w_Coleoptera", "V_e_w_Coleoptera",
                      "VC_brood_w_Coleoptera", "VC_sp_w_Coleoptera", "VC_spy_w_Coleoptera", "VC_wnr_w_Coleoptera", "VC_e_w_Coleoptera",
                      "phi_w_Coleoptera", "pi_w_Coleoptera",
                      "bluetit_effect_Coleoptera", "greattit_effect_Coleoptera",
                      "estimate1_Coleoptera", "estimate2_Coleoptera", "estimate3_Coleoptera","estimate4_Coleoptera")


# W2 - Diptera
m4_combined2 <- m4_column %>%
  filter(rownames_column %in% estimate_w2_rows) %>%
  slice(match(estimate_w2_rows, rownames_column)) %>%
  rename(EstimateW2 = EstimateH)

# W3 - Araneae
m4_combined3 <- m4_column %>%
  filter(rownames_column %in% estimate_w3_rows) %>%
  slice(match(estimate_w3_rows, rownames_column)) %>%
  rename(EstimateW3 = EstimateH)


# W5 - Coleoptera
m4_combined5 <- m4_column %>%
  filter(rownames_column %in% estimate_w5_rows) %>%
  slice(match(estimate_w5_rows, rownames_column)) %>%
  rename(EstimateW5 = EstimateH)



m4_combined2$Latex_names <- c("\\text{Intercept}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Size subplot}",
                              "\\text{Doublebox}",
                              "\\text{Chick}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{Species}",
                              "\\text{TMB x TMG}",
                              "\\text{Brood}", "\\text{SubPlot}","\\text{SubPlotYear}", "\\text{Within nest box}", "\\text{Residual}",
                              "\\text{Brood VC}", "\\text{SubPlot VC}", "\\text{SubPlotYear VC}", "\\text{Within nest box VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$")



m4_combined2$Latex_format <- paste(
  m4_combined2$Latex_names,  # Latex names column
  " & ",                    
  m4_combined2$EstimateW2, " & ",
  m4_combined3$EstimateW3, " & ",
  m4_combined5$EstimateW5,
  " \\"
)



m4_Latex <- m4_combined2 %>%
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




estimate_w2_rows <- c("mean_w_Diptera",
                      "bw.2.1.",
                      "bw.2.2.",
                      "bw.2.3.",
                      "bw.2.4.",
                      "bw.2.5.",
                      "bw.2.6.",
                      "bw.2.7.",
                      "bw_int.2.1.",
                      "V_brood_w_Diptera","V_sp_w_Diptera", "V_spy_w_Diptera", "V_wnr_w_Diptera", "V_e_w_Diptera",
                      "VC_brood_w_Diptera", "VC_sp_w_Diptera", "VC_spy_w_Diptera", "VC_wnr_w_Diptera", "VC_e_w_Diptera",
                      "phi_w_Diptera", "pi_w_Diptera",
                      "bluetit_effect_Diptera", "greattit_effect_Diptera",
                      "estimate1_Diptera", "estimate2_Diptera", "estimate3_Diptera","estimate4_Diptera")


estimate_w3_rows <- c("mean_w_Araneae",
                      "bw.3.1.",
                      "bw.3.2.",
                      "bw.3.3.",
                      "bw.3.4.",
                      "bw.3.5.",
                      "bw.3.6.",
                      "bw.3.7.",
                      "bw_int.3.1.",
                      "V_brood_w_Araneae", "V_sp_w_Araneae", "V_spy_w_Araneae", "V_wnr_w_Araneae", "V_e_w_Araneae",
                      "VC_brood_w_Araneae", "VC_sp_w_Araneae",  "VC_spy_w_Araneae", "VC_wnr_w_Araneae","VC_e_w_Araneae",
                      "phi_w_Araneae", "pi_w_Araneae",
                      "bluetit_effect_Araneae", "greattit_effect_Araneae",
                      "estimate1_Araneae", "estimate2_Araneae", "estimate3_Araneae","estimate4_Araneae")


estimate_w5_rows <- c("mean_w_Coleoptera",
                      "bw.5.1.",
                      "bw.5.2.",
                      "bw.5.3.",
                      "bw.5.4.",
                      "bw.5.5.",
                      "bw.5.6.",
                      "bw.5.7.",
                      "bw_int.5.1.",
                      "V_brood_w_Coleoptera","V_sp_w_Coleoptera", "V_spy_w_Coleoptera", "V_wnr_w_Coleoptera", "V_e_w_Coleoptera",
                      "VC_brood_w_Coleoptera", "VC_sp_w_Coleoptera", "VC_spy_w_Coleoptera", "VC_wnr_w_Coleoptera", "VC_e_w_Coleoptera",
                      "phi_w_Coleoptera", "pi_w_Coleoptera",
                      "bluetit_effect_Coleoptera", "greattit_effect_Coleoptera",
                      "estimate1_Coleoptera", "estimate2_Coleoptera", "estimate3_Coleoptera","estimate4_Coleoptera")




# W2 - Diptera
m5_combined2 <- m5_column %>%
  filter(rownames_column %in% estimate_w2_rows) %>%
  slice(match(estimate_w2_rows, rownames_column)) %>%
  rename(EstimateW2 = EstimateH)

# W3 - Araneae
m5_combined3 <- m5_column %>%
  filter(rownames_column %in% estimate_w3_rows) %>%
  slice(match(estimate_w3_rows, rownames_column)) %>%
  rename(EstimateW3 = EstimateH)

# W5 - Coleoptera
m5_combined5 <- m5_column %>%
  filter(rownames_column %in% estimate_w5_rows) %>%
  slice(match(estimate_w5_rows, rownames_column)) %>%
  rename(EstimateW5 = EstimateH)




m5_combined2$Latex_names <- c("\\text{Intercept}", 
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



m5_combined2$Latex_format <- paste(
  m5_combined2$Latex_names,  # Latex names column
  " & ",                    
  m5_combined2$EstimateW2, " & ",
  m5_combined3$EstimateW3, " & ",
  m5_combined5$EstimateW5,
  " \\"
)




m5_Latex <- m5_combined2 %>%
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


estimate_w_rows <- c("mean_w_Lepidoptera",
                     "bw.1.1.",
                     "bw.1.2.",
                     "bw.1.3.",
                     "bw.1.4.",
                     "bw.1.5.",
                     "bw.1.6.",
                     "bw.1.7.",
                     "bw_int.1.1.",
                     "V_brood_w_Lepidoptera", "V_sp_w_Lepidoptera", "V_spy_w_Lepidoptera",  "V_wnr_w_Lepidoptera",  "V_e_w_Lepidoptera",
                     "VC_brood_w_Lepidoptera", "VC_sp_w_Lepidoptera", "VC_spy_w_Lepidoptera", "VC_wnr_w_Lepidoptera", "VC_e_w_Lepidoptera",
                     "phi_w_Lepidoptera", "pi_w_Lepidoptera",
                     "bluetit_effect_Lepidoptera", "greattit_effect_Lepidoptera",
                     "estimate1_Lepidoptera", "estimate2_Lepidoptera", "estimate3_Lepidoptera","estimate4_Lepidoptera")


estimate_w2_rows <- c("mean_w_Diptera",
                      "bw.2.1.",
                      "bw.2.2.",
                      "bw.2.3.",
                      "bw.2.4.",
                      "bw.2.5.",
                      "bw.2.6.",
                      "bw.2.7.",
                      "bw_int.2.1.",
                      "V_brood_w_Diptera","V_sp_w_Diptera", "V_spy_w_Diptera", "V_wnr_w_Diptera", "V_e_w_Diptera",
                      "VC_brood_w_Diptera", "VC_sp_w_Diptera", "VC_spy_w_Diptera", "VC_wnr_w_Diptera", "VC_e_w_Diptera",
                      "phi_w_Diptera", "pi_w_Diptera",
                      "bluetit_effect_Diptera", "greattit_effect_Diptera",
                      "estimate1_Diptera", "estimate2_Diptera", "estimate3_Diptera","estimate4_Diptera")


estimate_w3_rows <- c("mean_w_Araneae",
                      "bw.3.1.",
                      "bw.3.2.",
                      "bw.3.3.",
                      "bw.3.4.",
                      "bw.3.5.",
                      "bw.3.6.",
                      "bw.3.7.",
                      "bw_int.3.1.",
                      "V_brood_w_Araneae", "V_sp_w_Araneae", "V_spy_w_Araneae", "V_wnr_w_Araneae", "V_e_w_Araneae",
                      "VC_brood_w_Araneae", "VC_sp_w_Araneae",  "VC_spy_w_Araneae", "VC_wnr_w_Araneae","VC_e_w_Araneae",
                      "phi_w_Araneae", "pi_w_Araneae",
                      "bluetit_effect_Araneae", "greattit_effect_Araneae",
                      "estimate1_Araneae", "estimate2_Araneae", "estimate3_Araneae","estimate4_Araneae")


estimate_w5_rows <- c("mean_w_Coleoptera",
                      "bw.5.1.",
                      "bw.5.2.",
                      "bw.5.3.",
                      "bw.5.4.",
                      "bw.5.5.",
                      "bw.5.6.",
                      "bw.5.7.",
                      "bw_int.5.1.",
                      "V_brood_w_Coleoptera","V_sp_w_Coleoptera", "V_spy_w_Coleoptera", "V_wnr_w_Coleoptera", "V_e_w_Coleoptera",
                      "VC_brood_w_Coleoptera", "VC_sp_w_Coleoptera", "VC_spy_w_Coleoptera", "VC_wnr_w_Coleoptera", "VC_e_w_Coleoptera",
                      "phi_w_Coleoptera", "pi_w_Coleoptera",
                      "bluetit_effect_Coleoptera", "greattit_effect_Coleoptera",
                      "estimate1_Coleoptera", "estimate2_Coleoptera", "estimate3_Coleoptera","estimate4_Coleoptera")

# W1 - Lepidoptera
m6_combined1 <- m6_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

# W2 - Diptera
m6_combined2 <- m6_column %>%
  filter(rownames_column %in% estimate_w2_rows) %>%
  slice(match(estimate_w2_rows, rownames_column)) %>%
  rename(EstimateW2 = EstimateH)

# W3 - Araneae
m6_combined3 <- m6_column %>%
  filter(rownames_column %in% estimate_w3_rows) %>%
  slice(match(estimate_w3_rows, rownames_column)) %>%
  rename(EstimateW3 = EstimateH)


# W5 - Coleoptera
m6_combined5 <- m6_column %>%
  filter(rownames_column %in% estimate_w5_rows) %>%
  slice(match(estimate_w5_rows, rownames_column)) %>%
  rename(EstimateW5 = EstimateH)



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
  m6_combined1$EstimateW1, " & ",
  m6_combined2$EstimateW2, " & ",
  m6_combined3$EstimateW3, " & ",
  m6_combined5$EstimateW5,
  " \\"
)



m6_Latex <- m6_combined1 %>%
  select(Latex_format)
rownames(m6_Latex) <- NULL
print(m6_Latex, quote = FALSE, row.names = FALSE)

####################################################################################################### ALL3 ----
m7 <- m7[c(2:4),]

m7_transposed <- t(m7)


colnames(m7_transposed) <- m7_transposed[1, ]# take the first row as column names
m7_transposed <- m7_transposed[-1, ] # remove the first row (which is now the column names)

m7_df <- as.data.frame(m7_transposed)

m7_df$rownames_column <- rownames(m7_df) # make row names into a column

m7_df <- m7_df %>%
  mutate(across(c(`2.5%`, `50%`, `97.5%`), ~ format(round(as.numeric(.), 3), nsmall = 3))) # concerts to numeric then rounds to 3rd decimal

m7_column <- m7_df %>%
  mutate(EstimateH = paste(`50%`, "[", `2.5%`, ";", `97.5%`, "]", sep = " ")) %>%
  select(EstimateH, rownames_column)

### rearrange df


estimate_w4_rows <- c("mean_w_Hymenoptera",
                      "bw.4.1.",
                      "bw.4.2.",
                      "bw.4.3.",
                      "bw.4.4.",
                      "bw.4.5.",
                      "bw.4.6.",
                      "bw.4.7.",
                      "bw.4.8.",
                      "bw.4.9.",
                      "bw_int.4.1.",
                      "V_brood_w_Hymenoptera", "V_sp_w_Hymenoptera", "V_spy_w_Hymenoptera", "V_wnr_w_Hymenoptera", "V_e_w_Hymenoptera",
                      "VC_brood_w_Hymenoptera","VC_sp_w_Hymenoptera",  "VC_spy_w_Hymenoptera", "VC_wnr_w_Hymenoptera", "VC_e_w_Hymenoptera",
                      "phi_w_Hymenoptera", "pi_w_Hymenoptera",
                      "bluetit_effect_Hymenoptera", "greattit_effect_Hymenoptera",
                      "estimate1_Hymenoptera", "estimate2_Hymenoptera", "estimate3_Hymenoptera","estimate4_Hymenoptera")

# W4 - Hymenoptera
m7_combined4 <- m1_column %>%
  filter(rownames_column %in% estimate_w4_rows) %>%
  slice(match(estimate_w4_rows, rownames_column)) %>%
  rename(EstimateW4 = EstimateH)


m7_combined4$Latex_names <- c("\\text{Intercept}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Size subplot}",
                              "\\text{Doublebox}",
                              "\\text{Chick 10}",
                              "\\text{Chick 15}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{Species}",
                              "\\text{TMB x TMG}",
                              "\\text{Brood}", "\\text{SubPlot}","\\text{SubPlotYear}", "\\text{Within nest box}", "\\text{Residual}",
                              "\\text{Brood VC}", "\\text{SubPlot VC}", "\\text{SubPlotYear VC}", "\\text{Within nest box VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$")



m7_combined4$Latex_format <- paste(
  m7_combined4$Latex_names,  # Latex names column
  " & ",                    
  m7_combined4$EstimateW4, 
  " \\"
)



m7_Latex <- m7_combined4 %>%
  select(Latex_format)
rownames(m7_Latex) <- NULL
print(m7_Latex, quote = FALSE, row.names = FALSE)



######################################################################################################### B3 ----
m8 <- m8[c(2:4),]

m8_transposed <- t(m8)


colnames(m8_transposed) <- m8_transposed[1, ]# take the first row as column names
m8_transposed <- m8_transposed[-1, ] # remove the first row (which is now the column names)

m8_df <- as.data.frame(m8_transposed)

m8_df$rownames_column <- rownames(m8_df) # make row names into a column

m8_df <- m8_df %>%
  mutate(across(c(`2.5%`, `50%`, `97.5%`), ~ format(round(as.numeric(.), 3), nsmall = 3))) # concerts to numeric then rounds to 3rd decimal

m8_column <- m8_df %>%
  mutate(EstimateH = paste(`50%`, "[", `2.5%`, ";", `97.5%`, "]", sep = " ")) %>%
  select(EstimateH, rownames_column)

### rearrange df




estimate_w_rows <- c("mean_w_Lepidoptera",
                     "bw.1.1.",
                     "bw.1.2.",
                     "bw.1.3.",
                     "bw.1.4.",
                     "bw.1.5.",
                     "bw.1.6.",
                     "bw.1.7.",
                     "bw.1.8.",
                     "bw_int.1.1.",
                     "V_brood_w_Lepidoptera", "V_sp_w_Lepidoptera", "V_spy_w_Lepidoptera",  "V_wnr_w_Lepidoptera",  "V_e_w_Lepidoptera",
                     "VC_brood_w_Lepidoptera", "VC_sp_w_Lepidoptera", "VC_spy_w_Lepidoptera", "VC_wnr_w_Lepidoptera", "VC_e_w_Lepidoptera",
                     "phi_w_Lepidoptera", "pi_w_Lepidoptera",
                     "bluetit_effect_Lepidoptera", "greattit_effect_Lepidoptera",
                     "estimate1_Lepidoptera", "estimate2_Lepidoptera", "estimate3_Lepidoptera","estimate4_Lepidoptera",
                     "estimateF_Lepidoptera","estimateM_Lepidoptera","estimateC10_Lepidoptera", "estimateC15_Lepidoptera",
                     "estimateA_Lepidoptera","estimateC_Lepidoptera",
                     "diffA_Lepidoptera", "diffC_Lepidoptera","diffT_Lepidoptera")



estimate_w4_rows <- c("mean_w_Hymenoptera",
                      "bw.4.1.",
                      "bw.4.2.",
                      "bw.4.3.",
                      "bw.4.4.",
                      "bw.4.5.",
                      "bw.4.6.",
                      "bw.4.7.",
                      "bw.4.8.",
                      "bw_int.4.1.",
                      "V_brood_w_Hymenoptera", "V_sp_w_Hymenoptera", "V_spy_w_Hymenoptera", "V_wnr_w_Hymenoptera", "V_e_w_Hymenoptera",
                      "VC_brood_w_Hymenoptera","VC_sp_w_Hymenoptera",  "VC_spy_w_Hymenoptera", "VC_wnr_w_Hymenoptera", "VC_e_w_Hymenoptera",
                      "phi_w_Hymenoptera", "pi_w_Hymenoptera",
                      "bluetit_effect_Hymenoptera", "greattit_effect_Hymenoptera",
                      "estimate1_Hymenoptera", "estimate2_Hymenoptera", "estimate3_Hymenoptera","estimate4_Hymenoptera",
                      "estimateF_Hymenoptera","estimateM_Hymenoptera","estimateC10_Hymenoptera", "estimateC15_Hymenoptera",
                      "estimateA_Hymenoptera","estimateC_Hymenoptera",
                      "diffA_Hymenoptera", "diffC_Hymenoptera","diffT_Hymenoptera")





# W1 - Lepidoptera
m8_combined1 <- m8_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

# W4
m8_combined4 <- m8_column %>%
  filter(rownames_column %in% estimate_w4_rows) %>%
  slice(match(estimate_w4_rows, rownames_column)) %>%
  rename(EstimateW3 = EstimateH)





m8_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Size subplot}",
                              "\\text{Doublebox}",
                              "\\text{Chick 10}",
                              "\\text{Chick 15}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{TMB x TMG}",
                              "\\text{Brood}", "\\text{SubPlot}","\\text{SubPlotYear}", "\\text{Within nest box}", "\\text{Residual}",
                              "\\text{Brood VC}", "\\text{SubPlot VC}", "\\text{SubPlotYear VC}", "\\text{Within nest box VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$")



m8_combined1$Latex_format <- paste(
  m8_combined1$Latex_names,  # Latex names column
  " & ",                    
  m8_combined1$EstimateW1, " & ",
  m8_combined4$EstimateW3, " & ",
  " \\"
)




m8_Latex <- m8_combined1 %>%
  select(Latex_format)
rownames(m8_Latex) <- NULL
print(m8_Latex, quote = FALSE, row.names = FALSE)



####################################################################################################### ALL4 ----
m9 <- m9[c(2:4),]

m9_transposed <- t(m9)


colnames(m9_transposed) <- m9_transposed[1, ]# take the first row as column names
m9_transposed <- m9_transposed[-1, ] # remove the first row (which is now the column names)

m9_df <- as.data.frame(m9_transposed)

m9_df$rownames_column <- rownames(m9_df) # make row names into a column

m9_df <- m9_df %>%
  mutate(across(c(`2.5%`, `50%`, `97.5%`), ~ format(round(as.numeric(.), 3), nsmall = 3))) # concerts to numeric then rounds to 3rd decimal

m9_column <- m9_df %>%
  mutate(EstimateH = paste(`50%`, "[", `2.5%`, ";", `97.5%`, "]", sep = " ")) %>%
  select(EstimateH, rownames_column)

### rearrange df

estimate_w_rows <- c("mean_w_Lepidoptera",
                     "bw.1.1.",
                     "bw.1.2.",
                     "bw.1.3.",
                     "bw.1.4.",
                     "bw.1.5.",
                     "bw.1.6.",
                     "bw.1.7.",
                     "bw_int.1.1.",
                     "V_brood_w_Lepidoptera", "V_sp_w_Lepidoptera", "V_spy_w_Lepidoptera",  "V_wnr_w_Lepidoptera",  "V_e_w_Lepidoptera",
                     "VC_brood_w_Lepidoptera", "VC_sp_w_Lepidoptera", "VC_spy_w_Lepidoptera", "VC_wnr_w_Lepidoptera", "VC_e_w_Lepidoptera",
                     "phi_w_Lepidoptera", "pi_w_Lepidoptera",
                     "bluetit_effect_Lepidoptera", "greattit_effect_Lepidoptera",
                     "estimate1_Lepidoptera", "estimate2_Lepidoptera", "estimate3_Lepidoptera","estimate4_Lepidoptera")



# W1 - Lepidoptera
m9_combined1 <- m9_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)


m9_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Size subplot}",
                              "\\text{Doublebox}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{Species}",
                              "\\text{TMB x TMG}",
                              "\\text{Brood}", "\\text{SubPlot}","\\text{SubPlotYear}", "\\text{Within nest box}", "\\text{Residual}",
                              "\\text{Brood VC}", "\\text{SubPlot VC}", "\\text{SubPlotYear VC}", "\\text{Within nest box VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$")



m9_combined1$Latex_format <- paste(
  m9_combined1$Latex_names,  # Latex names column
  " & ",                    
  m9_combined1$EstimateW1,
  " \\"
)



m9_Latex <- m9_combined1 %>%
  select(Latex_format)
rownames(m9_Latex) <- NULL
print(m9_Latex, quote = FALSE, row.names = FALSE)

######################################################################################################### G4 ----
m10 <- m10[c(2:4),]

m10_transposed <- t(m10)


colnames(m10_transposed) <- m10_transposed[1, ]# take the first row as column names
m10_transposed <- m10_transposed[-1, ] # remove the first row (which is now the column names)

m10_df <- as.data.frame(m10_transposed)

m10_df$rownames_column <- rownames(m10_df) # make row names into a column

m10_df <- m10_df %>%
  mutate(across(c(`2.5%`, `50%`, `97.5%`), ~ format(round(as.numeric(.), 3), nsmall = 3))) # concerts to numeric then rounds to 3rd decimal

m10_column <- m10_df %>%
  mutate(EstimateH = paste(`50%`, "[", `2.5%`, ";", `97.5%`, "]", sep = " ")) %>%
  select(EstimateH, rownames_column)

### rearrange df

estimate_w_rows <- c("mean_w_Hymenoptera",
                     "bw.1.1.",
                     "bw.1.2.",
                     "bw.1.3.",
                     "bw.1.4.",
                     "bw.1.5.",
                     "bw.1.6.",
                     "bw_int.1.1.",
                     "V_brood_w_Hymenoptera", "V_sp_w_Hymenoptera", "V_spy_w_Hymenoptera",  "V_wnr_w_Hymenoptera",  "V_e_w_Hymenoptera",
                     "VC_brood_w_Hymenoptera", "VC_sp_w_Hymenoptera", "VC_spy_w_Hymenoptera", "VC_wnr_w_Hymenoptera", "VC_e_w_Hymenoptera",
                     "phi_w_Hymenoptera", "pi_w_Hymenoptera",
                     "bluetit_effect_Hymenoptera", "greattit_effect_Hymenoptera",
                     "estimate1_Hymenoptera", "estimate2_Hymenoptera", "estimate3_Hymenoptera","estimate4_Hymenoptera")



# W1 - Lepidoptera
m10_combined1 <- m10_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)


m10_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Size subplot}",
                              "\\text{Doublebox}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{TMB x TMG}",
                              "\\text{Brood}", "\\text{SubPlot}","\\text{SubPlotYear}", "\\text{Within nest box}", "\\text{Residual}",
                              "\\text{Brood VC}", "\\text{SubPlot VC}", "\\text{SubPlotYear VC}", "\\text{Within nest box VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$")



m10_combined1$Latex_format <- paste(
  m10_combined1$Latex_names,  # Latex names column
  " & ",                    
  m10_combined1$EstimateW1,
  " \\"
)



m10_Latex <- m10_combined1 %>%
  select(Latex_format)
rownames(m10_Latex) <- NULL
print(m10_Latex, quote = FALSE, row.names = FALSE)



