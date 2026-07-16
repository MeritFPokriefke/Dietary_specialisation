
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 04#T_Diet_Exp

### Prediction 1: Association prey preference and personality (adults)
### Latex ready tables for models 04_M_Exp_Diet 

### Accessed last: October 2025
######################################################

### packages and data


library(dplyr)
library(tidyr)


m1 <- read.csv("Output/Models/M_All_Pers_diet_040925.csv", header=TRUE)
m2 <- read.csv("Output/Models/M_B_Pers_diet_040925.csv", header=TRUE)
m3 <- read.csv("Output/Models/M_G_Pers_diet_040925.csv", header=TRUE)

################################################################################################ annotations ----

### ALL: both species
### B: blue tits
### G: great tits
### exp: exploration

############################################################################################### ALL exp diet ----
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

### define row order for exploration model
estimate_exp_rows <- c("mu_u",
                       "be_u_Seq2",
                       "be_u_Seq3",
                       "be_u_Seq4",
                       "be_u_Time",
                       "be_u_Sex",
                       "be_u_G",
                       "V_i_u", "V_spy_u", "V_e_u",
                       "VC_i", "VC_spy", "VC_e")

### define row order for diet model
estimate_w_rows <- c("mean_w_propp1_all",
                     "b1_u_propp1_all",
                     "b.1.2.",
                     "b.1.3.",
                     "b.1.4.",
                     "b.1.5.",
                     "b.1.6.",
                     "b_int_u_propp1_all", "b_int2_u_propp1_all","b_int.1.3.","b_int4_u_propp1_all",
                     "V_i_w_propp1_all", "V_spy_w_propp1_all", "V_e_w_propp1_all",
                     "VC_i_w_propp1_all", "VC_spy_w_propp1_all", "VC_e_w_propp1_all",
                     "phi_propp1_all", "pi_propp1_all",
                     "bluetit_effect_propp1_all", "greattit_effect_propp1_all",
                     "estimate1_propp1_all", "estimate2_propp1_all", "estimate3_propp1_all","estimate4_propp1_all",
                     "exploration_bluetit_effect_propp1_all", "exploration_greattit_effect_propp1_all",
                     "exploration_estimate1_propp1_all", "exploration_estimate2_propp1_all",
                     "exploration_estimate3_propp1_all","exploration_estimate4_propp1_all")

### subset and order exploration estimates
df_estimate_exp <- m1_column %>%
  filter(rownames_column %in% estimate_exp_rows) %>%
  slice(match(estimate_exp_rows, rownames_column))

### subset and order diet model estimates
m1_combined1 <- m1_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

### define latex labels for exploration model
df_estimate_exp$Latex_names <- c("\\text{Intercept}", 
                                 "\\text{Trial 1}", 
                                 "\\text{Trial 2}",
                                 "\\text{Trial 3}",
                                 "\\text{Time sunrise}",
                                 "\\text{Great tit}",
                                 "\\text{Individual}","\\text{SubPlotYear}","\\text{Residual}",
                                 "\\text{Individual VC}", "\\text{SubPlotYear VC}", "\\text{Residual VC}")

### define latex labels for diet model
m1_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Exploration}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{Great tit}",
                              "\\text{Exploration * TMB}", "\\text{Exploration * TMG}", "\\text{TMB * TMG}",
                              "\\text{Exploration * TMB * TMG}",
                              "\\text{Individual}", "\\text{SubplotYear}","\\text{Residual}",
                              "\\text{Individual VC}", "\\text{SubplotYear VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\\text{B}_\\text{L}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{L}:\\text{G}_\\text{H}$",
                              "$\\text{B}_\\text{H}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{H}:\\text{G}_\\text{H}$",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\\text{B}_\\text{L}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{L}:\\text{G}_\\text{H}$",
                              "$\\text{B}_\\text{H}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{H}:\\text{G}_\\text{H}$")

### create latex formatted rows for exploration model
df_estimate_exp$Latex_format <- paste(
  df_estimate_exp$Latex_names,  
  " & ",                    
  df_estimate_exp$EstimateH,    
  " \\"                     
)

### create latex formatted rows for diet model
m1_combined1$Latex_format <- paste(
  m1_combined1$Latex_names,  
  " & ",                    
  m1_combined1$EstimateW1,
  " \\"
)

### keep only formatted columns
exp_Latex <- df_estimate_exp %>%
  select(Latex_format)

m1_Latex <- m1_combined1 %>%
  select(Latex_format)

### remove rownames
rownames(exp_Latex) <- NULL
rownames(m1_Latex) <- NULL

### print latex outputs
print(exp_Latex, quote = FALSE, row.names = FALSE) # shows without rownumbers
print(m1_Latex, quote = FALSE, row.names = FALSE)

################################################################################################# B exp diet ----
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

estimate_exp_rows <- c("mu_u",
                       "be_u_Seq2",
                       "be_u_Seq3",
                       "be_u_Seq4",
                       "be_u_Time",
                       "be_u_Sex",
                       "V_i_u", "V_spy_u", "V_e_u",
                       "VC_i", "VC_spy", "VC_e")


estimate_w_rows <- c("mean_w_propp1_B",
                     "b1_u_propp1_B",
                     "b.1.2.",
                     "b.1.3.",
                     "b.1.4.",
                     "b.1.5.",
                     "b_int_u_propp1_B", "b_int2_u_propp1_B","b_int.1.3.","b_int4_u_propp1_B",
                     "V_i_w_propp1_B", "V_spy_w_propp1_B", "V_e_w_propp1_B",
                     "VC_i_w_propp1_B","VC_spy_w_propp1_B", "VC_e_w_propp1_B",
                     "phi_propp1_B", "pi_propp1_B",
                     "bluetit_effect_propp1_B", "greattit_effect_propp1_B",
                     "estimate1_propp1_B", "estimate2_propp1_B", "estimate3_propp1_B","estimate4_propp1_B",
                     "exploration_bluetit_effect_propp1_B", "exploration_greattit_effect_propp1_B",
                     "exploration_estimate1_propp1_B", "exploration_estimate2_propp1_B",
                     "exploration_estimate3_propp1_B","exploration_estimate4_propp1_B")



df_estimate_exp <- m2_column %>%
  filter(rownames_column %in% estimate_exp_rows) %>%
  slice(match(estimate_exp_rows, rownames_column))

m2_combined1 <- m2_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)



df_estimate_exp$Latex_names <- c("\\text{Intercept}", 
                                 "\\text{Trial 1}", 
                                 "\\text{Trial 2}",
                                 "\\text{Trial 3}",
                                 "\\text{Time sunrise}",
                                 "\\text{Sex}",
                                 "\\text{Individual}","\\text{SubPlotYear}","\\text{Residual}",
                                 "\\text{Individual VC}", "\\text{SubPlotYear VC}", "\\text{Residual VC}")


m2_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Exploration}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{Exploration * TMB}", "\\text{Exploration * TMG}", "\\text{TMB * TMG}",
                              "\\text{Exploration * TMB * TMG}",
                              "\\text{Individual}",  "\\text{SubplotYear}","\\text{Residual}",
                              "\\text{Individual VC}","\\text{SubplotYear VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$")

df_estimate_exp$Latex_format <- paste(
  df_estimate_exp$Latex_names,  # Latex names column
  " & ",                    # Add " & "
  df_estimate_exp$EstimateH,    # EstimateH column
  " \\"                     # Add "\\"
)


m2_combined1$Latex_format <- paste(
  m2_combined1$Latex_names,  # Latex names column
  " & ",                    
  m2_combined1$EstimateW1,
  " \\"
)



exp_Latex <- df_estimate_exp %>%
  select(Latex_format)


m2_Latex <- m2_combined1 %>%
  select(Latex_format)

rownames(exp_Latex) <- NULL
rownames(m2_Latex) <- NULL

print(exp_Latex, quote = FALSE, row.names = FALSE) # shows without rownumbers
print(m2_Latex, quote = FALSE, row.names = FALSE)




################################################################################################# G exp diet ----

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

estimate_exp_rows <- c("mu_u",
                       "be_u_Seq2",
                       "be_u_Seq3",
                       "be_u_Seq4",
                       "be_u_Time",
                       "be_u_Sex",
                       "V_i_u", "V_spy_u", "V_e_u",
                       "VC_i", "VC_spy", "VC_e")


estimate_w_rows <- c("mu_w_propp1_G",
                     "b1_u_propp1_G",
                     "b.1.2.",
                     "b.1.3.",
                     "b.1.4.",
                     "b.1.5.",
                     "b_int_u_propp1_G", "b_int2_u_propp1_G","b_int.1.3.","b_int4_u_propp1_G",
                     "V_i_w_propp1_G", "V_spy_w_propp1_G","V_e_w_propp1_G",
                     "VC_i_w_propp1_G","VC_spy_w_propp1_G",  "VC_e_w_propp1_G",
                     "phi_propp1_G", "pi_propp1_G",
                     "bluetit_effect_propp1_G", "greattit_effect_propp1_G",
                     "estimate1_propp1_G", "estimate2_propp1_G", "estimate3_propp1_G","estimate4_propp1_G",
                     "exploration_bluetit_effect_propp1_G", "exploration_greattit_effect_propp1_G",
                     "exploration_estimate1_propp1_G", "exploration_estimate2_propp1_G",
                     "exploration_estimate3_propp1_G","exploration_estimate4_propp1_G")



df_estimate_exp <- m3_column %>%
  filter(rownames_column %in% estimate_exp_rows) %>%
  slice(match(estimate_exp_rows, rownames_column))

m3_combined1 <- m3_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)



df_estimate_exp$Latex_names <- c("\\text{Intercept}", 
                                 "\\text{Trial 1}", 
                                 "\\text{Trial 2}",
                                 "\\text{Trial 3}",
                                 "\\text{Time sunrise}",
                                 "\\text{Sex}",
                                 "\\text{Individual}","\\text{SubPlotYear}","\\text{Residual}",
                                 "\\text{Individual VC}", "\\text{SubPlotYear VC}", "\\text{Residual VC}")


m3_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Exploration}", 
                              "\\text{TMB}",
                              "\\text{TMG}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{Exploration * TMB}", "\\text{Exploration * TMG}", "\\text{TMB * TMG}",
                              "\\text{Exploration * TMB * TMG}",
                              "\\text{Individual}",  "\\text{SubplotYear}","\\text{Residual}",
                              "\\text{Individual VC}","\\text{SubplotYear VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$",
                              "\\text{Blue tit}",  "\\text{Great tit}",
                              "$\text{B}_\text{L}:\text{G}_\text{L}$", "$\text{B}_\text{L}:\text{G}_\text{H}$",
                              "$\text{B}_\text{H}:\text{G}_\text{L}$", "$\text{B}_\text{H}:\text{G}_\text{H}$")

df_estimate_exp$Latex_format <- paste(
  df_estimate_exp$Latex_names,  # Latex names column
  " & ",                    # Add " & "
  df_estimate_exp$EstimateH,    # EstimateH column
  " \\"                     # Add "\\"
)


m3_combined1$Latex_format <- paste(
  m3_combined1$Latex_names,  # Latex names column
  " & ",                    
  m3_combined1$EstimateW1,
  " \\"
)



exp_Latex <- df_estimate_exp %>%
  select(Latex_format)


m3_Latex <- m3_combined1 %>%
  select(Latex_format)

rownames(exp_Latex) <- NULL
rownames(m3_Latex) <- NULL

print(exp_Latex, quote = FALSE, row.names = FALSE) # shows without rownumbers
print(m3_Latex, quote = FALSE, row.names = FALSE)

