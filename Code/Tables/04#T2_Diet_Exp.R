
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 04#T2_Diet_Exp

### Prediction 2: Association prey preference and personality (nestlings)
### Latex ready tables for models 04_M2_Exp_Diet_C 

### Accessed last: September 2025
######################################################

### packages and data

library(dplyr)
library(tidyr)

### blue tits
m1 <- read.csv("Output/Models/M_B_Female_Pers_dietC_040925.csv", header=TRUE)
m2 <- read.csv("Output/Models/M_B_Male_Pers_dietC_040925.csv", header=TRUE)

## great tits
m3 <- read.csv("Output/Models/M_G_Female_Pers_dietC_040925.csv", header=TRUE)
m4 <- read.csv("Output/Models/M_G_Male_Pers_dietC_040925.csv", header=TRUE)


################################################################################################ annotations ----

### F: Female
### M: Male
### B: blue tit
### G: great tit
### exp: exploration

################################################################################################ FB exp diet ----
### select rows 2 to 4
m1 <- m1[c(2:4),]

### transpose matrix
m1_transposed <- t(m1)

### set first row as column names
colnames(m1_transposed) <- m1_transposed[1, ]# take the first row as column names

m1_transposed <- m1_transposed[-1, ] # remove the first row (which is now the column names)

### convert to data frame
m1_df <- as.data.frame(m1_transposed)

m1_df$rownames_column <- rownames(m1_df) # make row names into a column

### convert to numeric and round to 3 decimals
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
                       "V_i_u", "V_spy_u", "V_e_u",
                       "VC_i", "VC_spy", "VC_e")

### define row order for diet model
estimate_w_rows <- c("mean_w_propp1_B",
                     "b1_u_propp1_B",
                     "b.1.2.",
                     "b.1.3.",
                     "b.1.4.",
                     "b_int_u_propp1_B",
                     "V_i_w_propp1_B", "V_wnr_w_propp1_B", "V_spy_w_propp1_B", "V_e_w_propp1_B",
                     "VC_i_w_propp1_B", "VC_wnr_w_propp1_B", "VC_spy_w_propp1_B", "VC_e_w_propp1_B",
                     "phi_propp1_B", "pi_propp1_B")

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
                                 "\\text{Individual}","\\text{SubPlotYear}","\\text{Residual}",
                                 "\\text{Individual VC}", "\\text{SubPlotYear VC}", "\\text{Residual VC}")

### define latex labels for diet model
m1_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Exploration}", 
                              "\\text{Chick}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{Exploration * Chick}",
                              "\\text{Female}", "\\text{WithinNestRep}", "\\text{SubplotYear}","\\text{Residual}",
                              "\\text{Female VC}", "\\text{WithinNestRep VC}","\\text{SubplotYear VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}")

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

### remove row names
rownames(exp_Latex) <- NULL
rownames(m1_Latex) <- NULL

### print latex outputs
print(exp_Latex, quote = FALSE, row.names = FALSE) # shows without row numbers
print(m1_Latex, quote = FALSE, row.names = FALSE)

################################################################################################ MB exp diet ----

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
                       "V_i_u", "V_spy_u", "V_e_u",
                       "VC_i", "VC_spy", "VC_e")


estimate_w_rows <- c("mu_w_propp1_B",
                     "b1_u_propp1_B",
                     "b.1.2.",
                     "b.1.3.",
                     "b.1.4.",
                     "b_int_u_propp1_B",
                     "V_i_w_propp1_B", "V_wnr_w_propp1_B", "V_spy_w_propp1_B","V_e_w_propp1_B",
                     "VC_i_w_propp1_B", "VC_wnr_w_propp1_B", "VC_spy_w_propp1_B",  "VC_e_w_propp1_B",
                     "phi_propp1_B", "pi_propp1_B")



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
                                 "\\text{Individual}","\\text{SubPlotYear}","\\text{Residual}",
                                 "\\text{Individual VC}", "\\text{SubPlotYear VC}", "\\text{Residual VC}")


m2_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Exploration}", 
                              "\\text{Chick}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{Exploration * Chick}",
                              "\\text{Male}", "\\text{WithinNestRep}", "\\text{SubplotYear}","\\text{Residual}",
                              "\\text{Male VC}", "\\text{WithinNestRep VC}","\\text{SubplotYear VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}")

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


################################################################################################ FG exp diet ----
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
                       "V_i_u", "V_spy_u", "V_e_u",
                       "VC_i", "VC_spy", "VC_e")


estimate_w_rows <- c("mean_w_propp1_G",
                     "b1_u_propp1_G",
                     "b.1.2.",
                     "b.1.3.",
                     "b.1.4.",
                     "b_int_u_propp1_G",
                     "V_i_w_propp1_G", "V_wnr_w_propp1_G", "V_spy_w_propp1_G", "V_e_w_propp1_G",
                     "VC_i_w_propp1_G", "VC_wnr_w_propp1_G", "VC_spy_w_propp1_G", "VC_e_w_propp1_G",
                     "phi_propp1_G", "pi_propp1_G")



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
                                 "\\text{Individual}","\\text{SubPlotYear}","\\text{Residual}",
                                 "\\text{Individual VC}", "\\text{SubPlotYear VC}", "\\text{Residual VC}")


m3_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Exploration}", 
                              "\\text{Chick}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{Exploration * Chick}",
                              "\\text{Female}", "\\text{WithinNestRep}", "\\text{SubplotYear}","\\text{Residual}",
                              "\\text{Female VC}", "\\text{WithinNestRep VC}","\\text{SubplotYear VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}")

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




################################################################################################ MB exp diet ----

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
estimate_exp_rows <- c("mu_u",
                       "be_u_Seq2",
                       "be_u_Seq3",
                       "be_u_Seq4",
                       "be_u_Time",
                       "V_i_u", "V_spy_u", "V_e_u",
                       "VC_i", "VC_spy", "VC_e")


estimate_w_rows <- c("mu_w_propp1_G",
                     "b1_u_propp1_G",
                     "b.1.2.",
                     "b.1.3.",
                     "b.1.4.",
                     "b_int_u_propp1_G",
                     "V_i_w_propp1_G", "V_wnr_w_propp1_G", "V_spy_w_propp1_G","V_e_w_propp1_G",
                     "VC_i_w_propp1_G", "VC_wnr_w_propp1_G", "VC_spy_w_propp1_G",  "VC_e_w_propp1_G",
                     "phi_propp1_G", "pi_propp1_G")



df_estimate_exp <- m4_column %>%
  filter(rownames_column %in% estimate_exp_rows) %>%
  slice(match(estimate_exp_rows, rownames_column))


m4_combined1 <- m4_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)



df_estimate_exp$Latex_names <- c("\\text{Intercept}", 
                                 "\\text{Trial 1}", 
                                 "\\text{Trial 2}",
                                 "\\text{Trial 3}",
                                 "\\text{Time sunrise}",
                                 "\\text{Individual}","\\text{SubPlotYear}","\\text{Residual}",
                                 "\\text{Individual VC}", "\\text{SubPlotYear VC}", "\\text{Residual VC}")


m4_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Exploration}", 
                              "\\text{Chick}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{Exploration * Chick}",
                              "\\text{Male}", "\\text{WithinNestRep}", "\\text{SubplotYear}","\\text{Residual}",
                              "\\text{Male VC}", "\\text{WithinNestRep VC}","\\text{SubplotYear VC}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}")

df_estimate_exp$Latex_format <- paste(
  df_estimate_exp$Latex_names,  # Latex names column
  " & ",                    # Add " & "
  df_estimate_exp$EstimateH,    # EstimateH column
  " \\"                     # Add "\\"
)


m4_combined1$Latex_format <- paste(
  m4_combined1$Latex_names,  # Latex names column
  " & ",                    
  m4_combined1$EstimateW1,
  " \\"
)



exp_Latex <- df_estimate_exp %>%
  select(Latex_format)


m4_Latex <- m4_combined1 %>%
  select(Latex_format)

rownames(exp_Latex) <- NULL
rownames(m4_Latex) <- NULL

print(exp_Latex, quote = FALSE, row.names = FALSE) # shows without rownumbers
print(m4_Latex, quote = FALSE, row.names = FALSE)

