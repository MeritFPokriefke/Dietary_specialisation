
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 07#T_Diet_Weight

### Prediction 3: Total brood weight and dietary specialisation
### Latex ready tables for models 06_M

### Accessed last: June 2026
######################################################

### packages and data

library(dplyr)
library(tidyr)


m1 <- read.csv("Output/Models/M_B_Weight_Diet_230626.csv", header=TRUE)
m2 <- read.csv("Output/Models/M_G_Weight_Diet_230626.csv", header=TRUE)


################################################################################################ annotations ----

### B: blue tit
### G: great tit

##################################################################################################### B diet ----
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

### define row order for diet model
estimate_exp_rows <- c("beta_0_prop",
                       "V_brood","V_wnr","V_e",
                       "VC_brood","VC_wnr","VC_e",
                       "phi","pi")

### define row order for fledging success model
estimate_w_rows <- c("beta_0_w_u",
                     "b_u.1.",
                     "b_u.2.",
                     "b_u.3.",
                     "b_u.4.",
                     "b_u.5.",
                     "b_u.6.",
                     "b_u.7.",
                     "b_int_u.1.",
                     "b_int_u.2.",
                     "b_int_u.3.",
                     "b_int_u.4.",
                     "V_spy_w", "V_e_w",
                     "VC_spy_w", "VC_e_w",
                     "bluetit_effect", "greattit_effect",
                     "estimate1", "estimate2", "estimate3","estimate4",
                     "sel_bluetit_effect", "sel_greattit_effect",
                     "sel_1", "sel_2", "sel_3","sel_4")

### subset and order diet estimates
df_estimate_exp <- m1_column %>%
  filter(rownames_column %in% estimate_exp_rows) %>%
  slice(match(estimate_exp_rows, rownames_column))

### subset and order fledging success model estimates
m1_combined1 <- m1_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

### define latex labels for diet model
df_estimate_exp$Latex_names <- c("\\text{Intercept}", 
                                 "\\text{Brood ID}","\\text{Nestling repeat}","\\text{Residual}",
                                 "\\text{Brood ID VC}", "\\text{Nestling repeat VC}", "\\text{Residual VC}",
                                 "\\text{phi}",  "\\text{pi}")

### define latex labels for fledging success model
m1_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Proportion of preferred prey}", 
                              "\\text{Treatment B}",
                              "\\text{Treatment G}",
                              "\\text{Double box}",
                              "\\text{Year 2024}",
                              "\\text{Time weight}",
                              "\\text{Brood size}",
                              "\\text{Proportion of preferred prey * Treatment B}", "\\text{Proportion of preferred prey * Treatment G}", "\\text{Treatment B * Treatment G}",
                              "\\text{Proportion of preferred prey * Treatment B * Treatment G}",
                              "\\text{SubplotYear}", "\\text{Residual}",
                              "\\text{SubplotYear}", "\\text{Residual}",
                              # effects of weight
                              "\\text{Treatment B}",  "\\text{Treatment G}",
                              "$\\text{B}_\\text{L}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{L}:\\text{G}_\\text{H}$",
                              "$\\text{B}_\\text{H}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{H}:\\text{G}_\\text{H}$",
                              # selection effects of diet
                              "\\text{Treatment B}",  "\\text{Treatment G}",
                              "$\\text{B}_\\text{L}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{L}:\\text{G}_\\text{H}$",
                              "$\\text{B}_\\text{H}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{H}:\\text{G}_\\text{H}$"
)

### create latex formatted rows for diet model
df_estimate_exp$Latex_format <- paste(
  df_estimate_exp$Latex_names,  
  " & ",                    
  df_estimate_exp$EstimateH,    
  " \\"                     
)

### create latex formatted rows for fledging success model
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

##################################################################################################### G diet ----
### select rows 2 to 4
m2 <- m2[c(2:4),]

### transpose matrix
m2_transposed <- t(m2)

colnames(m2_transposed) <- m2_transposed[1, ]# take the first row as column names

m2_transposed <- m2_transposed[-1, ] # remove the first row (which is now the column names)

### convert to data frame
m2_df <- as.data.frame(m2_transposed)

m2_df$rownames_column <- rownames(m2_df) # make row names into a column


m2_df <- m2_df %>%
  mutate(across(c(`2.5%`, `50%`, `97.5%`), ~ format(round(as.numeric(.), 3), nsmall = 3))) # concerts to numeric then rounds to 3rd decimal

### create formatted estimate string
m2_column <- m2_df %>%
  mutate(EstimateH = paste(`50%`, "[", `2.5%`, ";", `97.5%`, "]", sep = " ")) %>%
  select(EstimateH, rownames_column)

### rearrange df

### define row order for diet model
estimate_exp_rows <- c("beta_0_prop",
                       "V_brood","V_wnr","V_e",
                       "VC_brood","VC_wnr","VC_e",
                       "phi","pi")

### define row order for fledging success model
estimate_w_rows <- c("beta_0_w_u",
                     "b_u.1.",
                     "b_u.2.",
                     "b_u.3.",
                     "b_u.4.",
                     "b_u.5.",
                     "b_u.6.",
                     "b_u.7.",
                     "b_int_u.1.",
                     "b_int_u.2.",
                     "b_int_u.3.",
                     "b_int_u.4.",
                     "V_spy_w", "V_e_w",
                     "VC_spy_w", "VC_e_w",
                     "bluetit_effect", "greattit_effect",
                     "estimate1", "estimate2", "estimate3","estimate4",
                     "sel_bluetit_effect", "sel_greattit_effect",
                     "sel_1", "sel_2", "sel_3","sel_4")

### subset and order diet estimates
df_estimate_exp <- m2_column %>%
  filter(rownames_column %in% estimate_exp_rows) %>%
  slice(match(estimate_exp_rows, rownames_column))

### subset and order fledging success model estimates
m2_combined1 <- m2_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

### define latex labels for diet model
df_estimate_exp$Latex_names <- c("\\text{Intercept}", 
                                 "\\text{Brood ID}","\\text{Nestling repeat}","\\text{Residual}",
                                 "\\text{Brood ID VC}", "\\text{Nestling repeat VC}", "\\text{Residual VC}",
                                 "\\text{phi}",  "\\text{pi}")

### define latex labels for fledging success model
m2_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Proportion of preferred prey}", 
                              "\\text{Treatment B}",
                              "\\text{Treatment G}",
                              "\\text{Double box}",
                              "\\text{Year 2024}",
                              "\\text{Time weight}",
                              "\\text{Brood size}",
                              "\\text{Proportion of preferred prey * Treatment B}", "\\text{Proportion of preferred prey * Treatment G}", "\\text{Treatment B * Treatment G}",
                              "\\text{Proportion of preferred prey * Treatment B * Treatment G}",
                              "\\text{SubplotYear}", "\\text{Residual}",
                              "\\text{SubplotYear}", "\\text{Residual VC}",
                              # effects of weight
                              "\\text{Treatment B}",  "\\text{Treatment G}",
                              "$\\text{B}_\\text{L}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{L}:\\text{G}_\\text{H}$",
                              "$\\text{B}_\\text{H}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{H}:\\text{G}_\\text{H}$",
                              # selection effects of diet
                              "\\text{Treatment B}",  "\\text{Treatment G}",
                              "$\\text{B}_\\text{L}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{L}:\\text{G}_\\text{H}$",
                              "$\\text{B}_\\text{H}:\\text{G}_\\text{L}$", "$\\text{B}_\\text{H}:\\text{G}_\\text{H}$"
)

### create latex formatted rows for diet model
df_estimate_exp$Latex_format <- paste(
  df_estimate_exp$Latex_names,  
  " & ",                    
  df_estimate_exp$EstimateH,    
  " \\"                     
)

### create latex formatted rows for fledging success model
m2_combined1$Latex_format <- paste(
  m2_combined1$Latex_names,  
  " & ",                    
  m2_combined1$EstimateW1,
  " \\"
)

### keep only formatted columns
exp_Latex <- df_estimate_exp %>%
  select(Latex_format)

m2_Latex <- m2_combined1 %>%
  select(Latex_format)

### remove rownames
rownames(exp_Latex) <- NULL
rownames(m2_Latex) <- NULL

### print latex outputs
print(exp_Latex, quote = FALSE, row.names = FALSE) # shows without rownumbers
print(m2_Latex, quote = FALSE, row.names = FALSE)