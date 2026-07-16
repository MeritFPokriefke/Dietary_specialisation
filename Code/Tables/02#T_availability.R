
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 02#T_availability

### Identification prefered prey in diet: Existence of prey preferences 
### Latex ready tables for model 02_availability (1 table per five arthropod families)

### Accessed last: September 2025
######################################################

### packages and data

library(dplyr)
library(tidyr)


m1 <- read.csv("Output/Models/M_avail_020925.csv", header=TRUE)

################################################## Noctuidae, Erebidae, Geometridae, Tineidae, Lasiocampidae ----
### select rows 2 to 4
m1 <- m1[c(2:4),]

### transpose matrix
m1_transposed <- t(m1)

### set first row as column names
colnames(m1_transposed) <- m1_transposed[1, ]# take the first row as column names

### remove first row after using as header
m1_transposed <- m1_transposed[-1, ] # remove the first row (which is now the column names)

### convert to data frame
m1_df <- as.data.frame(m1_transposed)

m1_df$rownames_column <- rownames(m1_df) # make row names into a column

m1_df <- m1_df %>%
  mutate(across(c(`2.5%`, `50%`, `97.5%`), ~ format(round(as.numeric(.), 3), nsmall = 3))) # concerts to numeric then rounds to 3rd decimal

### create formatted estimate with interval
m1_column <- m1_df %>%
  mutate(EstimateH = paste(`50%`, "[", `2.5%`, ";", `97.5%`, "]", sep = " ")) %>%
  select(EstimateH, rownames_column)

### rearrange df

### define row order for noctuidae
# 1. Noctuidae
estimate_w_rows <- c(
  "mean_w_Noctuidae",
  "bw.1.1.",
  "bw.2.1.",
  "bw.3.1.",
  "bw.4.1.",
  "V_spy_Noctuidae", "V_locyear_Noctuidae", "V_e_Noctuidae",
  "VC_spy_Noctuidae", "VC_locyear_Noctuidae", "VC_e_Noctuidae",
  "phi_Noctuidae", "pi_Noctuidae"
)

### define row order for erebidae
# 2. Erebidae
estimate_w2_rows <- c(
  "mean_w_Erebidae",
  "bw.1.2.",
  "bw.2.2.",
  "bw.3.2.",
  "bw.4.2.",
  "V_spy_Erebidae", "V_locyear_Erebidae", "V_e_Erebidae",
  "VC_spy_Erebidae", "VC_locyear_Erebidae", "VC_e_Erebidae",
  "phi_Erebidae", "pi_Erebidae"
)

### define row order for geometridae
# 3. Geometridae
estimate_w3_rows <- c(
  "mean_w_Geometridae",
  "bw.1.3.",
  "bw.2.3.",
  "bw.3.3.",
  "bw.4.3.",
  "V_spy_Geometridae", "V_locyear_Geometridae", "V_e_Geometridae",
  "VC_spy_Geometridae", "VC_locyear_Geometridae", "VC_e_Geometridae",
  "phi_Geometridae", "pi_Geometridae"
)

### define row order for tineidae
# 4. Tineidae
estimate_w4_rows <- c(
  "mean_w_Tineidae",
  "bw.1.4.",
  "bw.2.4.",
  "bw.3.4.",
  "bw.4.4.",
  "V_spy_Tineidae", "V_locyear_Tineidae", "V_e_Tineidae",
  "VC_spy_Tineidae", "VC_locyear_Tineidae", "VC_e_Tineidae",
  "phi_Tineidae", "pi_Tineidae"
)

### define row order for lasiocampidae
# 5. Lasiocampidae
estimate_w5_rows <- c(
  "mean_w_Lasiocampidae",
  "bw.1.5.",
  "bw.2.5.",
  "bw.3.5.",
  "bw.4.5.",
  "V_spy_Lasiocampidae", "V_locyear_Lasiocampidae", "V_e_Lasiocampidae",
  "VC_spy_Lasiocampidae", "VC_locyear_Lasiocampidae", "VC_e_Lasiocampidae",
  "phi_Lasiocampidae", "pi_Lasiocampidae"
)

### subset and order noctuidae results
# W1
m1_combined1 <- m1_column %>%
  filter(rownames_column %in% estimate_w_rows) %>%
  slice(match(estimate_w_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

### subset and order erebidae results
# W2 
m1_combined2 <- m1_column %>%
  filter(rownames_column %in% estimate_w2_rows) %>%
  slice(match(estimate_w2_rows, rownames_column)) %>%
  rename(EstimateW2 = EstimateH)

### subset and order geometridae results
# W3 
m1_combined3 <- m1_column %>%
  filter(rownames_column %in% estimate_w3_rows) %>%
  slice(match(estimate_w3_rows, rownames_column)) %>%
  rename(EstimateW3 = EstimateH)

### subset and order tineidae results
# W4 
m1_combined4 <- m1_column %>%
  filter(rownames_column %in% estimate_w4_rows) %>%
  slice(match(estimate_w4_rows, rownames_column)) %>%
  rename(EstimateW4 = EstimateH)

### subset and order lasiocampidae results
# W5
m1_combined5 <- m1_column %>%
  filter(rownames_column %in% estimate_w5_rows) %>%
  slice(match(estimate_w5_rows, rownames_column)) %>%
  rename(EstimateW5 = EstimateH)

### define latex labels
m1_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Blue tit}",
                              "\\text{Great tit}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{SubPlotYear}", "\\text{LocationYear}", "\\text{Residual}",
                              "\\text{SubPlotYear VC}", "\\text{LocationYear}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}")

### combine labels and estimates into latex rows
m1_combined1$Latex_format <- paste(
  m1_combined1$Latex_names,  # Latex names column
  " & ",                    
  m1_combined1$EstimateW1, " & ",
  m1_combined2$EstimateW2, " & ",
  m1_combined3$EstimateW3, " & ",
  m1_combined4$EstimateW4, " & ",
  m1_combined5$EstimateW5,
  " \\"
)

### keep only formatted column
m1_Latex <- m1_combined1 %>%
  select(Latex_format)

### remove rownames
rownames(m1_Latex) <- NULL

### print latex output
print(m1_Latex, quote = FALSE, row.names = FALSE)


############################################ Chrysomelidae, Ypsolophidae, Tortricidae, Lycaenidae, Pyralidae ----

### rearrange df

# 6. Chrysomelidae
estimate_w6_rows <- c(
  "mean_w_Chrysomelidae",
  "bw.1.6.",
  "bw.2.6.",
  "bw.3.6.",
  "bw.4.6.",
  "V_spy_Chrysomelidae", "V_locyear_Chrysomelidae", "V_e_Chrysomelidae",
  "VC_spy_Chrysomelidae", "VC_locyear_Chrysomelidae", "VC_e_Chrysomelidae",
  "phi_Chrysomelidae", "pi_Chrysomelidae"
)

# 7. Ypsolophidae
estimate_w7_rows <- c(
  "mean_w_Ypsolophidae",
  "bw.1.7.",
  "bw.2.7.",
  "bw.3.7.",
  "bw.4.7.",
  "V_spy_Ypsolophidae", "V_locyear_Ypsolophidae", "V_e_Ypsolophidae",
  "VC_spy_Ypsolophidae", "VC_locyear_Ypsolophidae", "VC_e_Ypsolophidae",
  "phi_Ypsolophidae", "pi_Ypsolophidae"
)

# 8. Tortricidae
estimate_w8_rows <- c(
  "mean_w_Tortricidae",
  "bw.1.8.",
  "bw.2.8.",
  "bw.3.8.",
  "bw.4.8.",
  "V_spy_Tortricidae", "V_locyear_Tortricidae", "V_e_Tortricidae",
  "VC_spy_Tortricidae", "VC_locyear_Tortricidae", "VC_e_Tortricidae",
  "phi_Tortricidae", "pi_Tortricidae"
)

# 9. Lycaenidae
estimate_w9_rows <- c(
  "mean_w_Lycaenidae",
  "bw.1.9.",
  "bw.2.9.",
  "bw.3.9.",
  "bw.4.9.",
  "V_spy_Lycaenidae", "V_locyear_Lycaenidae", "V_e_Lycaenidae",
  "VC_spy_Lycaenidae", "VC_locyear_Lycaenidae", "VC_e_Lycaenidae",
  "phi_Lycaenidae", "pi_Lycaenidae"
)

# 10. Pyralidae
estimate_w10_rows <- c(
  "mean_w_Pyralidae",
  "bw.1.10.",
  "bw.2.10.",
  "bw.3.10.",
  "bw.4.10.",
  "V_spy_Pyralidae", "V_locyear_Pyralidae", "V_e_Pyralidae",
  "VC_spy_Pyralidae", "VC_locyear_Pyralidae", "VC_e_Pyralidae",
  "phi_Pyralidae", "pi_Pyralidae"
)



# W1
m1_combined1 <- m1_column %>%
  filter(rownames_column %in% estimate_w6_rows) %>%
  slice(match(estimate_w6_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

# W2 
m1_combined2 <- m1_column %>%
  filter(rownames_column %in% estimate_w7_rows) %>%
  slice(match(estimate_w7_rows, rownames_column)) %>%
  rename(EstimateW2 = EstimateH)

# W3 
m1_combined3 <- m1_column %>%
  filter(rownames_column %in% estimate_w8_rows) %>%
  slice(match(estimate_w8_rows, rownames_column)) %>%
  rename(EstimateW3 = EstimateH)

# W4 
m1_combined4 <- m1_column %>%
  filter(rownames_column %in% estimate_w9_rows) %>%
  slice(match(estimate_w9_rows, rownames_column)) %>%
  rename(EstimateW4 = EstimateH)

# W5
m1_combined5 <- m1_column %>%
  filter(rownames_column %in% estimate_w10_rows) %>%
  slice(match(estimate_w10_rows, rownames_column)) %>%
  rename(EstimateW5 = EstimateH)



m1_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Blue tit}",
                              "\\text{Great tit}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{SubPlotYear}", "\\text{LocationYear}", "\\text{Residual}",
                              "\\text{SubPlotYear VC}", "\\text{LocationYear}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}")



m1_combined1$Latex_format <- paste(
  m1_combined1$Latex_names,  # Latex names column
  " & ",                    
  m1_combined1$EstimateW1, " & ",
  m1_combined2$EstimateW2, " & ",
  m1_combined3$EstimateW3, " & ",
  m1_combined4$EstimateW4, " & ",
  m1_combined5$EstimateW5,
  " \\"
)



m1_Latex <- m1_combined1 %>%
  select(Latex_format)
rownames(m1_Latex) <- NULL
print(m1_Latex, quote = FALSE, row.names = FALSE)



########################################### Drepanidae, Gelechiidae, Argyresthiidae, Lypusidae, Notodontidae ----

### rearrange df

# 11. Drepanidae
estimate_w11_rows <- c(
  "mean_w_Drepanidae",
  "bw.1.11.",
  "bw.2.11.",
  "bw.3.11.",
  "bw.4.11.",
  "V_spy_Drepanidae", "V_locyear_Drepanidae", "V_e_Drepanidae",
  "VC_spy_Drepanidae", "VC_locyear_Drepanidae", "VC_e_Drepanidae",
  "phi_Drepanidae", "pi_Drepanidae"
)

# 12. Gelechiidae
estimate_w12_rows <- c(
  "mean_w_Gelechiidae",
  "bw.1.12.",
  "bw.2.12.",
  "bw.3.12.",
  "bw.4.12.",
  "V_spy_Gelechiidae", "V_locyear_Gelechiidae", "V_e_Gelechiidae",
  "VC_spy_Gelechiidae", "VC_locyear_Gelechiidae", "VC_e_Gelechiidae",
  "phi_Gelechiidae", "pi_Gelechiidae"
)

# 13. Argyresthiidae
estimate_w13_rows <- c(
  "mean_w_Argyresthiidae",
  "bw.1.13.",
  "bw.2.13.",
  "bw.3.13.",
  "bw.4.13.",
  "V_spy_Argyresthiidae", "V_locyear_Argyresthiidae", "V_e_Argyresthiidae",
  "VC_spy_Argyresthiidae", "VC_locyear_Argyresthiidae", "VC_e_Argyresthiidae",
  "phi_Argyresthiidae", "pi_Argyresthiidae"
)

# 14. Lypusidae
estimate_w14_rows <- c(
  "mean_w_Lypusidae",
  "bw.1.14.",
  "bw.2.14.",
  "bw.3.14.",
  "bw.4.14.",
  "V_spy_Lypusidae", "V_locyear_Lypusidae", "V_e_Lypusidae",
  "VC_spy_Lypusidae", "VC_locyear_Lypusidae", "VC_e_Lypusidae",
  "phi_Lypusidae", "pi_Lypusidae"
)

# 15. Notodontidae
estimate_w15_rows <- c(
  "mean_w_Notodontidae",
  "bw.1.15.",
  "bw.2.15.",
  "bw.3.15.",
  "bw.4.15.",
  "V_spy_Notodontidae", "V_locyear_Notodontidae", "V_e_Notodontidae",
  "VC_spy_Notodontidae", "VC_locyear_Notodontidae", "VC_e_Notodontidae",
  "phi_Notodontidae", "pi_Notodontidae"
)



# W1
m1_combined1 <- m1_column %>%
  filter(rownames_column %in% estimate_w11_rows) %>%
  slice(match(estimate_w11_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

# W2 
m1_combined2 <- m1_column %>%
  filter(rownames_column %in% estimate_w12_rows) %>%
  slice(match(estimate_w12_rows, rownames_column)) %>%
  rename(EstimateW2 = EstimateH)

# W3 
m1_combined3 <- m1_column %>%
  filter(rownames_column %in% estimate_w13_rows) %>%
  slice(match(estimate_w13_rows, rownames_column)) %>%
  rename(EstimateW3 = EstimateH)

# W4 
m1_combined4 <- m1_column %>%
  filter(rownames_column %in% estimate_w14_rows) %>%
  slice(match(estimate_w14_rows, rownames_column)) %>%
  rename(EstimateW4 = EstimateH)

# W5
m1_combined5 <- m1_column %>%
  filter(rownames_column %in% estimate_w15_rows) %>%
  slice(match(estimate_w15_rows, rownames_column)) %>%
  rename(EstimateW5 = EstimateH)



m1_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Blue tit}",
                              "\\text{Great tit}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{SubPlotYear}", "\\text{LocationYear}", "\\text{Residual}",
                              "\\text{SubPlotYear VC}", "\\text{LocationYear}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}")



m1_combined1$Latex_format <- paste(
  m1_combined1$Latex_names,  # Latex names column
  " & ",                    
  m1_combined1$EstimateW1, " & ",
  m1_combined2$EstimateW2, " & ",
  m1_combined3$EstimateW3, " & ",
  m1_combined4$EstimateW4, " & ",
  m1_combined5$EstimateW5,
  " \\"
)



m1_Latex <- m1_combined1 %>%
  select(Latex_format)
rownames(m1_Latex) <- NULL
print(m1_Latex, quote = FALSE, row.names = FALSE)





############################################ Nolidae, Glyphipterigidae, Adelidae, Hesperiidae, Coleophoridae ----

### rearrange df

# 16. Nolidae
estimate_w16_rows <- c(
  "mean_w_Nolidae",
  "bw.1.16.",
  "bw.2.16.",
  "bw.3.16.",
  "bw.4.16.",
  "V_spy_Nolidae", "V_locyear_Nolidae", "V_e_Nolidae",
  "VC_spy_Nolidae", "VC_locyear_Nolidae", "VC_e_Nolidae",
  "phi_Nolidae", "pi_Nolidae"
)

# 17. Glyphipterigidae
estimate_w17_rows <- c(
  "mean_w_Glyphipterigidae",
  "bw.1.17.",
  "bw.2.17.",
  "bw.3.17.",
  "bw.4.17.",
  "V_spy_Glyphipterigidae", "V_locyear_Glyphipterigidae", "V_e_Glyphipterigidae",
  "VC_spy_Glyphipterigidae", "VC_locyear_Glyphipterigidae", "VC_e_Glyphipterigidae",
  "phi_Glyphipterigidae", "pi_Glyphipterigidae"
)

# 18. Adelidae
estimate_w18_rows <- c(
  "mean_w_Adelidae",
  "bw.1.18.",
  "bw.2.18.",
  "bw.3.18.",
  "bw.4.18.",
  "V_spy_Adelidae", "V_locyear_Adelidae", "V_e_Adelidae",
  "VC_spy_Adelidae", "VC_locyear_Adelidae", "VC_e_Adelidae",
  "phi_Adelidae", "pi_Adelidae"
)

# 19. Hesperiidae
estimate_w19_rows <- c(
  "mean_w_Hesperiidae",
  "bw.1.19.",
  "bw.2.19.",
  "bw.3.19.",
  "bw.4.19.",
  "V_spy_Hesperiidae", "V_locyear_Hesperiidae", "V_e_Hesperiidae",
  "VC_spy_Hesperiidae", "VC_locyear_Hesperiidae", "VC_e_Hesperiidae",
  "phi_Hesperiidae", "pi_Hesperiidae"
)

# 20. Coleophoridae
estimate_w20_rows <- c(
  "mean_w_Coleophoridae",
  "bw.1.20.",
  "bw.2.20.",
  "bw.3.20.",
  "bw.4.20.",
  "V_spy_Coleophoridae", "V_locyear_Coleophoridae", "V_e_Coleophoridae",
  "VC_spy_Coleophoridae", "VC_locyear_Coleophoridae", "VC_e_Coleophoridae",
  "phi_Coleophoridae", "pi_Coleophoridae"
)



# W1
m1_combined1 <- m1_column %>%
  filter(rownames_column %in% estimate_w16_rows) %>%
  slice(match(estimate_w16_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

# W2 
m1_combined2 <- m1_column %>%
  filter(rownames_column %in% estimate_w17_rows) %>%
  slice(match(estimate_w17_rows, rownames_column)) %>%
  rename(EstimateW2 = EstimateH)

# W3 
m1_combined3 <- m1_column %>%
  filter(rownames_column %in% estimate_w18_rows) %>%
  slice(match(estimate_w18_rows, rownames_column)) %>%
  rename(EstimateW3 = EstimateH)

# W4 
m1_combined4 <- m1_column %>%
  filter(rownames_column %in% estimate_w19_rows) %>%
  slice(match(estimate_w19_rows, rownames_column)) %>%
  rename(EstimateW4 = EstimateH)

# W5
m1_combined5 <- m1_column %>%
  filter(rownames_column %in% estimate_w20_rows) %>%
  slice(match(estimate_w20_rows, rownames_column)) %>%
  rename(EstimateW5 = EstimateH)



m1_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Blue tit}",
                              "\\text{Great tit}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{SubPlotYear}", "\\text{LocationYear}", "\\text{Residual}",
                              "\\text{SubPlotYear VC}", "\\text{LocationYear}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}")



m1_combined1$Latex_format <- paste(
  m1_combined1$Latex_names,  # Latex names column
  " & ",                    
  m1_combined1$EstimateW1, " & ",
  m1_combined2$EstimateW2, " & ",
  m1_combined3$EstimateW3, " & ",
  m1_combined4$EstimateW4, " & ",
  m1_combined5$EstimateW5,
  " \\"
)



m1_Latex <- m1_combined1 %>%
  select(Latex_format)
rownames(m1_Latex) <- NULL
print(m1_Latex, quote = FALSE, row.names = FALSE)

################################################# Psychidae, Eriocraniidae, Crambidae, Argidae, Pamphiliidae ----

### rearrange df

# 21. Psychidae
estimate_w21_rows <- c(
  "mean_w_Psychidae",
  "bw.1.21.",
  "bw.2.21.",
  "bw.3.21.",
  "bw.4.21.",
  "V_spy_Psychidae", "V_locyear_Psychidae", "V_e_Psychidae",
  "VC_spy_Psychidae", "VC_locyear_Psychidae", "VC_e_Psychidae",
  "phi_Psychidae", "pi_Psychidae"
)

# 22. Eriocraniidae
estimate_w22_rows <- c(
  "mean_w_Eriocraniidae",
  "bw.1.22.",
  "bw.2.22.",
  "bw.3.22.",
  "bw.4.22.",
  "V_spy_Eriocraniidae", "V_locyear_Eriocraniidae", "V_e_Eriocraniidae",
  "VC_spy_Eriocraniidae", "VC_locyear_Eriocraniidae", "VC_e_Eriocraniidae",
  "phi_Eriocraniidae", "pi_Eriocraniidae"
)

# 23. Crambidae
estimate_w23_rows <- c(
  "mean_w_Crambidae",
  "bw.1.23.",
  "bw.2.23.",
  "bw.3.23.",
  "bw.4.23.",
  "V_spy_Crambidae", "V_locyear_Crambidae", "V_e_Crambidae",
  "VC_spy_Crambidae", "VC_locyear_Crambidae", "VC_e_Crambidae",
  "phi_Crambidae", "pi_Crambidae"
)

# 24. Argidae
estimate_w24_rows <- c(
  "mean_w_Argidae",
  "bw.1.24.",
  "bw.2.24.",
  "bw.3.24.",
  "bw.4.24.",
  "V_spy_Argidae", "V_locyear_Argidae", "V_e_Argidae",
  "VC_spy_Argidae", "VC_locyear_Argidae", "VC_e_Argidae",
  "phi_Argidae", "pi_Argidae"
)

# 25. Pamphiliidae
estimate_w25_rows <- c(
  "mean_w_Pamphiliidae",
  "bw.1.25.",
  "bw.2.25.",
  "bw.3.25.",
  "bw.4.25.",
  "V_spy_Pamphiliidae", "V_locyear_Pamphiliidae", "V_e_Pamphiliidae",
  "VC_spy_Pamphiliidae", "VC_locyear_Pamphiliidae", "VC_e_Pamphiliidae",
  "phi_Pamphiliidae", "pi_Pamphiliidae"
)



# W1
m1_combined1 <- m1_column %>%
  filter(rownames_column %in% estimate_w21_rows) %>%
  slice(match(estimate_w21_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

# W2 
m1_combined2 <- m1_column %>%
  filter(rownames_column %in% estimate_w22_rows) %>%
  slice(match(estimate_w22_rows, rownames_column)) %>%
  rename(EstimateW2 = EstimateH)

# W3 
m1_combined3 <- m1_column %>%
  filter(rownames_column %in% estimate_w23_rows) %>%
  slice(match(estimate_w23_rows, rownames_column)) %>%
  rename(EstimateW3 = EstimateH)

# W4 
m1_combined4 <- m1_column %>%
  filter(rownames_column %in% estimate_w24_rows) %>%
  slice(match(estimate_w24_rows, rownames_column)) %>%
  rename(EstimateW4 = EstimateH)

# W5
m1_combined5 <- m1_column %>%
  filter(rownames_column %in% estimate_w25_rows) %>%
  slice(match(estimate_w25_rows, rownames_column)) %>%
  rename(EstimateW5 = EstimateH)



m1_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Blue tit}",
                              "\\text{Great tit}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{SubPlotYear}", "\\text{LocationYear}", "\\text{Residual}",
                              "\\text{SubPlotYear VC}", "\\text{LocationYear}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}")



m1_combined1$Latex_format <- paste(
  m1_combined1$Latex_names,  # Latex names column
  " & ",                    
  m1_combined1$EstimateW1, " & ",
  m1_combined2$EstimateW2, " & ",
  m1_combined3$EstimateW3, " & ",
  m1_combined4$EstimateW4, " & ",
  m1_combined5$EstimateW5,
  " \\"
)



m1_Latex <- m1_combined1 %>%
  select(Latex_format)
rownames(m1_Latex) <- NULL
print(m1_Latex, quote = FALSE, row.names = FALSE)




################################################################ Tenthredinidae, Oecophoridae, Curculionidae ----

### rearrange df


# 26. Tenthredinidae
estimate_w26_rows <- c(
  "mean_w_Tenthredinidae",
  "bw.1.26.",
  "bw.2.26.",
  "bw.3.26.",
  "bw.4.26.",
  "V_spy_Tenthredinidae", "V_locyear_Tenthredinidae", "V_e_Tenthredinidae",
  "VC_spy_Tenthredinidae", "VC_locyear_Tenthredinidae", "VC_e_Tenthredinidae",
  "phi_Tenthredinidae", "pi_Tenthredinidae"
)

# 26. Oecophoridae
estimate_w27_rows <- c(
  "mean_w_Oecophoridae",
  "bw.1.26.",
  "bw.2.26.",
  "bw.3.26.",
  "bw.4.26.",
  "V_spy_Oecophoridae", "V_locyear_Oecophoridae", "V_e_Oecophoridae",
  "VC_spy_Oecophoridae", "VC_locyear_Oecophoridae", "VC_e_Oecophoridae",
  "phi_Oecophoridae", "pi_Oecophoridae"
)


# 26. Curculionidae
estimate_w28_rows <- c(
  "mean_w_Curculionidae",
  "bw.1.26.",
  "bw.2.26.",
  "bw.3.26.",
  "bw.4.26.",
  "V_spy_Curculionidae", "V_locyear_Curculionidae", "V_e_Curculionidae",
  "VC_spy_Curculionidae", "VC_locyear_Curculionidae", "VC_e_Curculionidae",
  "phi_Curculionidae", "pi_Curculionidae"
)

# W1
m1_combined1 <- m1_column %>%
  filter(rownames_column %in% estimate_w26_rows) %>%
  slice(match(estimate_w26_rows, rownames_column)) %>%
  rename(EstimateW1 = EstimateH)

# W2 
m1_combined2 <- m1_column %>%
  filter(rownames_column %in% estimate_w27_rows) %>%
  slice(match(estimate_w27_rows, rownames_column)) %>%
  rename(EstimateW2 = EstimateH)

# W3 
m1_combined3 <- m1_column %>%
  filter(rownames_column %in% estimate_w28_rows) %>%
  slice(match(estimate_w28_rows, rownames_column)) %>%
  rename(EstimateW3 = EstimateH)



m1_combined1$Latex_names <- c("\\text{Intercept}", 
                              "\\text{Blue tit}",
                              "\\text{Great tit}",
                              "\\text{Date}",
                              "\\text{Y2024}",
                              "\\text{SubPlotYear}", "\\text{LocationYear}", "\\text{Residual}",
                              "\\text{SubPlotYear VC}", "\\text{LocationYear}", "\\text{Residual VC}",
                              "\\text{phi}",  "\\text{pi}")



m1_combined1$Latex_format <- paste(
  m1_combined1$Latex_names,  # Latex names column
  " & ",                    
  m1_combined1$EstimateW1, " & ",
  m1_combined2$EstimateW2, " & ",
  m1_combined3$EstimateW3, 
  " \\"
)



m1_Latex <- m1_combined1 %>%
  select(Latex_format)
rownames(m1_Latex) <- NULL
print(m1_Latex, quote = FALSE, row.names = FALSE)