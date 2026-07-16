
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 02#Plot_gdietvar

### Variation in general diet: Variation in dietary composition
### Plot depicting mean relative read abundance for significantly differing bird types (adult, nestling 10, nestling 15)
### in five most abundant arthropod orders

### Accessed last: September 2025
######################################################


### packages and data

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(patchwork)


df <- read.csv("Data/Extracted/birds2_280825.csv", header=TRUE)


### select main orders
df <- df %>%
  select(RRA_Lepidoptera, RRA_Diptera, RRA_Araneae, RRA_Hymenoptera, RRA_Coleoptera, Species, Type)

### select bird types
df <- df %>%
  mutate(Type4 = case_when(
    Type %in% c("AF", "AM") ~ "A",
    Type %in% c("chick1")   ~ "C1",
    Type %in% c("chick2")   ~ "C2",
    TRUE ~ NA_character_   # catch anything else
  ),
  Type5 = case_when(
    Type %in% c("AF", "AM")   ~ "A",
    Type %in% c("chick1", "chick2") ~ "C",
    TRUE ~ NA_character_   # catch anything else
  ))

df <- df %>%
  mutate(
    # Type2 = split adults, chick1, chick2 per species
    Type2 = case_when(
      Type %in% c("AF", "AM") & Species == 0 ~ "B_A",   # Blue tit adult
      Type %in% c("AF", "AM") & Species == 1 ~ "G_A",   # Great tit adult
      Type %in% c("chick1")   & Species == 0 ~ "B_C1",  # Blue tit chick1
      Type %in% c("chick1")   & Species == 1 ~ "G_C1",  # Great tit chick1
      Type %in% c("chick2")   & Species == 0 ~ "B_C2",  # Blue tit chick2
      Type %in% c("chick2")   & Species == 1 ~ "G_C2",  # Great tit chick2
      TRUE ~ NA_character_
    ),
    
    # Type3 = coarser grouping: adults vs chicks per species
    Type3 = case_when(
      Type %in% c("AF", "AM") & Species == 0 ~ "B_A",   # Blue tit adult
      Type %in% c("AF", "AM") & Species == 1 ~ "G_A",   # Great tit adult
      Type %in% c("chick1", "chick2") & Species == 0 ~ "B_C", # Blue tit chicks
      Type %in% c("chick1", "chick2") & Species == 1 ~ "G_C", # Great tit chicks
      TRUE ~ NA_character_
    )
  )

#### define colors
mycols <- c(
  # Type3 (adults vs all chicks)
  "B_A"  = "darkblue",  # Blue tit adult
  "G_A"  = "#B8860B",   # Great tit adult
  
  # Type2 (adults vs chick1 vs chick2)
  "B_C1" = "#5F9EA0",   # lighter blue for Blue tit chick1
  "B_C2" = "#87CEEB",   # darker teal for Blue tit chick2
  "G_C1" = "#CDAD00",   # brighter gold for Great tit chick1
  "G_C2" = "#FFD700"    # olive-gold for Great tit chick2
)


######################################################################################## plot B only (Type2) ----
### filter data: keep only rows where Species == 0 (blue tit; B)
df_B <- subset(df, df$Species == 0)

### convert wide RRA (relative read abundances) columns to long format (Order + RRA values)
df_long_B_t2 <- df_B %>%
  pivot_longer(
    cols = starts_with("RRA_"),
    names_to = "Order",
    values_to = "RRA"
  ) %>%
  mutate(Order = sub("RRA_", "", Order))

### summarise data: mean, SD, sample size, and standard error per group
df_summary_B_t2 <- df_long_B_t2 %>%
  mutate(Grouping = Type2) %>%
  group_by(Order, Grouping) %>%
  summarise(
    mean_RRA = mean(RRA, na.rm = TRUE),
    sd_RRA   = sd(RRA, na.rm = TRUE),
    n        = sum(!is.na(RRA)),
    se_RRA   = sd_RRA / sqrt(n),            
    .groups = "drop"
  ) %>%
  mutate(
    Order = factor(Order,
                   levels = c("Lepidoptera",
                              "Diptera",
                              "Hymenoptera",
                              "Coleoptera",
                              "Araneae")),
    ### Define error bar limits
    ymin = mean_RRA - se_RRA,
    ymax = mean_RRA + se_RRA
  )

### Create bar plot with error bars
p2_t2 <- ggplot(df_summary_B_t2, aes(x = mean_RRA, y = Order, fill = Grouping)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = ymin, xmax = ymax),
                position = position_dodge(width = 0.9),
                width = 0.25) +
  scale_x_continuous(limits = c(0, 0.51), breaks = seq(0, 0.5, 0.1)) +
  scale_fill_manual(values = mycols) +
  labs(x = "Mean Relative Read Abundance", y = "Order", fill = "Type2") +
  theme_classic() +
  theme(legend.position = "top") +
  coord_flip()


######################################################################################## plot G only (Type2) ----
### filter data: keep only rows where Species == 1 (great tit; G)
df_G <- subset(df, df$Species == 1)

df_long_G_t2 <- df_G %>%
  pivot_longer(
    cols = starts_with("RRA_"),
    names_to = "Order",
    values_to = "RRA"
  ) %>%
  mutate(Order = sub("RRA_", "", Order))

df_summary_G_t2 <- df_long_G_t2 %>%
  mutate(Grouping = Type2) %>%
  group_by(Order, Grouping) %>%
  summarise(
    mean_RRA = mean(RRA, na.rm = TRUE),
    sd_RRA   = sd(RRA, na.rm = TRUE),
    n        = sum(!is.na(RRA)),
    se_RRA   = sd_RRA / sqrt(n),
    .groups  = "drop"
  ) %>%
  mutate(
    Order = factor(Order,
                   levels = c("Lepidoptera",
                              "Diptera",
                              "Hymenoptera",
                              "Coleoptera",
                              "Araneae")),
    ymin = mean_RRA - se_RRA,
    ymax = mean_RRA + se_RRA
  )

p3_t2 <- ggplot(df_summary_G_t2, aes(x = mean_RRA, y = Order, fill = Grouping)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = ymin, xmax = ymax),
                position = position_dodge(width = 0.9),
                width = 0.25) +
  scale_x_continuous(limits = c(0, 0.51), breaks = seq(0, 0.5, 0.1)) +
  scale_fill_manual(values = mycols) +
  labs(x = "Mean Relative Read Abundance", y = "Order", fill = "Type2") +
  theme_classic() +
  theme(legend.position = "top") +
  coord_flip()

###################################################################################################### patch ----
### patchwork plots underneath each other
p2_t2 / p3_t2





