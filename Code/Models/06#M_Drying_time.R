
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 06#M_Drying_time

### Test if drying time of environmental samples influences metabarcoding failing probability 

### Accessed last: Mai 2025
######################################################

### packages and data


library(dplyr)
library(tidyr)

frass4 <- read.csv("Data/Extracted/Frass_drying_150525")

###################################################################################################### model ----
# generalized linear model for drying time and sample failure

mod <- lme4:: glmer(failed_flag ~ Drying.time + April.day + (1 | collector_nr),
                    data = frass4,
                    family = binomial)
summary(mod)