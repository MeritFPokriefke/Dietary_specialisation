
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 03#Plot_prefprey

### Identification prefered prey in diet: Existence of prey preferences
### Forest plot for all leaf defoliating arthropod families showing overrepresentation in in blue and great tit diets 

### Accessed last: May 2026
######################################################


### packages and data frames
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(ggridges)


### read input data files
md <- readRDS("Output/Models/M_avail_020925.rds")

########################################################################### posterior distributions plot all ----
###extract posterior

### family names (must match model indexing 1:28)
families <- c(
  "Noctuidae","Erebidae","Geometridae","Tineidae","Lasiocampidae",
  "Gelechiidae","Ypsolophidae","Tortricidae","Lycaenidae","Lypusidae",
  "Notodontidae","Psychidae","Nolidae","Glyphipterigidae","Drepanidae",
  "Pyralidae","Eriocraniidae","Argyresthiidae","Adelidae","Coleophoridae",
  "Hesperiidae","Crambidae","Oecophoridae",
  "Argidae","Pamphiliidae","Tenthredinidae",
  "Chrysomelidae","Curculionidae")

post <- rstan::extract(md, pars = "bw")$bw

posterior <- as.data.frame.table(post)
colnames(posterior) <- c("Iteration", "SpeciesID", "FamilyID", "value")

posterior <- posterior %>%
  mutate(SpeciesID = as.integer(SpeciesID),
         FamilyID  = as.integer(FamilyID)) %>%
  mutate(Species = c("Blue tit", "Great tit", "Y2024", "Date")[SpeciesID],
         Family  = families[FamilyID]) %>%
  filter(Species %in% c("Blue tit", "Great tit"))

### compute posterior means for ordering
means <- posterior %>%
  group_by(Species, Family) %>%
  summarise(mean = mean(value), .groups = "drop")

### festlegen order from smallest to largest mean
family_order <- means %>%
  group_by(Family) %>%
  summarise(mean = mean(mean), .groups = "drop") %>%
  arrange(mean) %>%
  pull(Family)

###apply ordering
posterior$Family <- factor(posterior$Family, levels = family_order)
means$Family     <- factor(means$Family, levels = family_order)

species_cols <- c("Blue tit" = "#4E79A7", "Great tit" = "#DAA520")

p <- ggplot(posterior, aes(x = value, y = Family, fill = Species)) +
     geom_vline(xintercept = 0, colour = "black", linewidth = 0.8) +
  geom_density_ridges(aes(group = interaction(Family, Species)), colour = "black", alpha = 0.6,
    scale = 1.4, linewidth = 1) +
  geom_point(data = means,aes(x = mean, y = Family, colour = Species), inherit.aes = FALSE, size = 1.5) +
  labs(x = "Posterior effect size (bw)",
    y = "Family",
    title = "XXX") +
  #scale_y_discrete(limits = rev) + # reverses ABC y-axis
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.line = element_line(linewidth = 1.2, colour = "black"),
        axis.ticks = element_line(linewidth = 1),
        axis.ticks.length = unit(2, "mm")) +
  scale_colour_manual(values = species_cols) +
  scale_fill_manual(values = species_cols) +
  theme(strip.background = element_blank(),
    strip.text = element_text(face = "bold"))
### print plot
p


### save plot to file with fixed size
ggsave("Output/Plots/posterior_plot_large_190626.pdf", p, width = 8, height = 19, device = cairo_pdf)

######################################################################### posterior distributions plot small ----

keep_families <- c(
  "Geometridae",
  "Erebidae",
  "Noctuidae",
  "Tortricidae",
  "Lasiocampidae"
)

posterior <- posterior %>%
  filter(Family %in% keep_families) %>%
  mutate(Family = factor(Family, levels = keep_families))

means <- means %>%
  filter(Family %in% keep_families) %>%
  mutate(Family = factor(Family, levels = keep_families))


species_cols <- c("Blue tit" = "#4E79A7", "Great tit" = "#DAA520")

a <- ggplot(posterior, aes(x = value, y = Family, fill = Species)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.8) +
  geom_density_ridges(aes(group = interaction(Family, Species)), colour = "black", alpha = 0.6,
                      scale = 1.4, linewidth = 1) +
  geom_point(data = means,aes(x = mean, y = Family, colour = Species), inherit.aes = FALSE, size = 3) +
  labs(x = "Posterior effect size (bw)",
       y = "Family",
        title = "XXX") +
  scale_y_discrete(
    limits = rev
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.line = element_line(linewidth = 1.2, colour = "black"),
        axis.ticks = element_line(linewidth = 1),
        axis.ticks.length = unit(2, "mm")) +
  scale_colour_manual(values = species_cols) +
  scale_fill_manual(values = species_cols) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))
### print plot
a

























################### OLD


### read input data files
df <- read.csv("Data/Extracted/availabiliy_020925.csv", header=TRUE)
model <- read.csv("Output/Models/M_avail_020925.csv",
                  header = TRUE,
                  check.names = FALSE,
                  row.names = NULL)

############################################################################################ forest plot B&G ----
### prepare model output for plotting for blue and great tits (B&G)

### rename first column to 'stat'
names(model)[1] <- "stat"

### convert data from wide to long format
model_long <- model %>%
  pivot_longer(
    cols = -stat,
    names_to = "param",
    values_to = "value"
  )

### extract results for blue tit
blue <- model_long %>%
  filter(str_detect(param, "^bw\\[1,")) %>%  # keep parameters for B
  filter(stat %in% c("mean","2.5%","97.5%")) %>%  # keep summary stats
  mutate(FamilyID = as.integer(str_match(param, "bw\\[1,(\\d+)\\]")[,2])) %>%  # extract arthropod family ID
  pivot_wider(id_cols = FamilyID, names_from = stat, values_from = value) %>%  # reshape to wide
  rename(lower = `2.5%`, upper = `97.5%`) %>%  # rename confidence limits
  mutate(Species = "Blue tit")

### extract results for great tit
great <- model_long %>%
  filter(str_detect(param, "^bw\\[2,")) %>%  # keep parameters for G
  filter(stat %in% c("mean","2.5%","97.5%")) %>%
  mutate(FamilyID = as.integer(str_match(param, "bw\\[2,(\\d+)\\]")[,2])) %>%
  pivot_wider(id_cols = FamilyID, names_from = stat, values_from = value) %>%
  rename(lower = `2.5%`, upper = `97.5%`) %>%
  mutate(Species = "Great tit")

### combine both species into one dataset
both <- bind_rows(blue, great)

### define family names
family_names <- c("Noctuidae","Erebidae","Geometridae","Tineidae",
                  "Lasiocampidae","Gelechiidae","Ypsolophidae","Tortricidae",
                  "Lycaenidae","Lypusidae","Notodontidae","Psychidae",
                  "Nolidae","Glyphipterigidae","Drepanidae","Pyralidae",
                  "Eriocraniidae","Argyresthiidae","Adelidae","Coleophoridae",
                  "Hesperiidae","Crambidae","Oecophoridae","Argidae",
                  "Pamphiliidae","Tenthredinidae","Chrysomelidae","Curculionidae")

### add family names based on athropod family ID
both$Family <- family_names[both$FamilyID]

### set species order for plotting
both$Species <- factor(both$Species, levels = c("Great tit", "Blue tit"))

### create forest plot with both species
p <- ggplot(both, aes(x = mean, y = reorder(Family, mean), color = Species)) +
  geom_vline(xintercept = 0, color = "black") +  # reference line at zero
  geom_errorbarh(aes(xmin = lower, xmax = upper), 
                 height = 0.4, size = 0.8, 
                 position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +  # mean points
  labs(x = "Effect size", y = "Family") +
  theme_classic(base_size = 12) +
  scale_color_manual(values = c("Blue tit" = "#4E79A7", "Great tit" = "#DAA520")) +
  theme(axis.text.y = element_text(size = 12, margin = margin(r = 10)))

### save plot to file with fixed size
#ggsave("Output/Plots/forest_plot2_140925.pdf", p, width = 8, height = 14, device = cairo_pdf)



########################################################################################## small plot for ms ----


### families to display
selected_families <- c(
  "Noctuidae",
  "Erebidae",
  "Geometridae",
  "Lasiocampidae",
  "Tortricidae"
)

both <- subset(both, Family %in% selected_families)

### set species order for plotting
both$Species <- factor(both$Species, levels = c("Great tit", "Blue tit"))

### create forest plot with both species
p <- ggplot(both, aes(x = mean, y = reorder(Family, mean), color = Species)) +
  geom_vline(xintercept = 0, color = "black") +  # reference line at zero
  geom_errorbarh(aes(xmin = lower, xmax = upper), 
                 height = 0.4, size = 0.8, 
                 position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +  # mean points
  labs(x = "Effect size", y = "Family") +
  theme_classic(base_size = 12) +
  scale_color_manual(values = c("Blue tit" = "#4E79A7", "Great tit" = "#DAA520")) +
  theme(axis.text.y = element_text(size = 12, margin = margin(r = 10)))

### save plot to file with fixed size
ggsave("Output/Plots/forest_plot_short_200526.pdf", p, width = 8, height = 4, device = cairo_pdf)

