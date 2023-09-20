#Establish Libraries
library(dplyr)
library(ape)
library(phytools)
library(caper)

# Question 1: Load Files as Tibbles
anole.dat <- read.csv("anole.dat.csv")
anole.eco <- read.csv("anole.eco.csv")
anole.tre <- read.tree("anole.tre")

#Merge Data
anole.log <- anole.dat %>%
  left_join(anole.eco) %>% 
  print()

# Question 2: Linear Models
lm.PH <- lm(log(HTotal) ~ log(PH), data = anole.log)
lm.PD <- lm(log(HTotal) ~ log(ArbPD), data = anole.log)

#Question 3: Residuals & Plots
anole.log$residuals_PH <- resid(lm.PH)
anole.log$residuals_PD <- resid(lm.PD)

plot1 <- ggplot(anole.log, aes(x = log(PH), y = residuals_PH)) +
  geom_point() +
  labs(title = "Residuals vs. Perch Height",
       x = "Log Perch Height",
       y = "Residuals")

plot2 <- ggplot(anole.log, aes(x = log(PD), y = residuals_PD)) +
  geom_point() +
  labs(title = "Residuals vs. Perch Diameter",
       x = "Log Perch Diameter",
       y = "Residuals")

#Question 4: Phylogenetics
pgls_PH <- pgls(log(HTotal) ~ log(PH), phy = anole.tre)
pgls_PD <- pgls(log(HTotal) ~ log(ArbPD), phy = anole.tre)
pgls_PH_PD <- pgls(log(HTotal) ~ log(PH) + log(ArbPD), phy = anole.tre)

#Question 5: Model Assessment (AICc & AICw)
aic_PH <- AICc(pgls_PH)
aic_PD <- AICc(pgls_PD)
aic_PH_PD <- AICc(pgls_PH_PD)

#Question 6: Plot & PGLS 


ggsave("custom_plot.png", width = 6, height = 4)

