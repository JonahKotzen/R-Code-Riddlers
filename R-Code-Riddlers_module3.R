#Establish Libraries
library(tidyverse)
library(dplyr)
library(ape)
library(phytools)
library(caper)
library(nlme)
library(MuMIn)
library(viridis)
library(geiger)

# Question 1: Load Files as Tibbles
anole.dat <- read.csv("anole.dat.csv")
anole.eco <- read.csv("anole.eco.csv")
anole.tre <- read.tree("anole.tre")

#Merge Data and Log Transform
anole.log <- anole.dat %>%
  left_join(anole.eco) %>% 
  filter(!Ecomorph %in%c("U","CH")) %>%
  na.omit() %>% 
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)


# Question 2: Linear Models
lm.PH <- lm(HTotal ~ SVL+PH, data = anole.log)
lm.PD <- lm(HTotal ~ SVL+ArbPD, data = anole.log)


#Question 3: Residuals & Plots
anole.log$residuals_PH <- resid(lm.PH)
anole.log$residuals_PD <- resid(lm.PD)

plot1 <- ggplot(anole.log, aes(x = PH, y = residuals_PH)) +
  geom_point() +
  labs(title = "Residuals vs. Perch Height",
       x = "Log Perch Height",
       y = "Residuals")
plot1

plot2 <- ggplot(anole.log, aes(x = ArbPD, y = residuals_PD)) +
  geom_point() +
  labs(title = "Residuals vs. Perch Diameter",
       x = "Log Perch Diameter",
       y = "Residuals")
plot2


#Question 4: Phylogenetics
pgls_PH <- gls(HTotal~SVL + PH, correlation = corBrownian(1,phy = anole.tre,form=~Species),data = anole.log, method = "ML")
pgls_PD <- gls(HTotal~SVL + ArbPD, correlation = corBrownian(1,phy = anole.tre,form=~Species),data = anole.log, method = "ML")
pgls_PH_PD <- gls(HTotal~SVL + PH+ArbPD, correlation = corBrownian(1,phy = anole.tre,form=~Species),data = anole.log, method = "ML")


#Question 5: Model Assessment (AICc & AICw)
anole.phylo.aic <- AICc(pgls_PH,pgls_PD,pgls_PH_PD)
aicw(anole.phylo.aic$AICc)

#The results show that diameter is a better individual predictor than height, 
#but they are both correlated with hind-limb-length,
#hence why the model that includes them both is better than either individually.


#Question 6: Plot & PGLS 
anole.log <- anole.log %>%
  mutate(residuals_PH_PD=resid(pgls_PH_PD))

phylo.plot <- anole.log %>%
  ggplot(aes(x = Ecomorph2, y = residuals_PH_PD)) +
  geom_boxplot() +
  geom_point(stat = "summary", fun = "mean", shape = 20, size = 3, color = "red") +
  labs(title = "Best Model's Residuals Visualized with Other Factors",
       x = "Ecomorphological Data",
       y = "Residuals")
  
print(phylo.plot)

