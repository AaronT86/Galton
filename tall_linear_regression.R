#Load dplyr to manipulate data
#Load ggplot2 for visualization
#Load HistData 
library(dplyr)
library(ggplot2)
library(HistData)

#Call the GaltonFamilies dataset and rename to fam
data("GaltonFamilies")
fam <- GaltonFamilies

fam %>% 
  distinct(family, .keep_all = TRUE) %>% 
  summarise(mean_father_height = mean(father),
            sd_father_height = sd(father),
            mean_mother_height = mean(mother),
            sd_mother = sd(mother))
fam %>% 
  group_by(gender) %>% 
  summarise(mean(childHeight))

fam <- fam %>% 
  mutate(tall_father = (father > 69.3) == TRUE,
         tall_mother = (mother > 64.0) == TRUE,
         x_tall_father = (father > 69.3 + 2.6) == TRUE,
         x_tall_mother = (mother > 64.0 + 2.3) == TRUE)
         
boy_tall_fam <- fam %>% 
  filter(tall_father == TRUE & tall_mother == TRUE & gender == "male")

girl_tall_fam <- fam %>% 
  filter(tall_father == TRUE & tall_mother == TRUE & gender == "female")
#Save for later
#x_tall_fam <- fam %>% 
#  filter(x_tall_father == TRUE & x_tall_mother == TRUE)

tall_fam %>% 
  distinct(family, .keep_all = TRUE) %>% 
  summarise(mean_father_height = mean(father),
            sd_father_height = sd(father),
            mean_mother_height = mean(mother),
            sd_mother = sd(mother))
tall_fam %>% 
  group_by(gender) %>% 
  summarise(mean(childHeight))

boy_tall_fam %>% 
  ggplot(aes(father, childHeight)) +
    geom_jitter() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(aes(slope = 0, intercept = 70.78))

fam %>% 
  ggplot(aes(father, childHeight)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(aes(slope = 0, intercept = 69.3))

summary(lm(father ~ childHeight, boy_tall_fam))  
summary(lm(father ~ childHeight, fam))
