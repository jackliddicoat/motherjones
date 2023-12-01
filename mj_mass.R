# important packages
library(tidyverse)
library(janitor)
library(ggplot2)
library(dplyr)
library(MASS)
library(jtools)
library(stargazer)
library(patchwork)

# set wd to downloads

# load the data into R
mps <- read.csv("Mother Jones - Mass Shootings Database, 1982 - 2023 - Sheet1.csv")

# tb1: frequency
mean_82_91 <- mps %>% 
  filter(between(year, 1982, 1991)) %>% 
  count(year)
mean(mean_82_91$n)
sd(mean_82_91$n)

mean_92_01 <- mps %>% 
  filter(between(year, 1992, 2001)) %>% 
  count(year)
mean(mean_92_01$n)
sd(mean_92_01$n)

mean_02_11 <- mps %>%
  filter(between(year, 2002, 2011)) %>% 
  count(year)
mean(mean_02_11$n)
sd(mean_02_11$n)

mean_12_22 <- mps %>% 
  filter(between(year, 2012, 2022)) %>% 
  count(year)
mean(mean_12_22$n)
sd(mean_12_22$n)

# tb3: accounting for change in def

mean_82_91 <- mps %>%
  filter(fatalities > 3) %>% 
  filter(between(year, 1982, 1991)) %>% 
  count(year)
mean(mean_82_91$n)
sd(mean_82_91$n)

mean_92_01 <- mps %>% 
  filter(fatalities > 3) %>%
  filter(between(year, 1992, 2001)) %>% 
  count(year)
mean(mean_92_01$n)
sd(mean_92_01$n)

mean_02_11 <- mps %>%
  filter(fatalities > 3) %>%
  filter(between(year, 2002, 2011)) %>% 
  count(year)
mean(mean_02_11$n)
sd(mean_02_11$n)

mean_12_22 <- mps %>% 
  filter(fatalities > 3) %>%
  filter(between(year, 2012, 2022)) %>% 
  count(year)
mean(mean_12_22$n)
sd(mean_12_22$n)

# tb4: deadliness of ms by decade

fat_82_91 <- mps %>%
  filter(fatalities > 3) %>% 
  filter(between(year, 1982, 1991))

fat_82_91 %>% 
  summarise(sum(fatalities))

fat_92_01 <- mps %>% 
  filter(fatalities > 3) %>%
  filter(between(year, 1992, 2001))

fat_92_01 %>% 
  summarise(sum(fatalities))

fat_02_11 <- mps %>%
  filter(fatalities > 3) %>%
  filter(between(year, 2002, 2011))

fat_02_11 %>% 
  summarise(sum(fatalities))

fat_12_22 <- mps %>%
  filter(fatalities > 3) %>%
  filter(between(year, 2012, 2022))

fat_12_22 %>% 
  summarise(sum(fatalities))

# tb5: injuriousness of ms by decade

inj_82_91 <- mps %>%
  mutate(injured = as.numeric(injured)) %>% 
  filter(fatalities > 3) %>% 
  filter(between(year, 1982, 1991))

inj_82_91 %>% 
  summarise(sum(injured))

inj_92_01 <- mps %>% 
  mutate(injured = as.numeric(injured)) %>%
  filter(fatalities > 3) %>%
  filter(between(year, 1992, 2001))

inj_92_01 %>% 
  summarise(sum(injured))

inj_02_11 <- mps %>%
  mutate(injured = as.numeric(injured)) %>% 
  filter(fatalities > 3) %>%
  filter(between(year, 2002, 2011))

inj_02_11 %>% 
  summarise(sum(injured))

inj_12_22 <- mps %>% 
  mutate(injured = as.numeric(injured)) %>%
  filter(fatalities > 3) %>%
  filter(between(year, 2012, 2022))

inj_12_22 %>%
  filter(!is.na(injured)) %>% 
  summarise(sum(injured))

mps %>% 
  mutate(injured = as.numeric(injured)) %>% 
  arrange(desc(injured))

# tb6: age demographics of shooters

mean_age <- mps %>% 
  mutate(age_of_shooter = as.numeric(age_of_shooter)) %>% 
  filter(!is.na(age_of_shooter)) %>%
  group_by(year) %>% 
  summarise(mean(age_of_shooter))
 
mean_age %>%
  filter(year != 2023) %>% 
  ggplot(aes(year, `mean(age_of_shooter)`)) +
  geom_point() +
  geom_line(lwd = 1) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ylim(0, 70)

mps %>% 
  filter(age_of_shooter < 21) %>%
  filter(age_of_shooter >= 18) %>%
  View()

mps %>% 
  dim()

mps %>% 
  filter(year != 2023) %>% 
  filter(fatalities > 3) %>% 
  mutate(injured = as.numeric(injured)) %>%
  filter(injured < 500) %>% 
  ggplot(aes(injured, fatalities)) +
  geom_point(size = 2) +
  geom_smooth(se = F, method = "lm") +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Number injured", y = "Number killed") +
  ylim(0, 50)

# tb6: race/ethnicity
mps_r <- mps %>% 
  filter(!(race %in% c("-", "unclear"))) %>% 
  mutate(race = ifelse(race %in% c("white", "White "), "White", race)) %>%
  mutate(race = ifelse(race == "black", "Black", race)) %>% 
  count(race, sort = TRUE)

sum(mps_r$n)

# fig2: location

mps_us <- mps[-(1:17),]

mps_us$longitude <- as.numeric(mps_us$longitude)
mps_us$latitude <- as.numeric(mps_us$latitude)

mps_us %>% 
  View()

mps_us %>% 
  filter((between(longitude, -150, -50))) %>% 
  ggplot(aes(longitude, latitude)) +
  borders("state") +
  geom_point(aes(size = fatalities), show.legend = F) +
  theme()

# tb7: ms by location

mps_location <- mps %>% 
  mutate(location.1 = ifelse(mps$location.1=="Other\n", "Other", location.1),
         location.1 = ifelse(mps$location.1=="workplace", "Workplace", location.1),
         location.1 = ifelse(mps$location.1=="\nWorkplace", "Workplace", location.1),
         location.1 = ifelse(mps$location.1=="religious", "Religious", location.1)) %>% 
  count(location.1, sort = TRUE)

mps_location
sum(mps_location$n)

# tb8: weapons legality
mps %>% 
  View()

mps_legal <- mps[-c(50, 70),]


mps_legal %>% 
  filter(!(weapons_obtained_legally %in% c("-", "TBD"))) %>%
  mutate(weapons_obtained_legally = ifelse(weapons_obtained_legally=="\nYes", "Yes",
                                           weapons_obtained_legally),
         weapons_obtained_legally = ifelse(weapons_obtained_legally=="yes", "Yes",
                                           weapons_obtained_legally),
         weapons_obtained_legally = ifelse(weapons_obtained_legally=="Yes ", "Yes",
                                           weapons_obtained_legally)) %>% 
  count(weapons_obtained_legally, sort = T)

# tb9

mps %>%
  filter(!prior_signs_mental_health_issues %in% c("TBD")) %>% 
  mutate(prior_signs_mental_health_issues = ifelse(prior_signs_mental_health_issues=="yes", "Yes", prior_signs_mental_health_issues)) %>%
  mutate(prior_signs_mental_health_issues = ifelse(prior_signs_mental_health_issues=="Unclear ", "Unclear", prior_signs_mental_health_issues)) %>%
  filter(prior_signs_mental_health_issues != "-") %>% 
  count(prior_signs_mental_health_issues, sort = TRUE)
68 / (68 + 24 + 1 + 17)
# 61.8% had signs of prior mental health problems
17 / (68 + 24 + 1 + 17)
# 21.8% did not have signs of prior mental health problems
25 / (68 + 24 + 1 + 17)
# 22.7% of the time the case was unclear

# Fig 3
p1 <- mps_mh %>% 
  count(fatalities) %>% 
  ggplot(aes(fatalities, n)) +
  geom_col() +
  labs(x = "fatalities")

mps_mh$injured <- as.numeric(mps_mh$injured)

p2 <- mps_mh %>% 
  count(injured) %>% 
  ggplot(aes(injured, n)) +
  geom_col() +
  labs(x = "injuries")
p1 + p2

#tb 10

mps_mh <- mps %>%
  filter(!prior_signs_mental_health_issues %in% c("TBD")) %>% 
  mutate(prior_signs_mental_health_issues = ifelse(prior_signs_mental_health_issues=="yes", "Yes", prior_signs_mental_health_issues)) %>%
  mutate(prior_signs_mental_health_issues = ifelse(prior_signs_mental_health_issues=="Unclear ", "Unclear", prior_signs_mental_health_issues)) %>%
  filter(prior_signs_mental_health_issues != "-") %>%
  mutate(type = ifelse(type=="Mass", 1, 0),
         prior_signs_mental_health_issues = ifelse(prior_signs_mental_health_issues=="Yes", 1, 0))

mps_mh %>%
  View()

# nb model (fatalities)
nb1 <- glm.nb(fatalities ~ prior_signs_mental_health_issues, data = mps_mh,
              link = "log")
summmary(nb1)

# nb model (injured)
nb2 <- glm.nb(injured ~ prior_signs_mental_health_issues, data = mps_mh,
              link = "log")
summary(nb2)

stargazer(nb1, nb2)

# poisson models
pois1 <- glm(fatalities ~ prior_signs_mental_health_issues, data = mps_mh,
             family = poisson(link = "log"))
summary(pois1)

pois2 <- glm(injured ~ prior_signs_mental_health_issues, data = mps_mh,
             family = poisson(link = "log"))
summary(pois2)

