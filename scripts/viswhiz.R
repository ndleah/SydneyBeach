# load packages =========
library(tidyverse)
library(here)
library(ggbeeswarm)


# read in cleanbeaches_new data =======
plotbeaches <- read_csv(here("data","cleanbeaches_new.csv"))

# plot bug levels by year =========
plotbeaches %>% 
  ggplot(aes(x= year, y = beachbugs)) + 
  geom_point()

# summarizing how many observations per year
plotbeaches %>% 
  group_by(year) %>% 
  summarise(obs = n())

# replotting bug levels using jitter

plotbeaches %>% 
  ggplot(aes(x= year, y = beachbugs)) + 
  geom_jitter()

# replotting bug levels using quasirandom
plotbeaches %>% 
  ggplot(aes(x = year, y = beachbugs)) + 
  geom_quasirandom()

# coerce year to be factor rather than interger
plotbeaches$year <- as.factor(plotbeaches$year)

# plot bug levels by site ========
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x = site, y = beachbugs, color = year)) +
  geom_jitter() +
  coord_flip()

# facet wrap =========
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x = year, y = beachbugs, color = council)) +
  geom_jitter() +
  facet_wrap(~ site)

# combine filter and gglot ======
plotbeaches %>% 
  na.omit() %>% 
  filter(beachbugs < 1000) %>% 
  ggplot(aes(x = year, y = beachbugs, color = site)) +
  geom_jitter() +
  facet_wrap(~site)

plotbeaches %>% 
  na.omit() %>% 
  filter(beachbugs < 1000) %>% 
  filter(site%in%c("Coogee Beach", "Bondi Beach")) %>% 
  ggplot(aes(x = year, y = beachbugs, color = site)) +
  geom_jitter() +
  facet_wrap(~site)

# how to get ggplot out of R =======
ggsave(here("output", "coogeebondi.png"))

# boxes and violins
plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x = site, y = logbeachbugs)) +
  geom_boxplot()

plotbeaches %>% 
  na.omit() %>% 
  ggplot(aes(x = year, y = logbeachbugs)) +
  geom_violin()

# filtered for buggier than average for that site = true
plotbeaches %>% 
  na.omit() %>% 
  filter(buggier_site == "TRUE") %>% 
  ggplot(aes(x=year, y=logbeachbugs, color = site, fill = year)) +
  geom_violin() +
  facet_wrap(~site)

