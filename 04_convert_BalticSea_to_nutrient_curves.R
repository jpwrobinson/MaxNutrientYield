pacman::p_load(mizer, tidyverse, skimr, cowplot, here, mizerExamples, funk, install=FALSE)
theme_set(theme_bw())

## Goal: convert Baltic Sea model sim to nutrient yields

##  load Baltic biomass, yield results
sim<-read.csv('results/sim/BalticSea_curves.csv')
colnames(sim)<-tolower(colnames(sim))
sim$species<-str_to_title(sim$species)

# load nutrient profiles
nutrients<-read.csv(file = 'nutrient_profiles.csv')

# cut F above community/stock collapse
flim<-0.28

## convert yield to nutrient yields
yield_species<-sim %>% filter(unit == 'Yield' & f < flim) %>% select(-unit, -fmulti)
colnames(yield_species)[2]<-'yield'

yield_species$ScientificName<-Baltic_nut$ScientificName[match(yield_species$species, Baltic_nut$species)]
yield_species$calcium.mg<-nut$Calcium_mu[match(yield_species$ScientificName,nut$species)]
yield_species$iron.mg<-nut$Iron_mu[match(yield_species$ScientificName,nut$species)]
yield_species$selenium.mug<-nut$Selenium_mu[match(yield_species$ScientificName,nut$species)]
yield_species$zinc.mg<-nut$Zinc_mu[match(yield_species$ScientificName,nut$species)]
yield_species$omega3.g<-nut$Omega_3_mu[match(yield_species$ScientificName,nut$species)]
yield_species$vitamin_a.mug<-nut$Vitamin_A_mu[match(yield_species$ScientificName,nut$species)]
yield_species$vitamin_d.mug<-NS_nut$value[NS_nut$nutrient == 'vitamin_d.mug'][match(yield_species$ScientificName,NS_nut$ScientificName[NS_nut$nutrient == 'vitamin_d.mug'])]

## curves, scaled, and weighted versions
y_nut<-yield_species %>% 
  pivot_longer(-c(species, yield, ScientificName, f), names_to = 'nutrient', values_to = 'value') %>%
  ## drop ammodytes for now
  filter(!is.na(value)) %>%
  mutate(nut.yield=value*yield) %>%
  group_by(f, nutrient) %>%
  summarise(yield = sum(yield), nut.yield=sum(nut.yield)) 

y.nut.scale<-y_nut %>%
  ungroup() %>% group_by(nutrient) %>%
  ## rescale nutrients to proportion of max.
  mutate(max.yield = max(yield), max.nut=max(nut.yield)) %>% 
  mutate(yield.scale=yield / max.yield, nut.scale = nut.yield/max.nut) %>%
  ungroup()

  ## now catch-weight nutrient concentration per f level
nut_yield_species<-yield_species %>%
  pivot_longer(-c(species, yield, ScientificName, f), names_to = 'nutrient', values_to = 'value') %>%
  filter(!is.na(value)) %>%
  ## drop ammodytes for now
  # filter(species != 'Sandeel') %>%
  mutate(nut.yield=value*yield) %>%
  group_by(f, nutrient) %>%
  mutate(ty = sum(nut.yield)) %>% ungroup() %>%
  group_by(f, species, nutrient) %>%
  mutate(nut.weight=nut.yield / ty) 

## get total biomass and species biomass 
bs<-sim %>% filter(unit == 'Biomass' & f < flim) %>% mutate(biomass = value) %>% select(-unit, -value)
tb<-bs %>% group_by(f) %>% summarise(biomass = sum(biomass))

## unfished biomass
unfish.biom<-bs %>% filter(f == 0) %>% mutate(unfished_biomass = biomass)

## collapsed species
collapse<-bs
collapse$unfished_biomass<-unfish.biom$biomass[match(collapse$species, unfish.biom$species)]
collapse$prop<-with(collapse, biomass / unfished_biomass)*100
collapse$collapsed<-ifelse(collapse$prop < 10, 1, 0)
collapsed<-collapse %>% group_by(f) %>% summarise(n.collapse = sum(collapsed))

## scale biomass and collapsed
tb.scale<-tb %>% mutate(max.b = max(biomass), b.scale = biomass/max.b) 
cl.scale<-collapsed %>% mutate(max.cl = 3, cl.scale = n.collapse/max.cl)


## objects for plotting
curves<-rbind(y.nut.scale %>% mutate(group = nutrient, value = nut.yield, scaled = nut.scale) %>% select(f, group, value, scaled),
               y.nut.scale %>% filter(nutrient == 'iron.mg') %>% mutate(group = 'Total yield', value = yield, scaled = yield.scale) %>% select(f, group, value, scaled),
               tb.scale %>% mutate(group= 'Fishable biomass', value = biomass, scaled = b.scale) %>% select(f, group, value, scaled),
               cl.scale %>% mutate(group= 'Collapsed species', value = n.collapse, scaled = cl.scale) %>% select(f, group, value, scaled))

curves$group<-factor(curves$group, 
                      levels=c('calcium.mg', 'iron.mg', 'selenium.mug', 'zinc.mg', 'vitamin_a.mug','vitamin_d.mug',
                               'omega3.g','Total yield', 'Fishable biomass', 'Collapsed species'))

## save output
save(
  yield_species,
  nut_yield_species,
  tot_biomass,
  biomass_species,
  collapsed,
  curves,
file='BalticSea_mMNY_simulated.rds')
