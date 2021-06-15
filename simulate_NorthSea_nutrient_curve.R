pacman::p_load(mizer, tidyverse, skimr, cowplot, here, funk, install=TRUE)
theme_set(theme_bw())

## Goal: simulate North Sea community under historic fishing pressure

# Reference guides:
# https://github.com/sizespectrum/mizer
# https://sizespectrum.org/mizer/articles/mizer.html

## load nutrient profiles
nutrients<-read.csv(file = 'nutrient_profiles.csv')


## for north sea species (n = 12), data on w_inf, w_mat, beta, sigma, k_vb and R_max are provided

## load calibrated North Sea model
load(file = 'NorthSea_calibrated.rds')
# load(file = 'NorthSea_mizer_parameters.rds')


## set an effort matrix based on North Sea mortality history 
relative_effort<-f_history
initial_effort <- matrix(relative_effort[1, ], byrow = TRUE, nrow = 100,
                         ncol = ncol(relative_effort), dimnames = list(1867:1966))
relative_effort <- rbind(initial_effort, relative_effort)

## set up range of fishing effort
f=seq(0, 12, 0.2)

## set up unfished biomass for stock collapse definition
unfished <- project(params, effort = 0, dt = 0.25, t_save = 1, t_max=100)
unfish.biom<-getBiomass(unfished, min_w = 10, max_w = 100e3) %>% data.frame() %>%
  mutate(year = rownames(getBiomass(unfished))) %>%
  pivot_longer(-year, names_to = 'species', values_to = 'biomass') %>%
  group_by(year, species) %>%
  summarise(unfished_biomass = biomass) %>% 
  filter(year == 99)

## empty vectors to be filled in simulations
yield_species<-numeric() 
tot_biomass<-numeric() 
biomass_species<-numeric() 
mean_weight<-numeric() 
collapsed<-numeric() 

## loop over each f value, simulate to 2050, save 2050 results

for(i in 1:length(f)){
    
    # update the fishing effort for each species
    f.run <- c(Sprat = f[i], Sandeel = f[i], N.pout = f[i], Herring = f[i], Dab = f[i],
              Whiting = f[i], Sole = f[i], Gurnard = f[i], Plaice = f[i], Haddock = f[i],
              Cod = f[i], Saithe = f[i])
    
    ## setup 89 years of fishing at the fixed f level (to equilbrium at 2050)
    scenario2 <- t(array(f.run, dim=c(12,90), dimnames=list(NULL,year = 2011:2050)))
    
    # add historic effort up to 2010, 
    # then linearly shift towards f.run levels from 2010-2015
    scenario2 <- rbind(relative_effort, scenario2)
    for (sp in dimnames(scenario2)[[2]]){
      scenario2[as.character(2011:2015),sp] <- scenario2["2010",sp] +
        (((scenario2["2015",sp] - scenario2["2010",sp]) / 5) * 1:5)
    }
    
    ## run simulation
    sim2 <- project(params, effort = scenario2, dt = 0.25, t_save = 1)
    
    ## save total biomass
    t<-getBiomass(sim2, min_w = 10, max_w = 100e3) %>% data.frame() %>%
        mutate(year = rownames(getBiomass(sim2))) %>%
        pivot_longer(-year, names_to = 'species', values_to = 'biomass') %>%
        group_by(year) %>%
        summarise(biomass = sum(biomass)) %>%
        filter(year == 2050) %>% 
        mutate(f = f[i])
    
    ## save species biomass
    bs<-getBiomass(sim2, min_w = 10, max_w = 100e3) %>% data.frame() %>%
      mutate(year = rownames(getBiomass(sim2))) %>%
      pivot_longer(-year, names_to = 'species', values_to = 'biomass') %>%
      group_by(year, species) %>%
      summarise(biomass = sum(biomass)) %>%
      filter(year == 2050) %>% 
      mutate(f = f[i])
    
    ## save collapsed species
    collapse<-getBiomass(sim2, min_w = 10, max_w = 100e3) %>% data.frame() %>%
      mutate(year = rownames(getBiomass(sim2))) %>%
      pivot_longer(-year, names_to = 'species', values_to = 'biomass') %>%
      group_by(year, species) %>%
      summarise(min_biomass = min(biomass)) %>%
      filter(year %in% c(2050))
    
    ## define stock collapse as <10% of unfished biomass
    collapse$unfished_biomass<-unfish.biom$unfished_biomass[match(collapse$species, unfish.biom$species)]
    collapse$prop<-with(collapse, min_biomass / unfished_biomass)*100
    collapse$collapsed<-ifelse(collapse$prop < 10, 1, 0)
    collapse$f <- f[i]
    
    cl<-data.frame(n.collapse = sum(collapse$collapsed), f = f[i])
    
    ## save mean weight
    w<-getMeanWeight(sim2, min_w = 10, max_w = 100e3) %>% last()
    w<-data.frame(mean_weight = w, year=2050, f = f[i])
    
    ## save total catch (yield)
    y<-data.frame(getYield(sim2))
    y$year<-as.numeric(rownames(y))
    y<-y %>% 
      pivot_longer(-year, names_to = 'species', values_to = 'yield') %>% 
      filter(year == 2050) %>% mutate(f = f[i])
    
    ## bind result and loop to next simulated F value
    yield_species<-rbind(yield_species, y)
    tot_biomass<-rbind(tot_biomass, t)
    biomass_species<-rbind(biomass_species, data.frame(bs))
    mean_weight<-rbind(mean_weight, w)
    collapsed<-rbind(collapsed, cl)
  
  }

## match in nutrient concentrations
yield_species$ScientificName<-nutrients$ScientificName[match(yield_species$species, nutrients$species)]
yield_species$calcium.mg<-nutrients$Calcium_mu[match(yield_species$ScientificName,nutrients$ScientificName_corrected)]
yield_species$iron.mg<-nutrients$Iron_mu[match(yield_species$ScientificName,nutrients$ScientificName_corrected)]
yield_species$selenium.mug<-nutrients$Selenium_mu[match(yield_species$ScientificName,nutrients$ScientificName_corrected)]
yield_species$zinc.mg<-nutrients$Zinc_mu[match(yield_species$ScientificName,nutrients$ScientificName_corrected)]
yield_species$omega3.g<-nutrients$Omega3_mu[match(yield_species$ScientificName,nutrients$ScientificName_corrected)]
yield_species$vitamin_a.mug<-nutrients$Vitamin_A_mu[match(yield_species$ScientificName,nutrients$ScientificName_corrected)]
yield_species$vitamin_d.mug<-nutrients$value[nutrients$nutrient == 'vitamin_d.mug'][match(yield_species$ScientificName,NS_nut$ScientificName[NS_nut$nutrient == 'vitamin_d.mug'])]

## estimate nutrient yields at each F, by species
y_nut<-yield_species %>% 
  pivot_longer(-c(year, species, yield, ScientificName, f), names_to = 'nutrient', values_to = 'value') %>%
  mutate(nut.yield=value*yield) %>%
  group_by(f, nutrient) %>%
  summarise(yield = sum(yield), nut.yield=sum(nut.yield)) 

## scale yields to get proportional nutrient curves
y.nut.scale<-y_nut %>%
  ungroup() %>% group_by(nutrient) %>%
  ## rescale nutrients to proportion of max.
  mutate(max.yield = max(yield), max.nut=max(nut.yield)) %>% 
  mutate(yield.scale=yield / max.yield, nut.scale = nut.yield/max.nut) %>%
  ungroup()

## now catch-weight nutrient concentration per f level
nut_yield_species<-yield_species %>%
  pivot_longer(-c(year, species, yield, ScientificName, f), names_to = 'nutrient', values_to = 'value') %>%
  mutate(nut.yield=value*yield) %>%
  group_by(f, nutrient) %>%
  mutate(ty = sum(nut.yield)) %>% ungroup() %>%
  group_by(f, species, nutrient) %>%
  mutate(nut.weight=nut.yield / ty) 

## scale total biomass, mean weight and collapsed stocks as proportion of maximum
tb.scale<-tot_biomass %>% mutate(max.b = max(biomass), b.scale = biomass/max.b) 
mw.scale<-mean_weight %>% mutate(max.mw = max(mean_weight), mw.scale = mean_weight/max.mw)
cl.scale<-collapsed %>% mutate(max.cl = 12, cl.scale = n.collapse/max.cl)


## save all curves in 1 dataframe
curves<-rbind(y.nut.scale %>% mutate(group = nutrient, value = nut.yield, scaled = nut.scale) %>% select(f, group, value, scaled),
               tb.scale %>% mutate(group= 'Fishable biomass', value = biomass, scaled = b.scale) %>% select(f, group, value, scaled),
               mw.scale %>% mutate(group= 'Mean weight', value = mean_weight, scaled = mw.scale) %>% select(f, group, value, scaled),
               cl.scale %>% mutate(group= 'Collapsed species', value = n.collapse, scaled = cl.scale) %>% select(f, group, value, scaled))

## assign ordered names for plotting
curves$group<-factor(curves$group, 
                      levels=c('calcium.mg', 'iron.mg', 'selenium.mug', 'zinc.mg', 'vitamin_a.mug', 'vitamin_d.mug',
                               'omega3.g','Nutrient density', 'Total yield', 'Fishable biomass', 'Mean weight', 'Collapsed species'))

## save output
save(
  yield_species,
  nut_yield_species,
  tot_biomass,
  biomass_species,
  mean_weight,
  collapsed,
  curves,
file='NorthSea_mMNY_simulated.rds')

