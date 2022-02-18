pacman::p_load(mizer, tidyverse, skimr, cowplot, here, install=TRUE)
theme_set(theme_bw())

## Goal: simulate North Sea community under varying fishing pressure

# Reference guides:
# https://github.com/sizespectrum/mizer
# https://sizespectrum.org/mizer/articles/mizer.html

## load nutrient profiles
nutrients<-read.csv(file = 'nutrient_profiles.csv')

## for north sea species (n = 12), data on w_inf, w_mat, beta, sigma, k_vb and R_max are provided

## load calibrated North Sea model
load(file = 'NorthSea_calibrated.rds')

##clean some params
params@gear_params$initial_effort<-NULL

### set starting effort position = simulated FMSY
start_effort<-t(as.matrix(fmsy$fmsy))
colnames(start_effort)<-fmsy$species
rownames(start_effort)<-'2010'

unfished <- project(params, effort = 0, dt = 0.25, t_save = 1, t_max=100)
unfish.biom<-getBiomass(unfished, min_w = 10, max_w = 100e3) %>% data.frame() %>%
  mutate(year = rownames(getBiomass(unfished))) %>%
  pivot_longer(-year, names_to = 'species', values_to = 'biomass') %>%
  group_by(year, species) %>%
  summarise(unfished_biomass = biomass) %>% 
  filter(year == 99)

unfish_ref<-sum(unfish.biom$unfished_biomass)

## and shift F by a multiplier (not set at fixed value for all species)
f=seq(0, 20, by =0.05)

## get F baseline as simulated FMSY
f_base<-start_effort

## empty vectors to be filled in simulations
yield_species<-numeric() 
tot_biomass<-numeric() 
biomass_species<-numeric() 
mean_weight<-numeric() 
cl<-numeric() 

## loop over each f value, simulate to 2100, save 2050 results
i=0
 repeat{
    i = i + 1
    
    f.run <- c(Sprat = f_base[11]*f[i], Sandeel = f_base[9]*f[i], N.pout = f_base[6]*f[i], 
              Herring = f_base[5]*f[i], Dab = f_base[2]*f[i],
              Whiting = f_base[12]*f[i], Sole = f_base[10]*f[i], Gurnard = f_base[3]*f[i], 
              Plaice = f_base[7]*f[i], Haddock = f_base[4]*f[i],
              Cod = f_base[1]*f[i], Saithe = f_base[8]*f[i])

    scenario2 <- t(array(f.run, dim=c(12,90), dimnames=list(NULL,year = 2011:2100)))

    # start from time-averaged 2000-2010, then linearly shift towards f.run levels over 5 years
    scenario2 <- rbind(start_effort, scenario2)
    for (sp in dimnames(scenario2)[[2]]){
      scenario2[as.character(2011:2015),sp] <- scenario2["2010",sp] +
        (((scenario2["2015",sp] - scenario2["2010",sp]) / 5) * 1:5)
    }

    sim2 <- project(params, effort = scenario2, dt = 0.25, t_save = 1)
    
    ## ssb
    gb<-plotBiomass(sim2)
    
    t<-getBiomass(sim2, min_w = 10, max_w = 100e3) %>% data.frame() %>%
        mutate(year = rownames(getBiomass(sim2))) %>%
        pivot_longer(-year, names_to = 'species', values_to = 'biomass') %>%
        group_by(year) %>%
        summarise(biomass = sum(biomass)) %>%
        filter(year == 2100) %>% 
        mutate(f = f[i])
    
    bs<-getBiomass(sim2, min_w = 10, max_w = 100e3) %>% data.frame() %>%
      mutate(year = rownames(getBiomass(sim2))) %>%
      pivot_longer(-year, names_to = 'species', values_to = 'biomass') %>%
      group_by(year, species) %>%
      summarise(biomass = sum(biomass)) %>%
      filter(year == 2100) %>% 
      mutate(f = f[i])
    
    ## collapsed species
    collapse<-getBiomass(sim2, min_w = 10, max_w = 100e3) %>% data.frame() %>%
      mutate(year = rownames(getBiomass(sim2))) %>%
      pivot_longer(-year, names_to = 'species', values_to = 'biomass') %>%
      group_by(year, species) %>%
      summarise(min_biomass = min(biomass)) %>%
      filter(year %in% c(2100))
    
    collapse$unfished_biomass<-unfish.biom$unfished_biomass[match(collapse$species, unfish.biom$species)]
    collapse$prop<-with(collapse, min_biomass / unfished_biomass)*100
    collapse$collapsed<-ifelse(collapse$prop < 10, 1, 0)
    collapse$f <- f[i]
    
    collapsed<-data.frame(n.collapse = sum(collapse$collapsed), f = f[i])
    
    ## mean weight
    gmw<-getMeanWeight(sim2, min_w = 10, max_w = 100e3) %>% data.frame() %>% 
      ggplot() + geom_line(aes(rownames(scenario2)[1]:last(rownames(scenario2)), .)) +
      labs(x = '', y = 'Mean weight, kg')
    
    w<-getMeanWeight(sim2, min_w = 10, max_w = 100e3) %>% last()
    w<-data.frame(mean_weight = w, year=2100, f = f[i])
    
    ## yield
    y<-data.frame(getYield(sim2))
    y$year<-as.numeric(rownames(y))
    y<-y %>% pivot_longer(-year, names_to = 'species', values_to = 'yield')
    
    gy<-ggplot(y, aes(year, yield, col=species)) + geom_line() +
      theme(legend.position = c(0.2, 0.6)) +
      labs(x = '', y = 'Yield, tonnes', title = paste0('Fishing mortality = ', f[i]))
    
    
    print(cowplot::plot_grid(gy, gb, gmw, nrow=1))
    
    
    y<-y %>% filter(year == 2100) %>% mutate(f = f[i])
    
    yield_species<-rbind(yield_species, y)
    tot_biomass<-rbind(tot_biomass, t)
    biomass_species<-rbind(biomass_species, data.frame(bs))
    mean_weight<-rbind(mean_weight, w)
    cl<-rbind(cl, collapsed)
  
  if( t$biomass / unfish_ref < 0.1 ){break}
  }

## match in nutrient concentrations
yield_species$ScientificName<-nutrients$ScientificName[match(yield_species$species, nutrients$species)]
yield_species$calcium.mg<-nutrients$calcium.mg[match(yield_species$ScientificName,nutrients$ScientificName)]
yield_species$iron.mg<-nutrients$iron.mg[match(yield_species$ScientificName,nutrients$ScientificName)]
yield_species$selenium.mug<-nutrients$selenium.mug[match(yield_species$ScientificName,nutrients$ScientificName)]
yield_species$zinc.mg<-nutrients$zinc.mg[match(yield_species$ScientificName,nutrients$ScientificName)]
yield_species$omega3.g<-nutrients$omega3.g[match(yield_species$ScientificName,nutrients$ScientificName)]
yield_species$vitamin_a.mug<-nutrients$vitamin_a.mug[match(yield_species$ScientificName,nutrients$ScientificName)]
yield_species$vitamin_d.mug<-nutrients$value[nutrients$nutrient == 'vitamin_d.mug'][match(yield_species$ScientificName,nutrients$ScientificName[nutrients$nutrient == 'vitamin_d.mug'])]

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
cl.scale<-cl %>% mutate(max.cl = 12, cl.scale = n.collapse/max.cl)


## save all curves in 1 dataframe
curves<-rbind(y.nut.scale %>% mutate(group = nutrient, value = nut.yield, scaled = nut.scale) %>% select(f, group, value, scaled),
               y.nut.scale %>% filter(nutrient == 'iron.mg') %>% mutate(group = 'Total yield', value = yield, scaled = yield.scale) %>% select(f, group, value, scaled),
               tb.scale %>% mutate(group= 'Fishable biomass', value = biomass, scaled = b.scale) %>% select(f, group, value, scaled),
               mw.scale %>% mutate(group= 'Mean weight', value = mean_weight, scaled = mw.scale) %>% select(f, group, value, scaled),
               cl.scale %>% mutate(group= 'Collapsed species', value = n.collapse, scaled = cl.scale) %>% select(f, group, value, scaled))

## assign ordered names for plotting
curves$group<-factor(curves$group, 
                      levels=c('calcium.mg', 'iron.mg', 'selenium.mug', 'zinc.mg', 'vitamin_a.mug', 'vitamin_d.mug',
                               'omega3.g','Nutrient density', 'Total yield', 'Fishable biomass', 'Mean weight', 'Collapsed species'))

## fishing mortality metric
f_mort<-data.frame(f = y_nut$f[y_nut$nutrient=='calcium.mg'], 
            yield = y_nut$yield[y_nut$nutrient=='calcium.mg']) %>% 
    left_join(tot_biomass, by = 'f')
f_mort$f_mort<-f_mort$yield / f_mort$biom

curves$f_mort<-f_mort$f_mort[match(curves$f, f_mort$f)]
yield_species$f_mort<-f_mort$f_mort[match(yield_species$f, f_mort$f)]
nut_yield_species$f_mort<-f_mort$f_mort[match(nut_yield_species$f, f_mort$f)]


## save output
# save(
#   yield_species,
#   nut_yield_species,
#   tot_biomass,
#   biomass_species,
#   mean_weight,
#   collapsed,
#   curves,
# file='NorthSea_mMNY_simulated.rds')

