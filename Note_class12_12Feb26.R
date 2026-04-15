## https://www.appsilon.com/post/r-ggmap


library(ggmap)
location = "Orem"

map = get_map(location = location, size = 12)
ggmap(map)

geocode("Orem")
geocode("800 W University Pkwy, Orem, UT")
geocode("White house")

## with your own data
df_brassica <- read_csv('/Users/yu-yaliang/Documents/Lab/Brassica/Data/test_Brassica_accession_info.csv')
View(df_brassica)

## add longitude and latitude info
df_brassica$long <- geocode(df_brassica$country)[1]
df_brassica$lat <- geocode(df_brassica$country)[2]
df_brassica$long <- geocode(df_brassica$Origin)[1]
df_brassica$lat <- geocode(df_brassica$Origin)[2]
head(df_brassica)

## make map
world.map <- get_map(location = c(lon = 20, lat = 30), 
                     maptype = 'terrain', 
                     source = 'google', 
                     zoom = 1)

# Plot the map and add simple dots
ggmap(world.map) + 
  geom_point(data=df_brassica, 
             aes(x=long$lon, y=lat$lat), 
             color='red',size=2) +
  theme(axis.ticks = element_blank(), axis.text = element_blank())+
  xlab('')+ylab('')


library(leaflet)
leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = -77.0, lat = 38.9)


leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = -122.4149, lat = 37.7749,
             popup = 'Hello!!!') %>% 
  addMarkers(lng = -122.5, lat = 37.8,
             popup = 'Hello2!!!') 


## plot average body mass of penguins by sex and species
## try to add error bar

p3 = penguins %>% 
  filter(!is.na(sex)) %>% 
  group_by(species, sex) %>% 
  summarise(avg_mass = mean(body_mass_g),
            sd_mass = sd(body_mass_g)) %>% 
  ggplot(aes(x = species,
             y = avg_mass,
             fill = sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(ymin = avg_mass - sd_mass, ymax = avg_mass + sd_mass),
                position = position_dodge2(width = 0.5, padding = 0.5))


sd(penguins$body_mass_g, na.rm = T)

## 

penguins %>% 
  ggplot(aes(x = as.factor(year),
             y = body_mass_g,
             color = species)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, aes(color = species))

p2 = penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = as.factor(year), 
             y = body_mass_g, 
             color = species)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), 
              alpha = 0.5)

## in penguin dataset add a new col called mass_100 and save to an new obj
## value = body mass g + 100

new_peng = penguins %>% 
  mutate(mass_100 = body_mass_g + 100) 

View(new_peng)

new_peng %>% 
  select(- mass_100) %>% View()

new_peng %>% 
  select(- c(species, mass_100, sex)) %>% View()


View(new_peng)
new_peng[ , -length(new_peng)-]

length(new_peng)
ncol(new_peng)

## recreate penguin plot 
p1 = penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = bill_depth_mm,
             y = body_mass_g,
             color = sex)) +
  geom_point(size = 4, alpha = 0.75) +
  facet_grid(~ species) +
  scale_color_viridis_d(end = 0.8) +
  labs(x = 'Bill depth (mm)',
       y = 'Body mass (g)',
       color = 'Sex') + 
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold', size = 12),
        axis.title = element_text(face = 'bold', size = 12))
  


## patchwork
library(patchwork)
p1
p2
p3

p1 + p2
p1 + p2 + p3
p1 /p2


(p1+p2)/p3 +
plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

## read '/Users/yu-yaliang/Desktop/Data_Course_LASTNAME/Data/DatasaurusDozen.tsv'
## exam and make a good graph 


