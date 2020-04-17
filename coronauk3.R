install.packages("mapdata")
install.packages("maptools")
install.packages("rgdal")
install.packages("ggmap")
install.packages("rgeos")
install.packages("broom")
install.packages("plyr")
install.packages("anchors")
install.packages("viridis")
install.packages("cowplot")
install.packages("ggforce")


#Clear the memory
rm(list=ls())
#Download some important packages
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ggmap)
library(ggplot2)
library(rgeos)
library(broom)
library(plyr)
library(anchors)
library(reshape)
library(viridis)
library(tidyr)
library(dplyr)
library(cowplot)
library(ggforce)

test$id <-revalue(test$id, c("Bournemouth, Christchurch and Poole" = "Bournemouth"))
test$id <-revalue(test$id, c("Cornwall and Isles of Scilly" = "Cornwall"))
uapop$id <-revalue(uapop$id, c("Bournemouth, Christchurch and Poole" = "Bournemouth"))

#combine cases data with uapop data
newdf <- join(test, uapop, by = "id")
newdf$csht <- (newdf$cases/newdf$Estimated.Population.mid.2018)*100000

#set up qua?tiles and fills

{quantiles_cases <- newdf %>%
    pull(csht) %>%
    quantile(probs = seq(0, 1, length.out = 4), na.rm=TRUE)
  
  quantiles_pop <- newdf %>%
    pull(X2018.people.per.sq..km) %>%
    quantile(probs = seq(0, 1, length.out = 4), na.rm=TRUE)
  
  bivariate_color_scale <- tibble(
    "3 - 3" = "#3F2949", # high inequality, high income
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low inequality, high income
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium inequality, medium income
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high inequality, low income
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low inequality, low income
  ) %>%
    gather("group", "fill")
  
  # cut into groups defined above and join fill
  newdf %<>%
    mutate(
      cases_quantiles = cut(
         csht,
        breaks = quantiles_cases,
        include.lowest = TRUE
      ),
      pop_quantiles = cut(
        X2018.people.per.sq..km,
        breaks = quantiles_pop,
        include.lowest = TRUE
      ),
      # by pasting the factors together as numbers we match the?groups defined
      # in the tibble bivariate_color_scale
      group = paste(
        as.numeric(cases_quantiles), "-",
        as.numeric(pop_quantiles)
      )
    ) %>%
    # we now join the actual hex values per "group"
    # so each municipality knows its hex value?based on the his gini and avg
    # income value
    left_join(bivariate_color_scale, by = "group")
}

newdf$group <- NULL

#Load the shapefile - make sure you change the filepath to where you saved the shapefiles
shapefile <- readOGR(dsn="C:/Users/Jonathan/Downloads/CUAs", layer="Counties_and_Unitary_Authorities_December_2017_Full_Extent_Boundaries_in_UK_WGS84")
str(shapefile)
#Reshape for ggplot2 using the Broom package
mapdata <- tidy(shapefile, region="ctyua17nm") #This might take a few minutes

#Check the s?apefile has l?aded correctly by plotting an outline map of the UK
gg <- ggplot() + geom_polygon(data = mapdata, aes(x = long, y = lat, group = group), color = "#FFFFFF", size = 0.25)
gg <- gg + coord_fixed(1) #This gives the map a 1:1 aspect ratio to preve?t the map fro? appearing squashed
print(gg)

#some data cleaning
unique_map <- as.data.frame(unique(mapdata$id))
unique_test <- as.data.frame(unique(test$id))

mapdata$id <-revalue(mapdata$id, c("Hackney" = "Hackney and City of London"))
mapdata$id <-revalue(mapdata$id, c("City of London" = "Hackney and City of London"))

#Join newdf  with mapdata
df <- join(mapdata, newdf, by="id")

#Create the heatmap using the ggplot2 package
gg <- ggplot() + geom_polygon(data = df, 
                              aes(x = long, y = lat, group = group, fill = fill), 
                              color = "#FFFFFF", 
                              size = 0.25)
gg <- gg + scale_fill_identity()
gg <- gg + coord_fixed(ylim = c(49.5, 56), xlim = c(-7, 1.4), ratio = 1.3)##(ylim = c(51.25, 51.75),?xlim=-c(-0.75, 0.65), ratio=1.3)
gg <- gg + theme_minimal()
gg <- gg + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
gg <- gg + theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
gg <- gg + theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) 
gg <- gg + theme(legend.position = "bottom")
gg <- gg + labs(x = NULL,
                y = NULL,
                title = "England population density and COVID-19 cases",
                subtitle = paste0("People per square kilometre and COVID-19 cases per 100k people (April 2nd 2020)"),
                caption = "Sources: ONS and PHE",
                hjust = -1, size =20)

print(gg) 

ldn <- ggplot() + geom_polygon(data = df, 
                               aes(x = long, y = lat, group = group, fill = fill), 
                               color = "#FFFFFF", 
                               size = 0.25)
ldn <- ldn + scale_fill_identity()
ldn <- ldn + coord_fixed(ylim = c(51.25, 51.75), xlim=-c(-0.4, 0.65), ratio=1.3)
ldn <- ldn + theme_minimal()
ldn <- ldn + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ldn <- ldn + theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
ldn <- ldn + theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) 
ldn <- ldn + theme(legend.position = "bottom")
print(ldn) 

# LEGEND
bivariate_color_scale %<>%
   separate(group, into = c("cases", "pop"), sep = " - ") %>%
  mutate(cases = as.integer(cases),
         pop = as.integer(pop))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = cases,
      y =pop,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "More cases per
       100k population⟶️",
       y = "More people per 
       square km ⟶️") +
  theme_minimal() +
  # make font small enough
  theme(
    axis.title = element_text(size = 12),
axis.text = element_blank()
  ) +
  # quadratic tiles
  coord_fixed()

str(df)
#draw plot
ggdraw() +
  draw_plot(gg, 0, 0, 1, 1) +
  draw_plot(legend, 0.075, 0.2, 0.3, 0.3) +
  draw_plot(ldn, 0.075, 0.6, 0.4, 0.3)

str(df)
#univariate
uni <- ggplot() + geom_polygon(data = df, 
                               aes(x = long, y = lat, group = group, fill = csht), 
                               color = "#FFFFFF", 
                               size = 0.25) 
uni <- uni + scale_fill_viridis(option = "magma", direction = -1, name = "Cases per 100,000 population", na.value = "white",
                                guide = guide_colorbar(
                                  direction = "horizontal",
                                  barheight = unit(3, units = "mm"),
                                  barwidth = unit(50, units = "mm"),
                                  draw.ulim = F,
                                  title.position = 'top',
                                  title.hjust = 0.5,
                                  label.hjust = 0.5,
                                  trans = "log10"))
uni <- uni + coord_fixed(ylim = c(49.5, 56), xlim = c(-7, 1.4), ratio = 1.3)##(ylim = c(51.25, 51.75), xlim=-c(-0.75, 0.65), ratio=1.3)
uni <- uni + theme_minimal()
uni <- uni + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
uni <- uni + theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
uni <- uni + theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) 
uni <- uni + theme(legend.position = "bottom", legend.text = element_text(size = 13), legend.title = element_text(size = 15))
uni <- uni + labs(x = NULL,
                  y = NULL,
                  title = "Cases of COVID-19 by local authority region",
                  subtitle = paste0("Cases per 100,000 inhabitants (April 2nd 2020)"),
                  caption = "Sources: ONS and PHE",
                  hjust = -1, size =20)

print(uni) 

ldn2 <- ggplot() + geom_polygon(data = df, 
                                aes(x = long, y = lat, group = group, fill = csht), 
                                color = "#FFFFFF", 
                                 size = 0.25)
ldn2 <- ldn2 + scale_fill_viridis(option = "magma", direction = -1, name = "Total Cases", na.value = "white",
                                  guide = guide_colorbar(
                                    direction = "horizontal",
                                    barheight = unit(3, units = "mm"),
                                    barwidth = unit(50, units = "mm"),
                                    draw.ulim = F,
                                    title.position = 'top',
                                    title.hjust = 0.5,
                                    label.hjust = 0.5,
                                    trans = "log10"))
ldn2 <- ldn2 + coord_fixed(ylim = c(51.25, 51.75), xlim=-c(-0.4, 0.65), ratio=1.3)
ldn2 <- ldn2 + theme_minimal()
ldn2 <- ldn2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ldn2 <- ldn2 + theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
ldn2 <- ldn2 + theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) 
ldn2 <- ldn2 + theme(legend.position = "none") 
print(ldn2) 


ggdraw() +
  draw_plot(uni, 0, 0, 1, 1) +
  draw_plot(ldn2, 0.075, 0.6, 0.4, 0.3)


ggdraw() +
  draw_plot(gg, 0, 0, 1, 1) +
  draw_plot(ldn, 0.075, 0.6, 0.3, 0.2) +
  draw_plot(legend, 0.075, 0.2, 0.3, 0.3) 

str(df)

stplt <- ggplot(newdf, 
                aes(x = X2018.people.per.sq.km, y = csht, colour = fill)) + geom_point(size = 4) + scale_colour_identity() +
  xlim(0, 18000) + ylim(-3, 120) + theme_minimal() + 
  theme(panel.grid.minor = element_blank()) +
  labs(x = "People per sq km", y = "Cases per 100,000 people", title="UK Population Density and COVID-19 cases (April 2nd 2020)") +
  #geom_mark_circle(aes(filter = id == "Cumbria", label = "Cumbria", description = "is sparsely populated but has a high case rate")) 
  #geom_mark_circle(aes(filter = id == "Kingston upon Hull, ?i?y of", label = "Kingston upon Hull", description = "a city with a low case rate")) 
  #geom_mark_circle(aes(filter = id == "Sheffield", label = "Sheffield", description = "has the highest case rate outside of London")) 
  #geom_mark_circle(aes(filter = id =? ?Havering", label = "Havering", description = "is the London Borough with the lowest case rate")) 
  
  print(stplt)

ggdraw() +
  draw_plot(stplt, 0, 0, 1, 1) +
  draw_plot(legend, 0.075, 0.2, 0.3, 0.3)





#annotating
gg <- gg + 
  annotate(
    geom = "curve", x = -1.5, y = 50, xend = -1.3, yend = 51, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = -1.4, y = 50, label = "Hampshire", hjust = "left")
print(gg)

midpoints <- mapdata %>%
  group_by(id) %>%
  summarise(midlat = mean(lat), midlong = mean(long))

df<- left_join(df, midpoints, by = "id")

