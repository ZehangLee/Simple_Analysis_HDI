cities <- c("City A", "City B", "City C", "City D", "City E")
regions <- c("Region M", "Region N", "Region O")
countries <- c("Country Z", "Country X", "Country Y", "Country W")
geography_all <- as.factor(c(cities, regions, countries))
year <- as.factor(2011:2014)

df <- expand.grid(geography = geography_all, year = year)
df$value <- runif(48)


test=hdi.databank.m %>% 
  filter(Region %in% c("East Asia and Pacific","Europe and Central Asia")) %>%
  filter((year>= 1992) &(year<= 1994)) %>%
  replace_na(list(hdi=0))%>%
  group_by(Region,indicator_name)%>%summarise(avg = mean(hdi))
  
test=test %>%
  mutate_at(vars(avg),funs(rescale)) 
test%>%spread(indicator_name,avg)->test2




subset(hdi.databank.m,c("Region","year","hdi","indicator_name"))
       
ggradar(test2[,1:5],grid.max=max(test$avg)) 



hdi.databank.m %>% 
  filter(Region %in%  c("East Asia and Pacific","Europe and Central Asia"))





          
          test_heal_plot_fun=function(heal.level.in,heal.geography.in,date_from,date_to,plot_type){
            if (is.null(heal.level.in)==TRUE){
              heal.overview1 <- hdi.databank.m %>% 
                filter( Region %in% heal.geography.in) %>%
                filter((year>= date_from) & (year<= date_to)) %>%
                replace_na(list(hdi=0))%>%
                group_by(Region,indicator_name)%>%summarise(avg = mean(hdi))
              
              
              heal.overview2 <- hdi.databank.m %>% 
                filter(country_name %in% heal.geography.in) %>%
                filter((year>= date_from) &(year<= date_to)) %>%
                replace_na(list(hdi=0))%>%
                group_by(country_name,indicator_name)%>%summarise(avg = mean(hdi))
              colnames(heal.overview2)[1]="Region"
              
              heal.overview <- rbind(heal.overview1,heal.overview2)
              # heal.overview %>%
              #   mutate_at(vars(avg),funs(rescale)) 
              
              heal.overview.final=heal.overview%>%spread(indicator_name,avg)
              colnames(heal.overview.final)[1]="geo"
              colnames(heal.overview.final)=gsub(" ",".",colnames(heal.overview.final))
              colnames(heal.overview.final)=gsub(",","",colnames(heal.overview.final))
              #colnames(heal.overview.final)=gsub("(","",colnames(heal.overview.final))
              #colnames(heal.overview.final)=gsub(")","",colnames(heal.overview.final))
              
              
              heal.overview.final=heal.overview.final[,c("geo",
                                                         "Current.health.expenditure.(%.of.GDP)",
                                                         "HIV.prevalence.adult.(%.ages.15-49)",
                                                         "Life.expectancy.at.birth.(years)",
                                                         "Mortality.rate.infant.(per.1000.live.births)",
                                                         "Mortality.rate.under-five.(per.1000.live.births)")]
              heal.overview.final[is.na(heal.overview.final)]=0
              
              heal.overview.final$`HIV.prevalence.adult.(%.ages.15-49)`=heal.overview.final$`HIV.prevalence.adult.(%.ages.15-49)`*10
              colnames(heal.overview.final)[3]="HIV.prevalence.adult.(per.1000.ages.15-49)"
              
              heal.life1=hdi.databank.m %>%
                filter( Region %in%  heal.geography.in) %>%
                filter((year>= date_from) & (year<= date_to)) %>%
                filter( indicator_name =="Life expectancy at birth (years)")%>%
                select(Region,year,hdi)%>%arrange(year,Region)%>%drop_na(hdi)%>%
                group_by(year,Region)%>%  summarise(avg = mean(hdi))
              
              heal.life2=hdi.databank.m %>%
                filter( country_name %in%  heal.geography.in) %>%
                filter((year>= date_from) & (year<= date_to)) %>%
                filter( indicator_name =="Life expectancy at birth (years)")%>%
                select(country_name,year,hdi)%>%arrange(year,country_name)%>%drop_na(hdi)%>%
                group_by(year,country_name)%>%  summarise(avg = mean(hdi))
              colnames(heal.life2)[2]="Region"
              
              heal.life <- rbind(heal.life1,heal.life2)
              colnames(heal.life)[2]="geo"
              
              # 
              # heal.life1=hdi.databank.m %>% 
              #   filter( Region %in% heal.geography.in) %>%
              #   filter((year>= date_from) & (year<= date_to)) %>%
              #   filter( indicator_name =="Life expectancy at birth (years)")%>%
              #   group_by(year,country_name)
              
              
              # radar=ggradar(heal.overview.final,grid.mid = 50,grid.max = 100)
              # hiv.plot=ggplot(data=heal.overview.final,mapping = aes(x=geo,y=`HIV.prevalence.adult.(per.1000.ages.15-49)`))+
              #   geom_bar(stat="identity")+
              #   coord_flip()
            }else{
              heal.overview <- hdi.databank.m %>% 
                filter( level %in% heal.level.in) %>%
                filter((year>= date_from) & (year<= date_to)) %>%
                replace_na(list(hdi=0))%>%
                group_by(level,indicator_name)%>%summarise(avg = mean(hdi))
              
              heal.overview.final=heal.overview%>%spread(indicator_name,avg)
              colnames(heal.overview.final)=gsub(" ",".",colnames(heal.overview.final))
              colnames(heal.overview.final)=gsub(",","",colnames(heal.overview.final))
              #colnames(heal.overview.final)=gsub("(","",colnames(heal.overview.final))
              #colnames(heal.overview.final)=gsub(")","",colnames(heal.overview.final))
              
              
              heal.overview.final=heal.overview.final[,c("level",
                                                         "Current.health.expenditure.(%.of.GDP)",
                                                         "HIV.prevalence.adult.(%.ages.15-49)",
                                                         "Life.expectancy.at.birth.(years)",
                                                         "Mortality.rate.infant.(per.1000.live.births)",
                                                         "Mortality.rate.under-five.(per.1000.live.births)")]
              heal.overview.final[is.na(heal.overview.final)]=0
              
              heal.overview.final$`HIV.prevalence.adult.(%.ages.15-49)`=heal.overview.final$`HIV.prevalence.adult.(%.ages.15-49)`*10
              colnames(heal.overview.final)[3]="HIV.prevalence.adult.(per.1000.ages.15-49)"
              
              heal.life=hdi.databank.m %>%
                filter( level %in%  heal.level.in) %>%
                filter((year>= date_from) & (year<= date_to)) %>%
                filter( indicator_name =="Life expectancy at birth (years)")%>%
                select(level,year,hdi)%>%arrange(year,level)%>%drop_na(hdi)%>%
                group_by(year,level)%>%  summarise(avg = mean(hdi))
              
              # radar=ggradar(heal.overview.final,grid.mid = 50,grid.max = 100)
              # hiv.plot=ggplot(data=heal.overview.final,mapping = aes(x=level,y=`HIV.prevalence.adult.(per.1000.ages.15-49)`))+
              #   geom_bar(stat="identity")+
              #   coord_flip()
              
            }

            
            return(heal.overview.final)
          }

heal.level.in=as.null()
#heal.level.in=c("VERY HIGH HUMAN DEVELOPMENT","LOW HUMAN DEVELOPMENT","MEDIUM HUMAN DEVELOPMENT","HIGH HUMAN DEVELOPMENT")
heal.overview.test=test_heal_plot_fun(heal.level.in,date_from=2013,date_to=2016,
                                           heal.geography.in=c("Arab State","Botswana","Central African Republic","Congo"),plot_type = "")

ggradar(heal.overview.test,grid.mid = 50,grid.max = max(heal.overview.test[,2:6])+10)

# ggplot(data = heal.overview.test,mapping = aes(level,y=factor(`Current.health.expenditure.(%.of.GDP)`),fill=level))+
#   geom_bar(width=0.5,stat="identity")+
#   coord_polar("y")
# 
# ggplot(data = heal.overview.test,mapping = aes(x=))
# 
# heal.life1=hdi.databank.m %>%
#   filter( level %in% c("VERY HIGH HUMAN DEVELOPMENT","LOW HUMAN DEVELOPMENT","MEDIUM HUMAN DEVELOPMENT","HIGH HUMAN DEVELOPMENT")) %>%
#   filter((year>= 2000) & (year<= 2016)) %>%
#   filter( indicator_name =="Life expectancy at birth (years)")%>%
#   group_by(year,level)%>% select(level,year,hdi)

library(leaflet)
library(rworldmap)
library(RColorBrewer)


map=getMap()         



#####################################################################

demo.data<-hdi.databank.m%>%filter(indicator_name %in%
                                     c("Total population (millions)","Urban population (%)",
                                       "Young age (0-14) dependency ratio (per 100 people ages 15-64)",
                                       "Old-age (65 and older) dependency ratio (per 100 people ages 15-64)"))%>%
  filter((year>= 2015) & (year<= 2017))%>%
  select(iso3,country_name,year,indicator_name,hdi,level,Region)%>%
  arrange(year,level)%>%drop_na(hdi)%>%
  group_by(indicator_name,iso3,country_name,level,Region)%>%  summarise(avg = mean(hdi))%>%
  spread(indicator_name,avg)%>%replace_na(list(`Old-age (65 and older) dependency ratio (per 100 people ages 15-64)`=0,
                                               `Total population (millions)`=0,
                                               `Urban population (%)`=0,
                                               `Young age (0-14) dependency ratio (per 100 people ages 15-64)`=0))

colnames(demo.data)=gsub(" ",".",colnames(demo.data))
#colnames(demo.data)=gsub("%","percent",colnames(demo.data))
map$`Old-age.(65.and.older).dependency.ratio.(per.100.people.ages.15-64)`=pull(demo.data[match(map$ISO3,demo.data$iso3),"Old-age.(65.and.older).dependency.ratio.(per.100.people.ages.15-64)"])
map$`Total.population.(millions)`=pull(demo.data[match(map$ISO3,demo.data$iso3),"Total.population.(millions)" ])
map$`Urban.population.(%)`=pull(demo.data[match(map$ISO3,demo.data$iso3),"Urban.population.(%)" ])
map$`Young.age.(0-14).dependency.ratio.(per.100.people.ages.15-64)`=pull(demo.data[match(map$ISO3,demo.data$iso3),"Young.age.(0-14).dependency.ratio.(per.100.people.ages.15-64)"])
map$level=pull(demo.data[match(map$ISO3,demo.data$iso3),"level"])

cc<-brewer.pal(4,"Set3")

pal <- colorFactor(palette =cc, domain = map$level,na.color = "#808080")
map$labels <- paste0("<strong> Country: </strong> ", map$NAME, "<br/> ",
                     "<strong> Total population (millions): </strong> ", map$`Total.population.(millions)`, "<br/> ",
                     "<strong> Urban population (%): </strong> ", map$`Urban.population.(%)`, "<br/> ",
                     "<strong> Young age (0-14) dependency ratio (per 100 people ages 15-64): </strong> ", map$`Young.age.(0-14).dependency.ratio.(per.100.people.ages.15-64)`, "<br/> ",
                     "<strong> Old-age (65 and older) dependency ratio (per 100 people ages 15-64): </strong> ", map$`Old-age.(65.and.older).dependency.ratio.(per.100.people.ages.15-64)`, "<br/> ") %>%
  lapply(htmltools::HTML)
leaflet(map) %>% addTiles() %>%
  setView(lng = 0, lat = 30, zoom = 2) %>%
  addPolygons(
    fillColor = ~pal(map$level),
    color = "grey",
    fillOpacity = 0.7,
    label = ~labels,
    highlight = highlightOptions(color = "black", bringToFront = TRUE)) %>%
  leaflet::addLegend(pal = pal, values = ~map$level, opacity = 0.7, title = 'Development levels')

demo_plot_fun(2003,2107,gen.plot = TRUE)

