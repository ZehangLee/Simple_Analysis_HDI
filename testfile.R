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

heal.level.in=as.null()#test for country and region
#heal.level.in=c("VERY HIGH HUMAN DEVELOPMENT","LOW HUMAN DEVELOPMENT","MEDIUM HUMAN DEVELOPMENT","HIGH HUMAN DEVELOPMENT")
heal.overview.test=test_heal_plot_fun(heal.level.in,date_from=2013,date_to=2016,
                                           heal.geography.in=c("Arab State","Botswana","Central African Republic","Congo"),plot_type = "")

ggradar(heal.overview.test,grid.mid = 50,grid.max = max(heal.overview.test[,2:6])+10,
        axis.label.size = 3,axis.label.offset = 0.7)+
        theme(legend.position = 'bottom')+
        guides(fill=guide_legend(nrow=2,byrow = TRUE))

cc<-brewer.pal(4,"Set3")

#hiv.plot=
  ggplot(data=heal.overview.test,mapping = aes(x=geo,y=`HIV.prevalence.adult.(per.1000.ages.15-49)`,fill=cc))+
  geom_bar(stat="identity",width = 0.5)+
  scale_fill_manual(values = cc)+
  labs(title = "HIV Prevalence Adult ‰ (Age 15 to 49)",
    x = ' ',
    y = ' ')+
  theme_minimal()+
  theme(plot.title = element_text(size=14,face = "bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")+
  geom_text(aes(label=round(`HIV.prevalence.adult.(per.1000.ages.15-49)`,3)),size=3,hjust=0,color="gray27")+
  coord_flip()


expend.plot=
  ggplot(data = heal.overview.test,mapping = aes(geo,y=factor(`Current.health.expenditure.(%.of.GDP)`),fill=geo))+
  theme_minimal()+
  labs(title = "Current Health Expenditure of GDP %",
    x = ' ',
    y = ' ')+
  geom_bar(width=0.5,stat="identity")+
  scale_fill_manual(values = cc)+
  coord_polar("y")+
  guides(fill=guide_legend(nrow=1))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())
  
  
  
  heal.life=hdi.databank.m %>%
    filter( level %in%  unique(hdi.databank.m$level)) %>%
    filter((year>= 2013) & (year<= 2017)) %>%
    filter( indicator_name =="Life expectancy at birth (years)")%>%
    select(level,year,hdi)%>%arrange(year,level)%>%drop_na(hdi)%>%
    group_by(year,level)%>%  summarise(avg = mean(hdi))
  
  
 life.plot=
   ggplot(data=heal.life)+
      theme_minimal()+
      labs(title = "Life Expectancy at Birth",
        x = ' ',
        y = ' ')+
   geom_line(aes(x=as.numeric(year),y=avg,group=level,colour =level),size=2)+
      scale_color_manual(values=cc)+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position = "bottom",
            legend.title = element_blank())+
      guides(col=guide_legend(nrow=2,byrow = TRUE))
  

heal.mortal.test=heal.overview.test%>%select(c(1,5,6))%>%gather(key="mortal.index",
                                                            "Mortality.rate.infant.(per.1000.live.births)",
                                                            "Mortality.rate.under-five.(per.1000.live.births)",
                                                            value = "values")

mortal.plot=
ggplot(data=heal.mortal.test)+
  theme_minimal()+
  labs(title = "Mortality Rate of Children",
    x = ' ',
    y = ' ')+
 geom_bar(aes(x=geo,y=values, fill=mortal.index),stat = "identity",position = "stack",width=0.5)+
 scale_fill_manual(values = cc,labels=c("Mortality Rate Infant ‰","Mortality Rate Under-Five ‰"))+
  guides(fill=guide_legend(nrow=1))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())















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


      



#####################################################################
map=getMap()   
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

cc<-brewer.pal(5,"Set3")

pal <- colorFactor(palette =cc, domain = map$level,na.color = "#808080")


pal <- colorFactor("YlOrRd", domain = map$level, na.color = "#808080")

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
    weight = 2,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    dashArray = "3",
    label = ~labels,
    highlight = highlightOptions(color = "#666", bringToFront = TRUE),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  leaflet::addLegend(pal = pal, values = ~map$level, opacity = 0.7, title = 'Development levels',position="bottomright")


#############################
demo_plot_fun(2003,2107,gen.plot = TRUE)

<<<<<<< HEAD
test=demo.data%>%gather(`Old-age (65 and older) dependency ratio (per 100 people ages 15-64)`,
            `Total population (millions)`,
            `Urban population (%)`,
            `Young age (0-14) dependency ratio (per 100 people ages 15-64)`,key = "indictor_name",value = "value")
######################################################################################
income.data<-hdi.databank.m%>%filter(indicator_name %in% 
                                       c("Income index",
                                         "Gross national income (GNI) per capita (2011 PPP $)"))%>%
  filter((year>= 2013) & (year<= 2017)) %>%
  select(country_name,year,indicator_name,hdi,level,Region)%>%
  arrange(year,level)%>%drop_na(hdi)%>%
  group_by(year,indicator_name,country_name,level,Region)%>%  summarise(avg = mean(hdi))%>%
  spread(indicator_name,avg)%>%replace_na(list(`Income index`=0,
                                               `Gross national income (GNI) per capita (2011 PPP $)`=0))
=======
ggplot(data=NULL,aes(x=country_name))+
           geom_bar(aes(y=`Urban population (%)`,fill="Urban population (%)"),data=demo.data[9,],stat = "identity")+
           geom_bar(aes(y=`Young age (0-14) dependency ratio (per 100 people ages 15-64)`,fill="Young age (0-14) dependency ratio (per 100 people ages 15-64)"),data=demo.data[9,],stat = "identity")+
           geom_bar(aes(y=`Old-age (65 and older) dependency ratio (per 100 people ages 15-64)`,fill="Old-age (65 and older) dependency ratio (per 100 people ages 15-64)"),data=demo.data[9,],stat = "identity")
         

>>>>>>> 29b8e344128d6975dff771dd4552695c3b4f4a8a

