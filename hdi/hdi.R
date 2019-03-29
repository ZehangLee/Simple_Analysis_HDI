library(openxlsx)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggradar)
library(plotly)
library(leaflet)
library(rworldmap)
library(RColorBrewer)

hdi.databank=read.xlsx("http://hdr.undp.org/sites/default/files/2018_all_indicators.xlsx")

temp2 <- tempfile()
download.file("http://hdr.undp.org/sites/default/files/2018_statistical_annex_all.xlsx",temp2, mode="wb")
hdi.stat <- readxl::read_xlsx(temp2,sheet = 1,range = "A3:M198")
unlink(temp2)
rm(temp2)

###tidy table hdi.databank
year=as.character(seq(2000,2017))
drop_years=as.character(seq(1990,1999))
hdi.databank=hdi.databank%>%select(-c("9999",drop_years))%>%gather(one_of(year),key = "year",value = "hdi")



###tidy table hdi.stat
colnames(hdi.stat)=gsub(" ",".",colnames(hdi.stat))
colnames(hdi.stat)[c(1,2)]=c("HDI.rank","Country")
hdi.stat%>%slice(-c(1,2))%>%select(-c(4,6,8,10,12))->hdi.stat

level_one <- hdi.stat %>% select(Country) %>% filter (startsWith(Country,'VERY')) %>% pull
level_two <- hdi.stat %>% select(Country) %>% filter (startsWith(Country,'HIGH')) %>% pull
level_three <- hdi.stat %>% select(Country) %>% filter (startsWith(Country,'MEDIUM')) %>% pull
level_four <- hdi.stat %>% select(Country) %>% filter (startsWith(Country,'LOW')) %>% pull

level_one_index= which(hdi.stat$Country==level_one)
level_two_index= which(hdi.stat$Country==level_two)
level_three_index= which(hdi.stat$Country==level_three)
level_four_index=which(hdi.stat$Country==level_four)

hdi.stat.m=rbind(hdi.stat%>%slice((level_one_index+1):(level_two_index-1))%>%mutate(level=level_one),
                 hdi.stat%>%slice((level_two_index+1):(level_three_index-1))%>%mutate(level=level_two),
                 hdi.stat%>%slice((level_three_index+1):(level_four_index-1))%>%mutate(level=level_three),
                 hdi.stat%>%slice((level_four_index+1):nrow(hdi.stat))%>%mutate(level=level_four))

#merge level information from hdi.stat table
hdi.databank.m=hdi.databank%>%mutate(level=hdi.stat.m$level[match(hdi.databank$country_name,hdi.stat.m$Country)])%>%filter(!is.na(level))


#add region column to hdi.databank.m
hdi.databank.m$Region=rep(".",nrow(hdi.databank.m))



### East Asia and Pacific
hdi.databank.m[which(hdi.databank.m$country_name %in% c("Australia","Brunei Darussalam","Cambodia","China","Fiji",
                                                        "Hong Kong, China (SAR)","Indonesia","Japan","Kiribati",
                                                        "Korea (Republic of)", "Lao People's Democratic Republic",
                                                        "Malaysia","Marshall Islands","Myanmar","Micronesia (Federated States of)","Mongolia",
                                                        "New Zealand","Palau","Philippines","Samoa","Singapore","Solomon Islands", "Thailand", "Timor-Leste",
                                                        "Tonga","Vanuatu", "Viet Nam")),]$Region="East Asia and Pacific"

### Europe and Central Asia
hdi.databank.m[which(hdi.databank.m$country_name %in% c("Moldova (Republic of)","Albania","Andorra","Bulgaria","Latvia",
                                                        "Armenia","Austria","Azerbaijan","Belarus","Belgium","Bosnia and Herzegovina",
                                                        "Croatia","Cyprus","Czech Republic","Czechia","Denmark","Estonia","Finland","France",
                                                        "Georgia","Germany","Greece","Hungary","Iceland","Ireland","Italy","Kazakhstan",
                                                        "Kyrgyzstan","Liechtenstein","Lithuania","Luxembourg","Montenegro","Netherlands",
                                                        "The former Yugoslav Republic of Macedonia","Norway","Poland","Portugal","Romania",
                                                        "Russian Federation","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland",
                                                        "Tajikistan","Turkey","Turkmenistan","Ukraine","United Kingdom","Uzbekistan")),]$Region="Europe and Central Asia"

### Latin America and the Caribbean 
hdi.databank.m[which(hdi.databank.m$country_name %in% c("Antigua and Barbuda","Argentina","Bahamas","Barbados","Belize","Bolivia (Plurinational State of)",
                                                        "Brazil","Costa Rica","Chile","Colombia","Cuba","Dominica","Dominican Republic","Ecuador","El Salvador",
                                                        "Grenada","Guatemala","Guyana","Haiti","Honduras","Jamaica","Mexico","Nicaragua","Panama","Paraguay",
                                                        "Peru","Puerto Rico","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Suriname",
                                                        "Trinidad and Tobago","Uruguay","Venezuela (Bolivarian Republic of)")),]$Region="Latin America and the Caribbean"

### North America
hdi.databank.m[which(hdi.databank.m$country_name %in% c("Canada","United States")),]$Region="North America"


### Africa
hdi.databank.m[which(hdi.databank.m$country_name %in% c ("Angola","Benin","Botswana","Burkina Faso","Burundi","Cabo Verde","Cameroon","Chad","Central African Republic",
                                                         "Eswatini (Kingdom of)","CÃ´te d'Ivoire","Comoros","Congo (Democratic Republic of the)","Congo","Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia","Swaziland",
                                                         "Ghana","Papua New Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania",
                                                         "Mauritius","Mozambique","Namibia","Niger","Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles","Guinea",
                                                         "Sierra Leone","South Africa","South Sudan","Sudan","Tanzania (United Republic of)","Togo","Uganda","Zambia","Zimbabwe")),]$Region="Africa"

### South Asia 
hdi.databank.m[which(hdi.databank.m$country_name %in% c ("Afghanistan","Bangladesh","Bhutan","India","Maldives","Nepal","Pakistan","Sri Lanka","Palestine, State of")),]$Region="South Asia"

### Arab States
hdi.databank.m[which(hdi.databank.m$country_name %in% c ("Algeria","Bahrain","Djibouti","Egypt","Iran (Islamic Republic of)","Iraq","Jordan","Kuwait","Lebanon","Libya","Malta","Morocco",
                                                         "Israel","Oman","Qatar","Saudi Arabia","Syrian Arab Republic","Tunisia","United Arab Emirates","Yemen")),]$Region="Arab State"

##############################################################
#generate radar plot for health overview

heal_plot_fun=function(heal.level.in,heal.geography.in,date_from,date_to,plot_type){
  cc<-brewer.pal(4,"Set3")
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
    
    heal.mortal=heal.overview.final%>%select(c(1,5,6))%>%gather(key="mortal.index",
                                                                "Mortality.rate.infant.(per.1000.live.births)",
                                                                "Mortality.rate.under-five.(per.1000.live.births)",
                                                                value = "values")
    
    
    radar=ggradar(heal.overview.final,grid.mid = 50,grid.max = max(heal.overview.final[,2:6])+10,axis.label.size = 3,axis.label.offset = 0.5)+
      theme(legend.position = 'bottom')
    
    hiv.plot=ggplot(data=heal.overview.final,mapping = aes(x=geo,y=`HIV.prevalence.adult.(per.1000.ages.15-49)`,fill=geo))+
      geom_bar(stat="identity",width = 0.5)+
      scale_fill_manual(values = cc)+
      labs(title = "HIV Prevalence Adult (Age 15 to 49 per 1000)",
           x = ' ',
           y = ' ')+
      theme_minimal()+
      theme(plot.title = element_text(size=14,face = "bold"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position = "none")+
      geom_text(aes(label=round(`HIV.prevalence.adult.(per.1000.ages.15-49)`,3)),size=3,hjust=0,color="gray27")+
      coord_flip()
    
    expend.plot=ggplot(data = heal.overview.final,mapping = aes(x=geo,y=factor(`Current.health.expenditure.(%.of.GDP)`),fill=geo))+
      theme_minimal()+
      labs(title = "Current Health Expenditure of GDP %",
           x = ' ',
           y = ' ')+
      geom_bar(width=0.5,stat="identity")+
      scale_fill_manual(values = cc)+
      coord_polar("y")+
      guides(fill=guide_legend(nrow=1))+
      theme(plot.title = element_text(size=14,face = "bold"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank())
    
    
    life.plot=ggplot(data=heal.life)+
      theme_minimal()+
      labs(title = "Life Expectancy at Birth",
           x = ' ',
           y = ' ')+
      geom_line(aes(x=as.numeric(year),y=avg,group=geo,colour =geo),size=2)+geom_point(aes(x=as.numeric(year),y=avg,group=geo,colour =geo),size=3)+
      scale_color_manual(values=cc)+
      theme(plot.title = element_text(size=14,face = "bold"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position = "bottom",
            legend.title = element_blank())+
      guides(col=guide_legend(nrow=1,byrow = TRUE))
    
    mortal.plot=ggplot(data=heal.mortal)+
      theme_minimal()+
      labs(title = "Mortality Rate of Children",
           x = ' ',
           y = ' ')+
      geom_bar(aes(x=geo,y=values, fill=mortal.index),stat = "identity",position = "stack",width=0.5)+
      scale_fill_manual(values = cc,labels=c("Mortality Rate Infant (per 1000)","Mortality Rate Under-Five (per 1000)"))+
      guides(fill=guide_legend(nrow=1))+
      theme(plot.title = element_text(size=14,face = "bold"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position = "bottom",
            legend.title = element_blank())
    
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
    
    heal.mortal=heal.overview.final%>%select(c(1,5,6))%>%gather(key="mortal.index",
                                                                "Mortality.rate.infant.(per.1000.live.births)",
                                                                "Mortality.rate.under-five.(per.1000.live.births)",
                                                                value = "values")
    mortal.plot=ggplot(data=heal.mortal)+
      theme_minimal()+
      labs(title = "Mortality Rate of Children",
           x = ' ',
           y = ' ')+
      geom_bar(aes(x=level,y=values, fill=mortal.index),stat = "identity",position = "stack",width=0.5)+
      scale_fill_manual(values = cc,labels=c("Mortality Rate Infant per 1000","Mortality Rate Under-Five per 1000"))+
      guides(fill=guide_legend(nrow=1))+
      theme(plot.title = element_text(size=14,face = "bold"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position = "bottom",
            legend.title = element_blank())
    
    
    radar=ggradar(heal.overview.final,grid.mid = 50,grid.max = max(heal.overview.final[,2:6])+10,axis.label.size = 3,axis.label.offset = 0.5)+theme(legend.position = 'bottom')
    
    hiv.plot=ggplot(data=heal.overview.final,mapping = aes(x=level,y=`HIV.prevalence.adult.(per.1000.ages.15-49)`,fill=level))+
      geom_bar(stat="identity",width = 0.5)+
      scale_fill_manual(values = cc)+
      labs(title = "HIV Prevalence Adult (Age 15 to 49 per 1000)",
           x = ' ',
           y = ' ')+
      theme_minimal()+
      theme(plot.title = element_text(size=14,face = "bold"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position = "none")+
      geom_text(aes(label=round(`HIV.prevalence.adult.(per.1000.ages.15-49)`,3)),size=3,hjust=0,color="gray27")+
      coord_flip()
    
    expend.plot=ggplot(data = heal.overview.final,mapping = aes(level,y=factor(`Current.health.expenditure.(%.of.GDP)`),fill=level))+
      theme_minimal()+
      labs(title = "Current Health Expenditure of GDP %",
           x = ' ',
           y = ' ')+
      geom_bar(width=0.5,stat="identity")+
      scale_fill_manual(values = cc)+
      coord_polar("y")+
      guides(fill=guide_legend(nrow=1))+
      theme(plot.title = element_text(size=14,face = "bold"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank())
    
    life.plot=ggplot(data=heal.life)+
      theme_minimal()+
      labs(title = "Life Expectancy at Birth",
           x = ' ',
           y = ' ')+
      geom_line(aes(x=as.numeric(year),y=avg,group=level,colour =level),size=2)+geom_point(aes(x=as.numeric(year),y=avg,group=level,colour =level),size=3)+
      scale_color_manual(values=cc)+
      theme(plot.title = element_text(size=14,face = "bold"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position = "bottom",
            legend.title = element_blank())+
      guides(col=guide_legend(nrow=2,byrow = TRUE))
  }
  
  if(plot_type=="radar")
    return(radar)
  if(plot_type=="hiv.plot")
    return(hiv.plot)
  if(plot_type=="expend.plot")
    return(expend.plot)
  if(plot_type=="life.plot")
    return(life.plot)
  if(plot_type=="mortal.plot")
    return(mortal.plot)
  
  #return(heal.overview.final)
}

demo_plot_fun=function(demo_date_from,demo_date_to){
  map=getMap()         
  
  
  demo.data<-hdi.databank.m%>%filter(indicator_name %in% 
                                       c("Total population (millions)","Urban population (%)",
                                         "Young age (0-14) dependency ratio (per 100 people ages 15-64)",
                                         "Old-age (65 and older) dependency ratio (per 100 people ages 15-64)"))%>%
    filter((year>= demo_date_from) & (year<= demo_date_to)) %>%
    select(iso3,country_name,year,indicator_name,hdi,level,Region)%>%
    arrange(year,level)%>%drop_na(hdi)%>%
    group_by(year,indicator_name,iso3,country_name,level,Region)%>%  summarise(avg = mean(hdi))%>%
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
  pal <- colorFactor(palette = cc, domain = map$level,na.color = "#808080")
  map$labels <- paste0("<strong> Country: </strong> ", map$NAME, "<br/> ",
                       "<strong> Total population (millions): </strong> ", map$`Total.population.(millions)`, "<br/> ",
                       "<strong> Urban population (%): </strong> ", map$`Urban.population.(%)`, "<br/> ",
                       "<strong> Young age (0-14) dependency ratio (per 100 people ages 15-64): </strong> ", map$`Young.age.(0-14).dependency.ratio.(per.100.people.ages.15-64)`, "<br/> ",
                       "<strong> Old-age (65 and older) dependency ratio (per 100 people ages 15-64): </strong> ", map$`Old-age.(65.and.older).dependency.ratio.(per.100.people.ages.15-64)`, "<br/> ") %>%
    lapply(htmltools::HTML)
  
  map.plot=leaflet(map) %>% addTiles() %>% 
    setView(lng = 0, lat = 30, zoom = 2) %>%
    addPolygons(
      fillColor = ~pal(map$level),
      color = "grey",
      fillOpacity = 0.7,
      label = ~labels,
      highlight = highlightOptions(color = "black", bringToFront = TRUE)) %>%
    leaflet::addLegend(pal = pal, values = ~map$level, opacity = 0.7, title = 'Development levels')
  
  
}

# income_plot_cun=function(income_date_from,income_date_to){
#    
# }
