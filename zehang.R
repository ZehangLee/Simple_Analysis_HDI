#readxl::read_xlsx("http://hdr.undp.org/sites/default/files/2018_statistical_annex_all.xlsx",sheet=1)

temp1 <- tempfile()
download.file("http://hdr.undp.org/sites/default/files/2018_all_indicators.xlsx",temp1, mode="wb")
hdi.databank <- readxl::read_xlsx(temp1,sheet = 1)
unlink(temp1)

temp2 <- tempfile()
download.file("http://hdr.undp.org/sites/default/files/2018_statistical_annex_all.xlsx",temp2, mode="wb")
hdi.stat <- readxl::read_xlsx(temp2,sheet = 1,range = "A3:M198")
unlink(temp2)
rm(temp1,temp2)

library(tidyverse)


year=as.character(seq(1990,2017))
hdi.databank=hdi.databank%>%select(-"9999")%>%gather(one_of(year),key = "year",value = "hdi")


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

test=hdi.databank%>%mutate(level=hdi.stat.m$level[match(hdi.databank$country_name,hdi.stat.m$Country)])
