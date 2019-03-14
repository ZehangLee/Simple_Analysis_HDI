library(openxlsx)
library(tidyverse)

hdi.databank=read.xlsx("http://hdr.undp.org/sites/default/files/2018_all_indicators.xlsx")

temp2 <- tempfile()
download.file("http://hdr.undp.org/sites/default/files/2018_statistical_annex_all.xlsx",temp2, mode="wb")
hdi.stat <- readxl::read_xlsx(temp2,sheet = 1,range = "A3:M198")
unlink(temp2)
rm(temp2)

###tidy table hdi.databank
year=as.character(seq(1990,2017))
hdi.databank=hdi.databank%>%select(-"9999")%>%gather(one_of(year),key = "year",value = "hdi")



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
                                     "Eswatini (Kingdom of)","Côte d'Ivoire","Comoros","Congo (Democratic Republic of the)","Congo","Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia","Swaziland",
                                     "Ghana","Papua New Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania",
                                     "Mauritius","Mozambique","Namibia","Niger","Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles","Guinea",
                                     "Sierra Leone","South Africa","South Sudan","Sudan","Tanzania (United Republic of)","Togo","Uganda","Zambia","Zimbabwe")),]$Region="Africa"

### South Asia 
hdi.databank.m[which(hdi.databank.m$country_name %in% c ("Afghanistan","Bangladesh","Bhutan","India","Maldives","Nepal","Pakistan","Sri Lanka","Palestine, State of")),]$Region="South Asia"

### Arab States
hdi.databank.m[which(hdi.databank.m$country_name %in% c ("Algeria","Bahrain","Djibouti","Egypt","Iran (Islamic Republic of)","Iraq","Jordan","Kuwait","Lebanon","Libya","Malta","Morocco",
                                     "Israel","Oman","Qatar","Saudi Arabia","Syrian Arab Republic","Tunisia","United Arab Emirates","Yemen")),]$Region="Arab State"
View(hdi.databank.m)
