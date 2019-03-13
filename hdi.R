
library(openxlsx)

data<-read.xlsx("http://hdr.undp.org/sites/default/files/2018_all_indicators.xlsx")

data$Region=rep(".",nrow(data))

### East Asia and Pacific
data[which(data$country_name %in% c("Australia","Brunei Darussalam","Cambodia","China","Fiji",
                                    "Hong Kong, China (SAR)","Indonesia","Japan","Kiribati",
                                    "Korea (Republic of)", "Lao People's Democratic Republic",
                                    "Malaysia","Micronesia (Federated States of)","Mongolia",
                                    "Samoa","Singapore","Solomon Islands", "Thailand", "Timor-Leste",
                                    "Tonga","Vanuatu", "Viet Nam")),]$Region="East Asia and Pacific"

### Europe and Central Asia
data[which(data$country_name %in% c("Moldova (Republic of)","Albania","Andorra","Bulgaria","Latvia",
                                    "Armenia","Austria","Azerbaijan","Belarus","Belgium","Bosnia and Herzegovina",
                                    "Croatia","Cyprus","Czech Republic","Denmark","Estonia","Finland","France",
                                    "Georgia","Germany","Greece","Hungary","Iceland","Ireland","Italy","Kazakhstan",
                                    "Kyrgyzstan","Liechtenstein","Lithuania","Luxembourg","Montenegro","Netherlands",
                                    "The former Yugoslav Republic of Macedonia","Norway","Poland","Portugal","Romania",
                                    "Russian Federation","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland",
                                    "Tajikistan","Turkey","Turkmenistan","Ukraine","United Kingdom","Uzbekistan")),]$Region="Europe and Central Asia"

### Latin America and the Caribbean 
data[which(data$country_name %in% c("Antigua and Barbuda","Argentina","Bahamas","Barbados","Belize","Bolivia (Plurinational State of)",
                                    "Brazil","Costa Rica","Chile","Colombia","Cuba","Dominica","Dominican Republic","Ecuador","El Salvador",
                                    "Grenada","Guatemala","Guyana","Haiti","Honduras","Jamaica","Mexico","Nicaragua","Panama","Paraguay",
                                    "Peru","Puerto Rico","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Suriname",
                                    "Trinidad and Tobago","Uruguay","Venezuela (Bolivarian Republic of)")),]$Region="Latin America and the Caribbean"


### Sub-Saharan Africa
data[which(data$country_name %in% c ("Angola","Benin","Botswana","Burkina Faso","Burundi","Cabo Verde","Cameroon","Chad","Central African Republic",
                                     "Comoros","Congo (Democratic Republic of the)","Congo","Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia","Swaziland",
                                     "Ghana","Papua New Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania",
                                     "Mauritius","Mozambique","Namibia","Niger","Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles","Guinea",
                                     "Sierra Leone","South Africa","South Sudan","Sudan","Tanzania (United Republic of)","Togo","Uganda","Zambia","Zimbabwe")),]$Region="Sub-Saharan Africa"

### South Asia 
data[which(data$country_name %in% c ("Afghanistan","Bangladesh","Bhutan","India","Maldives","Nepal","Pakistan","Sri Lanka","Palestine, State of")),]$Region="South Asia"

### Arab States
data[which(data$country_name %in% c ("Algeria","Bahrain","Djibouti","Egypt","Iran (Islamic Republic of)","Iraq","Jordan","Kuwait","Lebanon","Libya","Malta","Morocco",
                                     "Israel","Oman","Qatar","Saudi Arabia","Syrian Arab Republic","Tunisia","United Arab Emirates","Yemen")),]$Region="Arab State"
View(data)
