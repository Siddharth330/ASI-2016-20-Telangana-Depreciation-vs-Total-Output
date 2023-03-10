## Telangana state figures of depreciation and output
Telangana_total_data <- data.frame(Year=c("2016-17", "2017-18", "2018-19", "2019-20"),
                             Total_Output=c(20381796, 22240202, 26876887, 25493068),
                             Depreciation=c(668573, 767956, 890850, 829960))

## All India figures of depreciation and output
India_total_data <- data.frame(Year=c("2016-17", "2017-18", "2018-19", "2019-20"),
                               Total_Output=c(726551423, 807217258, 928179908, 898330129),
                               Depreciation=c(22213138, 23729624, 26155291, 27309742))

## industry wise figures of depreciation and output in India for 16-17 (imported .xlsx)
Country_wise_data_16_17 <- data.frame(Country_wise_data_16_17)
#library(dplyr)
Country_wise_data_16_17 <- Country_wise_data_16_17 %>% na.omit()

## industry wise figures of depreciation and output in India for 17-18 (imported .xlsx)
Country_wise_data_17_18 <- data.frame(Country_wise_data_17_18)
Country_wise_data_17_18 <- Country_wise_data_17_18 %>% na.omit()

## industry wise figures of depreciation and output in India for 18-19 (imported .xlsx)
Country_wise_data_18_19 <- data.frame(Country_wise_data_18_19)
Country_wise_data_18_19 <- Country_wise_data_18_19 %>% na.omit()

## industry wise figures of depreciation and output in India for 19-20 (imported .xlsx)
Country_wise_data_19_20 <- data.frame(Country_wise_data_19_20)

## NIC 999 refers to other industries in all tables** 
## NIC 16 refers to NIC 016 in all Telangana tables

## correlation between total output and total depreciation in Telangana
correlation_Telangana_total <- cor(Telangana_total_data$Total_Output, Telangana_total_data$Depreciation)
print(correlation_Telangana_total)

## correlation between total output and total depreciation in India
correlation_India_total <- cor(India_total_data$Total_Output, India_total_data$Depreciation)
print(correlation_India_total)

## industry wise correlation between total output and depreciation in India for 2016-17
correlation_16_17_india_industry <- cor(Country_wise_data_16_17$Total.Output, Country_wise_data_16_17$Depreciation)
print(correlation_16_17_india_industry)

## industry wise correlation between total output and depreciation in India for 2017-18
correlation_17_18_india_industry <- cor(Country_wise_data_17_18$Total.Output, Country_wise_data_17_18$Depreciation)
print(correlation_17_18_india_industry)

## industry wise correlation between total output and depreciation in India for 2018-19
Country_wise_data_18_19$Total.Output.. <- as.numeric(gsub(",", "",Country_wise_data_18_19$Total.Output..))
Country_wise_data_18_19$Depreciation <- as.numeric(gsub(",", "", Country_wise_data_18_19$Depreciation))
correlation_18_19_india_industry <- cor(Country_wise_data_18_19$Total.Output.., Country_wise_data_18_19$Depreciation)
print(correlation_18_19_india_industry)

## industry wise correlation between total output and depreciation in India for 2019-20
Country_wise_data_19_20$Total.Output <- as.numeric(gsub(",","", Country_wise_data_19_20$Total.Output))
Country_wise_data_19_20$Depreciation <- as.numeric(gsub(",","", Country_wise_data_19_20$Depreciation))
correlation_19_20_india_industry <- cor(Country_wise_data_19_20$Total.Output, Country_wise_data_19_20$Depreciation)
print(correlation_19_20_india_industry)

## total correlation for every industry in India between 2016 and 2020
#library(dplyr)
Country_wise_industry_data %>% 
  group_by(`NIC-2008`) %>% 
  summarise(cor=cor(`Total Output`, Depreciation)) 
#options(pillar.print_max=100, pillar.print_min=100)

## industry wise figures of depreciation and output in Telangana for 16-17 (imported .xlsx)
Telangana_data_16_17 <- data.frame(Telangana_data_16_17)

## correlation between total output and depreciation in Telangana for 16-17
correlation_16_17_telangana_industry <- cor(Telangana_data_16_17$Total.Output, Telangana_data_16_17$Depreciation)

## industry wise figures of depreciation and output in Telangana for 17-18 (imported .xlsx)
#Telangana_data_17_18_b <- data.frame(Telangana_data_17_18_2)

## data manipulation
#Telangana_data_17_18_2 <- Telangana_data_17_18_2[-c(1),]
colnames(Telangana_data_17_18_2)[1]<- "NIC.2008"
colnames(Telangana_data_17_18_2)[2]<- "Total_Output"
colnames(Telangana_data_17_18_2)[3]<- "Depreciation"

## correlation between total output and depreciation in Telangana for 17-18
correlation_17_18_telangana_industry <- cor(Telangana_data_17_18_2$Total_Output, Telangana_data_17_18_2$Depreciation)

## industry wise figures of depreciation and output in Telangana for 18-19 (imported .xlsx)
Telangana_data_18_19 <- data.frame(Telangana_data_18_19)

## data manipulation
#Telangana_data_18_19 <- Telangana_data_18_19[-c(1:3),-1]
colnames(Telangana_data_18_19)[1]<- "NIC.2008"
colnames(Telangana_data_18_19)[2]<- "Total_Output"
colnames(Telangana_data_18_19)[3]<- "Depreciation"

## correlation between total output and depreciation in Telangana for 18-19
correlation_18_19_telangana_industry <- cor(Telangana_data_18_19$Total_Output, Telangana_data_18_19$Depreciation)

## industry wise figures of depreciation and output in Telangana for 19-20 (imported .xlsx)
Telangana_data_19_20

## data manipulation
#Telangana_data_19_20 <- Telangana_data_19_20[-c(1,2),]
colnames(Telangana_data_19_20)[1]<- "NIC.2008"
colnames(Telangana_data_19_20)[2]<- "Total_Output"
colnames(Telangana_data_19_20)[3]<- "Depreciation"

## correlation between total output and depreciation in Telangana for 19-20
correlation_19_20_telangana_industry <- cor(Telangana_data_19_20$Total_Output, Telangana_data_19_20$Depreciation)

## combined dataset of all 4 years of output and depreciation in Telangana
Telangana_industry_data_all

## total correlation for every industry in Telangana between 2016 and 2020
library(dplyr)
Telangana_industry_data_all %>% 
  group_by(NIC.2008) %>% 
  summarise(cor=cor(Total_Output, Depreciation)) 
options(pillar.print_max=100, pillar.print_min=100)

