# Data HAL ----
HAL <- 
   read_csv("HAL/HAL-August-2022.csv") %>% 
   set_names("Url", "Title", "Authors", "Type.document", "DOI", "Year", "Journal", "Publisher", "Conference") 

## Filtering 2016 and tolower
HAL <- HAL %>% filter(Year >= 2011, Year<=2022)
HAL$Journal = tolower(HAL$Journal)

#HAL %>% group_by(Type.document) %>% tally()

# Exporting the data
#write.csv2(HAL, "HAL/HAL-2016-2020.csv")
#write_csv2(HAL, "HAL/HAL-2016-2020.csv")
#write_excel_csv2(HAL, "HAL/HAL-2016-2020-excel.csv")


# Data Scopus ----
Scopus <- read_csv2("Scopus/Scopus-2019.csv") 
names(Scopus)[2]=c("Journal")
#Scopus = Scopus %>%  rename(Journal = Title)
Scopus$Journal = tolower(Scopus$Journal)
Journal_scopus <- factor(Scopus$Journal) %>% unique()

#HAL <- HAL %>% left_join(Scopus, by= "Journal")

HAL <- 
   HAL %>%
   mutate(
      Scopus = case_when(
         HAL$Journal %in% Journal_scopus ~ "Oui",
         TRUE                      ~ "Non"
      ))



# Installing Scimago Data Science
#devtools::install_github("ikashnitsky/sjrdata")

# load
# library(sjrdata)
# data.scimago <- sjr_journals
# data.scimago$year <- as.numeric(data.scimago$year)
# names(data.scimago)
# #filtering 2019
# data.scimago <- data.scimago %>% filter(year >= 2019)
# 
# data.scimago <- 
#    data.scimago %>% 
#    select(title, sjr_best_quartile, h_index, categories) %>% 
#    set_names("Journal", "Quartile-Scimago-Engineering", "H.Factor", "Categories")
# Engineering.Scimago$Journal = tolower(Engineering.Scimago$Journal)



# Scimago ----
## Engineering
Scimago <- read.csv2("HAL/Scimago/scimagojr_2020.csv") %>%  select(Title, SJR.Best.Quartile, H.index, Categories)
Scimago <-  Scimago %>% set_names("Journal", "QScimago", "H.Factor", "Categories")
Scimago$Journal = tolower(Scimago$Journal)



## Decision Science
#DS.Scimago <- read.csv2("HAL/Scimago/Decision-sciences.csv") %>%  select(Title, SJR.Best.Quartile, H.index)
#DS.Scimago <-  DS.Scimago %>% set_names("Journal", "Scimago<br/>Decision<br/>Sciences", "H.Factor")
#DS.Scimago$Journal = tolower(DS.Scimago$Journal)


# Exporting
ERPI <- left_join(HAL, Scimago, by="Journal" )
#ERPI <- left_join(ERPI, DS.Scimago, by="Journal" )

rm(HAL, Scimago, Scopus, Journal_scopus)


