library("readxl")
library(lubridate)

onglet <- excel_sheets("Mauricio/Annexe-5.xlsx")
Edades <- read_excel("Mauricio/Annexe-5.xlsx",
                     sheet = onglet[4], skip = 32,
                     col_names = c("Nom", "Prénom", "Sexe", "Corps-grade", "Type_emploi",
                                   "Axe", "Date_naissance", "BAP", "HDR", "Employeur",
                                   "Code_UAI", "Date_in", "Date_out"), 
                     col_types = c( rep("text",6), "date", 
                                    rep("text",4), "date", "date" 
                     ))
# Filtering the data
Edades <- Edades[1:44,]
Edades$today <- Sys.time()

Edades$ages = interval(start = Edades$Date_naissance, end = Edades$today) /
   duration(num = 1, units = "years") %>% round(., digits = 0)

Edades$rango <- ordered( cut(Edades$ages, breaks = c(seq(20,70,10)), 
                             include.lowest = TRUE))
# Deleting personnes 
Excluidos <- Edades %>%
   filter(Prénom %in% c('NOÉMIE', 'CEDRIC', 'GREGORY', "ALEXIS", "STÉPHANE", "PEDRO", "CHRISTIAN", "DANIEL ANDRES", "MARIE REINE", "PATRICK")) 

Membres <- Edades %>% anti_join(Excluidos)


