library("readxl")
library(lubridate)

#onglet <- excel_sheets("Mauricio/Annexe-5.xlsx")
# Edades <- read_excel("ERPI/Annexe 2-Donnees_caracterisation_Vague-C.xlsm",
#                      sheet = onglet[4], skip = 21,
#                      col_names = c("Nom", "Prénom", "Sexe", "Corps-grade", "Type_emploi",
#                                    "Axe", "Date_naissance", "BAP", "HDR", "Employeur",
#                                    "Code_UAI", "Date_in", "Date_out"), 
#                      col_types = c( rep("text",6), "date", 
#                                     rep("text",4), "date", "date" 
#                      ))

onglet <- excel_sheets("ERPI/Annexe 2-complet-VALIDE.xlsm")
Edades <- 
   read_excel("ERPI/Annexe 2-complet-VALIDE.xlsm",
              sheet = onglet[4], skip = 19,
              col_types = c( rep("text",6), "date", 
                             rep("text",8), "date", 
                             rep("text",3), "date", "text"))
# Filtering the data
Edades <- Edades %>% slice(1:67)

names(Edades) <- c("Code_HCERES",  "Nom", "Prenom", "Sexe", "Fonction", "Equipe_int", 
                 "Date_naissance", "Employeur","Etablissement","Emploi",
                 "Corps_grade", "CNU", "Code_postal" ,"Panel", "HDR", 
                 "Date_in", "Corps_Grade_avant", "Precedente unite",
                 "Pays", "Date_out",
                 "ORCID")


Edades <- 
   Edades %>% 
   mutate(Date_in = as.Date(Date_in, format = "%d.%m.%Y"),
          Date_out = as.Date(Date_out, format = "%d.%m.%Y"))

names(Edades)

# PUttin the date of 31 december
Edades$today <- as_datetime("2021-12-31")

Edades$ages = interval(start = Edades$Date_naissance, end = Edades$today) /
   duration(num = 1, units = "years") %>% round(., digits = 0)

Edades$rango <- ordered( cut(Edades$ages, breaks = c(seq(20,70,10)), 
                             include.lowest = TRUE))


# Exlucing Persones that were out
Members <- Edades %>% filter(is.na(Date_out)) 
          
# Deleting personnes 
Excluidos <- Members %>%
   filter(Prenom %in% 
             c('CEDRIC', 
               'FREDERICK',
               'CHRISTIAN',
               'JEROME',
               "JONATHAN", 
               "VLADIMIR AICARDO", 
               "ALEXIS", 
               "CHRISTIAN" ,
               #"DANIEL ANDRES",
               "MARTINE",
               "PASCALE"
               )) 

Members <- Members %>% anti_join(Excluidos)

Excluidos %>% select(Nom, Prenom)
rm(Edades, Excluidos)

