# Impact Factor
# library(JCRImpactFactor)
# help(JCRImpactFactor)
#library(readxl)   
# library(purrr)   

# Rev2016 <-
#    ERPI %>%
#    filter(Type.document == "ART" & Year==2016) %>%
#    select(Journal) %>%
#    unique() %>%
#    map_df( ~ find.IF.JCR(., year=2016))

# IFJournals <- function(anne, ERPI= ERPI ) {
#    # Filtering the Dataframe
#    Rev <-
#       ERPI %>%
#       filter(Type.document == "ART" & Year==anne) %>%
#       select(Journal) %>%
#       unique() %>%
#       map_df( ~ find.IF.JCR(., year=anne))
#    return(Rev)
# }

# Test2016 <- IFJournals(2016, ERPI) 
# Test2017 <- IFJournals(2017, ERPI)
# Test2018 <- IFJournals(2018, ERPI)
# Test2019 <- IFJournals(2019, ERPI)
# Test2020 <- IFJournals(2020, ERPI)
# Test2021 <- IFJournals(2021, ERPI)
#Test2022 <- IFJournals(2021, ERPI)


# Impact Factor
load("JCR/JCR_data.rda")
JCR_2016_2019 <- DataDupN
names(JCR_2016_2019)[1] <- "Journal"
JCR_2016_2019$Journal <- tolower(JCR_2016_2019$Journal)


## JCR 2020
JCR2020 <- 
   #read_csv("JCR/JCR.csv", skip = 1) %>%
   #select("Full Journal Title", "Journal Impact Factor") %>% 
   read_csv("JCR/jcr_2020_wos.csv") %>%
   select("journal_name", "impact_factor_2020") %>% 
   set_names("Journal", "IF2020")
JCR2020$Journal <- tolower(JCR2020$Journal)

## JCR 2021
JCR2021 <- 
   read_csv("JCR/jcr_2021_wos.csv") %>% 
   select("journal_name", "impact_factor_2021") %>% unique() %>% 
   set_names("Journal", "IF2021")
JCR2021$Journal <- tolower(JCR2021$Journal)   


## JCR 2021
JCR2022 <- 
   read_excel("JCR/JCR2022_Released in 2023.xlsx") %>% 
   select("Journal name", "2022 JIF") %>% unique() %>% 
   set_names("Journal", "IF2022")
JCR2022$Journal <- tolower(JCR2022$Journal)   

# Joining database
JCR <- 
   JCR_2016_2019 %>% 
   left_join(JCR2020, by = "Journal") %>% 
   left_join(JCR2021, by = "Journal") %>% 
   left_join(JCR2022, by = "Journal")

# Adding manually the missing IF 2020

JCR$IF2022 <- as.numeric(JCR$IF2022)

JCR <- 
   JCR %>% 
   mutate(IF2022 = case_when(
      Journal == "environmental impact assessment review" ~ 7.9,
      Journal =="technological forecasting and social change" ~ 12,
      Journal =="creativity and innovation management" ~ 3.5,
      Journal =="international review of administrative sciences" ~ 2.3,
      Journal =="industrial marketing management" ~ 10.3,
      Journal =="ecological indicators" ~ 6.9,
      Journal =="thinking skills and creativity" ~ 3.7,
      Journal =="research policy" ~ 7.2,
      TRUE ~ IF2022
   ))


## Unique articles ERPI
JCR_ERPI <- 
   ERPI %>% filter(Type.document == "ART") %>% select(Journal) %>% unique() %>% 
   # CHanging Journal names to JCR format
   mutate(Journal = case_when(
      Journal == "renewable and sustainable energy reviews" ~ "renewable & sustainable energy reviews",
      Journal == "resources, conservation and recycling" ~ "resources conservation and recycling",
      Journal == "chemical engineering research and design" ~ "chemical engineering research & design",
      TRUE ~ Journal
   )) %>% 
   left_join(JCR, by = "Journal") # Joining the JCR data



## Selectiing articles ERPI
Articles <- 
   ERPI %>% filter(Type.document == "ART") %>% 
   # CHanging Journal names to JCR format
   mutate(Journal = case_when(
      Journal == "renewable and sustainable energy reviews" ~ "renewable & sustainable energy reviews",
      Journal == "resources, conservation and recycling" ~ "resources conservation and recycling",
      Journal == "chemical engineering research and design" ~ "chemical engineering research & design",
      TRUE ~ Journal
   )) %>% 
   arrange(desc(Year)) %>% 
   select(Year, Title, Journal, QScimago) %>% 
   left_join(JCR, by = "Journal") # Joining the JCR data
#names(ERPI)




# Creating the impact factor global à utiliser dans les moyenne
Articles <- 
   Articles %>% 
   mutate(Impact.Factor = case_when(
      Year == 2023 ~ IF2022,
      Year == 2022 ~ IF2022,
      Year == 2021 ~ IF2021,
      Year == 2020 ~ IF2020,
      Year == 2019 ~ IF2019,
      Year == 2018 ~ IF2018,
      Year == 2017 ~ IF2017,
      Year == 2016 ~ IF2016,
      Year == 2015 ~ IF2015,
      Year == 2014 ~ IF2014,
      Year == 2013 ~ IF2013,
      Year == 2012 ~ IF2012,
      Year == 2011 ~ IF2011,
      TRUE ~  0
   ))

# Identifying the with or not FI
Articles <- 
   Articles %>% 
   mutate(IF = case_when(
      is.na(Impact.Factor) ~ "No FI",
      TRUE ~  "Avec FI"
   ))

# Chaging the factors
Articles$IF <- factor(Articles$IF,
                      levels = c("No FI", "Avec FI"))


rm(JCR_2016_2019,JCR2020, JCR2021, JCR2022, DataDupN, JCR)


## Export

write.csv(Articles, file = "HAL/Articles-ERPI-January-2024.csv")




