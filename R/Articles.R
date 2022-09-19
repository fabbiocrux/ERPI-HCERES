# Impact Factor
# library(JCRImpactFactor)
# library(purrr)   
# 
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


# Impact Factor
load("JCR/JCR_data.rda")
JCR_2016_2019 <- DataDupN
names(JCR_2016_2019)[1] <- "Journal"

JCR2020 <- 
   read_csv("JCR/JCR.csv", skip = 1) %>%
   select("Full Journal Title", "Journal Impact Factor") %>% 
   set_names("Journal", "IF2020")

# Joining database
JCR <- 
   JCR_2016_2019 %>% 
   left_join(JCR2020, by = "Journal")

# tolower the title
JCR$Journal = tolower(JCR$Journal)


# Selectiing articles
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
   select(Year, Title, Journal) %>% 
   left_join(JCR, by = "Journal") # Joining the JCR data
#names(ERPI)




# Adding manually the missing IF 2020
Articles <- 
   Articles %>% 
   mutate(IF2020 = case_when(
      Journal == "mathematics" ~ 2.258,
      Journal =="forest policy and economics" ~ 3.673,
      Journal =="geoderma" ~ 6.114,
      Journal =="international journal of disaster risk reduction" ~ 4.320,
      Journal =="technological forecasting and social change" ~ 8.593,
      Journal =="creativity and innovation management" ~ 3.051,
      Journal =="international review of administrative sciences" ~ 3.094,
      Journal =="industrial marketing management" ~ 6.960,
      Journal =="ecological indicators" ~ 6.960,
      Journal =="thinking skills and creativity " ~ 3.106,
      TRUE ~ IF2020
   ))


# Creating the impact factor global à utiliser dans les moyenne
Articles <- 
   Articles %>% 
   mutate(Impact.Factor = case_when(
      Year == 2022 ~ IF2020,
      Year == 2021 ~ IF2020,
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


rm(DataDupN, JCR, JCR_2016_2019, JCR2020)
