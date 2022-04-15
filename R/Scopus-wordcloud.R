library(wordcloud2)

# Data Scopus ----
Scopus <- read_csv2("Scopus/Scopus-2019.csv") 
names(Scopus)[2]=c("Journal")
#Scopus = Scopus %>%  rename(Journal = Title)
Scopus$Journal = tolower(Scopus$Journal)
Journal_scopus <- factor(Scopus$Journal) %>% unique()


Domaines <- 
   ERPI %>% 
   filter(Type.document == "ART") %>% 
   filter(Scopus == "Oui") %>% 
   left_join(Scopus, by= "Journal") %>% 
   select("Scopus Sub-Subject Area") %>% 
   group_by(`Scopus Sub-Subject Area`) %>% 
   tally(sort = TRUE, name = "Frequency") 

Scopus.wordcloud = wordcloud2(data = Domaines,  shape = 'circle', size=0.2 ) 

rm(Scopus)
