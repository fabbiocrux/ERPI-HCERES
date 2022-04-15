
Criteria <- tribble(
   ~Type,  ~Production,
   "Articles scientifiques", "77",
   "Articles de sythèse / revues bibliographiques", "",
   "Autres articles (articles publiés dans des revues professionnelles ou techniques, etc)", " ",
   
   "Monographies, éditions critiques, traductions", " ",
   "Direction et coordination d'ouvrages scientifiques / édition scientifique", " ",
   "Direction et coordination d'ouvrages scientifiques / édition scientifique en anglais ou dans une autre langue étrangère", " ",
   "Chapitres d'ouvrage", " ",
   "Chapitres d'ouvrages en anglais ou dans une autre langue étrangère", " ",
   "Thèse éditées", "16",
   "Editions d'actes de colloques / congrès", "",
   "Articles publiés dans des actes de colloques / congrès", "81",
   "Autres produits présentés dans des colloques / congrès et des séminaires de recherche", "",
   
   "Logiciels", "",
   "Base de données", "",
   "Outils d'aide à la désicion", "",
   "Cohortes", "",
   
   
   "Prototypes et démostrateurs", "",
   "Plateformes et observatoires", "",
   
   "Créations artistiques théorisées, mises en scènes, films", "",
   
   "Participation à des comités éditoriaux (journaux scientifiques, revues, collections, etc)", "",
   "Direction de collection et de séries", "",
   
   "Evaluation d'articles et d'ouvrages scientifiques (relecture d'articles / reviewing)", ""
)

Overview <- 
Criteria %>% 
   kbl( caption = "Indicators for ERPI 2016 - 2020") %>%
   kable_paper("hover", full_width = F) %>% 
   pack_rows("Journaux / revues", 1, 3) %>%
   pack_rows("Ouvrages", 4, 9) %>%
   pack_rows("Production dans des colloques / congrès, séminaires de recherche", 10, 12) %>%
   pack_rows("Produits et outils informatiques", 13, 16) %>%
   pack_rows("Développements instrumentaux et méthodologiques", 17, 18) %>%
   pack_rows("Autres produits propres à une discipline", 19, 19) %>%
   pack_rows("Activités éditoriales", 20, 21) %>%
   pack_rows("Activités d'évaluation", 22, 22)
