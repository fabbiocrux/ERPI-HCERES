
Workshop <- function(){
   
# Reading data
Data=fromJSON(here("Workshop","Data.json"))


Conception=c(
   "Conception-1"="disposer d'une expertise scientifique tant générale que spécifique d'un domaine de recherche et de travail déterminé",
   "Conception-2"="faire le point sur l'état et les limites des savoirs au sein d'un secteur d'activité déterminé, aux échelles locale, nationale et internationale",
   "Conception-3"="identifier et résoudre des problèmes complexes et nouveaux impliquant une pluralité de domaines, en mobilisant les connaissances et les savoir-faire les plus avancés",
   "Conception-4"="identifier les possibilités de ruptures conceptuelles et concevoir des axes d'innovation pour un secteur professionnel",
   "Conception-5"="apporter des contributions novatrices dans le cadre d'échanges de haut niveau, et dans des contextes internationaux",
   "Conception-6"="s'adapter en permanence aux nécessités de recherche et d'innovation au sein d'un secteur professionnel")


Mise=
   c(
      "Mise-1"="mettre en œuvre les méthodes et les outils de la recherche en lien avec l'innovation",
      "Mise-2"="mettre en œuvre les principes, outils et démarches d'évaluation des coûts et de financement d'une démarche d'innovation ou de R&D",
      "Mise-3"="garantir la validité des travaux ainsi que leur déontologie et leur confidentialité en mettant en œuvre les dispositifs de contrôle adaptés",
      "Mise-4"="gérer les contraintes temporelles des activités d'études, d'innovation ou de R&D",
      "Mise-5"="mettre en œuvre les facteurs d'engagement, de gestion des risques et d'autonomie nécessaire à la finalisation d'un projet R&D, d'études ou d'innovation"
   )

Valorisation=
   c(
      
      "Valo-1"="mettre en œuvre les problématiques de transfert à des fins d'exploitation et valorisation des résultats ou des produits dans des secteurs économiques ou sociaux",
      "Valo-2"="respecter les règles de propriété intellectuelle ou industrielle liées à un secteur",
      "Valo-3"="respecter les principes de déontologie et d'éthique en relation avec l'intégrité des travaux et les impacts potentiels",
      "Valo-4"="mettre en œuvre l'ensemble des dispositifs de publication à l'échelle internationale permettant de valoriser les savoirs et connaissances nouvelles",
      "Valo-5"="mobiliser les techniques de communication de données en « open data » pour valoriser des démarches et résultats"
      
   )


Veille=
   c(
      
      "Veille-1"= "acquérir, synthétiser et analyser les données et informations scientifiques et technologiques d'avant-garde à l'échelle internationale ",
      "Veille-1.1"= "acquérir les données et informations scientifiques et technologiques d'avant-garde à l'échelle internationale ",
      "Veille-1.2"= "Synthétiser et analyser les données et informations scientifiques et technologiques d'avant-garde à l'échelle internationale ",
      "Veille-1.3"= "Analyser les données et informations scientifiques et technologiques d'avant-garde à l'échelle internationale ",
      "Veille-2"= "disposer d'une compréhension, d'un recul et d'un regard critique sur l'ensemble des informations de pointe disponibles ",
      "Veille-3"= "dépasser les frontières des données et du savoir disponibles par croisement avec différents champs de la connaissance ou autres secteurs professionnels ",
      "Veille-4"= "développer des réseaux de coopération scientifiques et professionnels à l'échelle internationale ",
      "Veille-5"= "disposer de la curiosité, de l'adaptabilité et de l'ouverture nécessaire pour se former et entretenir une culture générale et internationale de haut niveau"
   )


Formation=c(
   "Pedagogie-1"="rendre compte et communiquer en plusieurs langues des travaux à caractère scientifique et technologique en direction de publics ou publications différents, à l'écrit comme à l'oral",
   "Pedagogie-2"="enseigner et former des publics diversifiés à des concepts, outils et méthodes avancés",
   "Pedagogie-3"="s'adapter à un public varié pour communiquer et promouvoir des concepts et démarches d'avant-garde"
)


Encadrement=c(
   "Encadrement-1"= "animer et coordonner une équipe dans le cadre de tâches complexes ou interdisciplinaires",
   "Encadrement-2"= "repérer les compétences manquantes au sein d'une équipe et participer au recrutement ou à la sollicitation de prestataires",
   "Encadrement-3"= "construire les démarches nécessaires pour impulser l'esprit d'entrepreneuriat au sein d'une équipe",
   "Encadrement-4"= "identifier les ressources clés pour une équipe et préparer les évolutions en termes de formation et de développement personnel",
   "Encadrement-5"= "évaluer le travail des personnes et de l'équipe vis à vis des projets et objectifs"
)

Conception=data.frame(Conception)
Conception$Competence<- rownames(Conception)
names(Conception)=c("Description", "Competence") 

Mise=data.frame(Mise)
Mise$Competence<- rownames(Mise)
names(Mise)=c("Description", "Competence") 


Valorisation=data.frame(Valorisation)
Valorisation$Competence<- rownames(Valorisation)
names(Valorisation)=c("Description", "Competence") 


Veille=data.frame(Veille)
Veille$Competence<- rownames(Veille)
names(Veille)=c("Description", "Competence") 

Formation=data.frame(Formation)
Formation$Competence<- rownames(Formation)
names(Formation)=c("Description", "Competence") 

Encadrement=data.frame(Encadrement)
Encadrement$Competence<- rownames(Encadrement)
names(Encadrement)=c("Description", "Competence") 


Explanation.competences = rbind(Conception, Mise, Valorisation, Veille, Formation, Encadrement)
Explanation.competences=Explanation.competences%>%select(Competence,Description)

rm(Conception, Mise, Valorisation, Veille, Formation, Encadrement)



# Create the function.
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}
#getmode(x)

Data <- Data %>% rowwise() %>% mutate(`Veille-1` = replace(`Veille-1`, is.na(`Veille-1`) ,getmode( na.omit( c(`Veille-1.1`, `Veille-1.2`, `Veille-1.3`) ) )))



# Competence=c("Conception-1","Conception-2","Conception-3","Conception-4","Conception-5", "Conception-6",
#         "Mise-1","Mise-2","Mise-3","Mise-4","Mise-5", 
#         "Valo-1","Valo-2","Valo-3","Valo-4","Valo-5", 
#         "Veille-1","Veille-2","Veille-3","Veille-4","Veille-5", 
#         "Formation-1","Formation-2","Formation-3",
#         "Encadrement-1","Encadrement-2","Encadrement-3","Encadrement-4","Encadrement-5")
# 

# Competences
Competences=Data%>%select(People, Status, 'Conception-1':'Encadrement-5')    #, 'Veille-1.1':'Veille-1.3')
Competences= melt(Competences,id.vars = c("People", "Status"), variable.name = "Competence", value.name="Level")

Competences=na.omit(Competences)


# Homogenization of the levels
#Competences <- Competences %>% mutate(Level = replace(Level, str_detect(Level, '3- Competent'), c("3- Compétent")))
Competences$Level=as.factor(Competences$Level)
levels(Competences$Level)


#Explanation.competences <- read_csv(here("Workshop/Explanation.competences.csv"))
Competences <- merge(Competences,Explanation.competences, by="Competence")


# 
# Competences <- Competences %>% mutate(Level = recode(Level,
#                                              "0- Ne comprend pas" = "0",
#                                              "1- Novice" = "1",
#                                              "2- Débutant avancé" = "2",
#                                              "3- Compétent" = "3",
#                                              "4- Performant" = "4",
#                                              "5- Expert" = "5"))
# 


Competences=Competences%>%mutate(Niveau=c(1))

# Niveau
Competences <- Competences %>% mutate(Niveau = replace(Niveau, str_detect(Level, "0- Ne comprend pas"), c(0)))
Competences <- Competences %>% mutate(Niveau = replace(Niveau, str_detect(Level, "1- Novice"), c(1)))   
Competences <- Competences %>% mutate(Niveau = replace(Niveau, str_detect(Level, "2- Débutant avancé"), c(2)))   
Competences <- Competences %>% mutate(Niveau = replace(Niveau, str_detect(Level, "3- Compétent"), c(3)))   
Competences <- Competences %>% mutate(Niveau = replace(Niveau, str_detect(Level, "4- Performant"), c(4)))   
Competences <- Competences %>% mutate(Niveau = replace(Niveau, str_detect(Level, "5- Expert"), c(5)))   

Competences <- Competences %>% arrange(People) %>% select(People, Status, Competence,Description,Level,Niveau)




# Calculation for ERPI Global 
ERPI.Global=Competences%>%group_by(Competence)%>%
   summarise(Niveau=mean(Niveau)) %>% 
   mutate(People=c("ERPI Global")) %>% 
   mutate(Level=c(' '), Status=c(' '))

ERPI.Global=merge(ERPI.Global,Explanation.competences, by="Competence")

ERPI.Global=ERPI.Global%>%select(People, Status,Competence, Description, Level, Niveau)
names(ERPI.Global)


# ERPI Doctorants
ERPI.Doctorants=Competences%>%filter(Status=="Phd Student")%>%
   group_by(Competence)%>%
   summarise(Niveau=mean(Niveau)) %>% 
   mutate(People=c("ERPI PhD Students")) %>% 
   mutate(Level=c(' '), Status=c('ERPI PhD Students'))
ERPI.Doctorants=merge(ERPI.Doctorants,Explanation.competences, by="Competence")


# ERPI IE
ERPI.IR=Competences%>%filter(Status=="IR")%>%
   group_by(Competence)%>%
   summarise(Niveau=mean(Niveau)) %>% 
   mutate(People=c("ERPI Ingénieur Recherche")) %>% 
   mutate(Level=c(' '), Status=c('Ingénieure Recherche'))
ERPI.IR=merge(ERPI.IR,Explanation.competences, by="Competence")


# ERPI MdC
ERPI.Mcd=Competences%>%filter(Status=="MdC")%>%
   group_by(Competence)%>%
   summarise(Niveau=mean(Niveau)) %>% 
   mutate(People=c("ERPI MdC")) %>% 
   mutate(Level=c(' '), Status=c('ERPI MdC'))
ERPI.Mcd=merge(ERPI.Mcd, Explanation.competences, by="Competence")

# ERPI Prof
ERPI.Prof = Competences%>%filter(Status=="Prof")%>%
   group_by(Competence)%>%
   summarise(Niveau=mean(Niveau)) %>% 
   mutate(People=c("ERPI Prof")) %>% 
   mutate(Level=c(' '), Status=c('ERPI Prof'))
ERPI.Prof=merge(ERPI.Prof, Explanation.competences, by="Competence")




# Competences,

# Grafica
test=rbind(ERPI.Global, ERPI.Doctorants, ERPI.IR, ERPI.Mcd, ERPI.Prof )
test=test%>%group_by(People)%>%mutate(degree =seq(from=0, to=360-(360/29), length.out= length(levels(Competences$Competence))))
#test=test%>%select(People,'Competence':'Niveau')
test=test%>%arrange(People)



test$o <- test$Niveau * sin(test$degree * pi / 180) # SOH
test$a <- test$Niveau * cos(test$degree * pi / 180) # CAH
test$X.exterior <- 5.8 * sin(test$degree * pi / 180) # Outer ring x
test$Y.exterior <- 5.8 * cos(test$degree * pi / 180) # Outer ring y 

test$X10 <- 1 * sin(test$degree * pi / 180) # 10% ring x
test$Y10 <- 1 * cos(test$degree * pi / 180) # 10% ring y
test$X20 <- 2 * sin(test$degree * pi / 180) # 20% ring x
test$Y20 <- 2 * cos(test$degree * pi / 180) # 20% ring y
test$X30 <- 3 * sin(test$degree * pi / 180) # 30% ring x
test$Y30 <- 3 * cos(test$degree * pi / 180) # 30% ring y
test$X40 <- 4 * sin(test$degree * pi / 180) # 40% ring x
test$Y40 <- 4 * cos(test$degree * pi / 180) # 40% ring y
test$X50 <- 5 * sin(test$degree * pi / 180) # 50% ring x
test$Y50 <- 5 * cos(test$degree * pi / 180) # 50% ring y




p = plot_ly()

for(i in 1:29) {
   p <- add_trace(
      p, 
      x = c(test$X.exterior[i],0), 
      y = c(test$Y.exterior[i],0), 
      evaluate = TRUE,
      line = list(color = "#d3d3d3", dash = "2px"),
      showlegend = FALSE,
      hoverinfo = "text",
      text = test$Description[i],
      hoverlabel=list(font =list(family="Times New Roman"))
   )
}


# Conecting the lines
line.final = test %>% filter(Competence=="Conception-1")
test=rbind(test,line.final)
rm(line.final)





Profile <- 
   
   p %>% 
   add_trace(x = test$X.exterior, y = test$Y.exterior, 
             text = test$Competence,
             hoverinfo = "none",
             textposition = "top middle", mode = "lines+text", 
             textfont = list(family= "Helvetica" ,color = '#8e8e8e', size = 13) ,
             line = list(color = "#d3d3d3", dash = "2px", shape = "spline"),
             showlegend = FALSE) %>% 
   
   add_trace(x = rep(0,6), y = c(0:5),
             text = c("", "Novice", "Débutant avancé", "Compétant", "Performant", "Expert"),
             textposition = "top middle", mode = "text",
             textfont = list(family= "Helvetica" ,color = '#8e8e8e', size = 13),
             showlegend = FALSE)   %>%
   add_trace(x = test$o, y = test$a, color = test$People, 
             mode = "lines+markers",
             hoverinfo = "text", 
             fill = 'toself',
             visible = "legendonly",
             text = paste(test$People, test$Level,  test$Description, sep="\n")) %>%  
   
   add_trace(x = test$X10, y = test$Y10, mode = "lines", 
             line = list(color = "#d3d3d3", dash = "1px", shape = "spline"), 
             hoverinfo = "none",
             showlegend = FALSE) %>%
   
   add_trace(x = test$X20, y = test$Y20, mode = "lines", 
             line = list(color = "#d3d3d3", dash = "1px", shape = "spline"), 
             hoverinfo = "none",
             showlegend = FALSE) %>%
   
   add_trace(x = test$X30, y = test$Y30, mode = "lines", 
             line = list(color = "#d3d3d3", dash = "1px", shape = "spline"), 
             hoverinfo = "none",
             showlegend = FALSE) %>%
   
   add_trace(x = test$X40, y = test$Y40, mode = "lines", 
             line = list(color = "#d3d3d3", dash = "1px", shape = "spline"), 
             hoverinfo = "none",
             showlegend = FALSE) %>%
   
   add_trace(x = test$X50, y = test$Y50, mode = "lines", 
             line = list(color = "#d3d3d3", dash = "3px", shape = "spline"), 
             hoverinfo = "none",
             showlegend = FALSE) %>%
   layout(
      legend = list(orientation = 'h'),
      autosize = FALSE,
      hovermode = "closest",     
      width = 500,
      height = 500,
      autosize = TRUE,
      autoscale = TRUE,
      xaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
      yaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE)
   )

results <- list(Profile, Explanation.competences)
return(Profile)
}