#Reading the data
Competences <- read_csv(here("Workshop", "Explanation.competences.csv"))
reactable(Competences,
          defaultColDef = colDef(
             header = function(value) gsub(".", " ", value, fixed = TRUE),
             #cell = function(value) format(value, nsmall = 1),
             align = "center",
             minWidth = 25,
             headerStyle = list(background = "#f7f7f8")
          ),
          columns = list(
             Competence = colDef(minWidth = 50),   
             Description = colDef(minWidth = 150)
          ),
          wrap = TRUE,
          filterable = TRUE, 
          bordered = TRUE, striped = TRUE, highlight = TRUE,
          compact = TRUE,
          showPageSizeOptions = TRUE, pageSizeOptions = c(1,5, 10, 50,100), defaultPageSize = 1,
          #groupBy = "Year"
)