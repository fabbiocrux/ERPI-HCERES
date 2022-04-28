library(rscopus)

rscopus::get_api_key()
options()


set_api_key('1c2a510107201a33ade9442e73b81545')
api_key = Sys.getenv('Elsevier_API')

library(tidyverse)
fabio = author_df(last_name = "Cruz", first_name = "Fabio",
                verbose = FALSE, general = FALSE)
names(res)

rscopus::?get_api_key()

res = get_api_key(error = FALSE)

print(res, reveal = TRUE)

options("elsevier_api_key" = '1c2a510107201a33ade9442e73b81545')