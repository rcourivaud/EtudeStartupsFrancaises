library(xml2)
library(rvest)

url <- 'https://fr.wikipedia.org/wiki/Liste_des_d%C3%A9partements_fran%C3%A7ais_class%C3%A9s_par_population_et_superficie'
test <- read_html(url, encoding = "UTF-8")

y <- read_html("Density.html",encoding = "UTF-8") %>%
  html_table(fill=TRUE, header=TRUE)

class(y)
