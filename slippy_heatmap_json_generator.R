# This script generates the json for an unused slippy heatmap
# http://projects.ctmirror.org/content/trend/2016/08/towed/index.html

library(dplyr)

tab <- read.csv("clean-qcew.csv", stringsAsFactors=F)
tab <- tab %>%
  arrange(Ind)

tab2 <- tab
measures <- unique(tab2$Ind)
#races <- unique(tab2$Group)

the_js <- "var data= {
"

for (i in 1:length(measures)) {
  print(i)
  
  type <- measures[i]
  sub1 <- subset(tab2, Ind==type)
  
  the_js <- paste0(the_js, "\"", type, "\": [
                   ")
    the_js <- paste0(the_js, "{
                     \"name\": \"employees\", 
                     \"data\": [", sub1$NumEmployees[1], ", ",
                     sub1$NumEmployees[2], ", ",
                     sub1$NumEmployees[3], ", ",
                     sub1$NumEmployees[4], ", ",
                     sub1$NumEmployees[5], ", ",
                     sub1$NumEmployees[6], ", ",
                     sub1$NumEmployees[7], ", ",
                     sub1$NumEmployees[8], ", ",
                     sub1$NumEmployees[9], ", ",
                     sub1$NumEmployees[10], ", ",
                     sub1$NumEmployees[11], ", ",
                     sub1$NumEmployees[12], ", ",
                     sub1$NumEmployees[13], ", ",
                     sub1$NumEmployees[14], ", ",
                     sub1$NumEmployees[15], ", ",
                     sub1$NumEmployees[16], "]
  },")
    

  
  the_js <- substr(the_js, 1, nchar(the_js)-1)
  the_js <- paste0(the_js, "],")
  }

the_js <- substr(the_js, 1, nchar(the_js)-1)
the_js <- paste0(the_js, "];")

write(the_js, "grouped_js3.js")