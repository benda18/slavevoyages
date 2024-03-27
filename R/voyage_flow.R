#voyage flow

library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(haven)
library(purrr)

#https://stackoverflow.com/questions/57552015/how-to-extract-column-variable-attributes-labels-from-r-to-csv-or-excel

rm(list=ls());cat('\f')
gc()

## wd----
# wd <- {list(home   = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade", 
#             data   = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/data", 
#             output = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/output",
#             R      = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/R", 
#             shiny  = NA, 
#             cw     = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_intraamerican_slave_trade/crosswalks")
# }

#setwd(wd$R)

# load data----
#setwd(wd$data)

setwd(dir = "data/")
getwd()

load(list.files( pattern = "\\.RData$"))

setwd('..')
getwd()





# journey general flow----

library(igraph)

graph_from_literal(start --+ purchasing --+ middle_passage --+ landing --+ 
                     selling --+ returning --+ end)

top_lvl <- c("Home\nPort", "departure", "purchasing", "middle_passage",  
  "selling", "returning", "end", "Home\nPort")

gr.top_lvl <- NULL
for(i in 2:length(top_lvl)){
  gr.top_lvl <- rbind(gr.top_lvl, 
                  data.frame(from = top_lvl[i-1], 
                             to   = top_lvl[i]))
}

graph_from_data_frame(gr.top_lvl) %>% plot()


cw_master.col.labels[2] %>% as_tibble()

# explore----

iastsav <- iastsav %>% haven::as_factor()
nastsav <- nastsav %>% haven::as_factor() %>% janitor::clean_names()




colnames(nastsav) %>% grep("1imp$", ., value = T)  # at first port of purchase
colnames(nastsav) %>% grep("2imp$", ., value = T)  # middle passage
colnames(nastsav) %>% grep("3imp$", ., value = T)  # first port of landing




data.frame(graph_node = "purchasing", 
           colname    = c("d1slatra", "d1slatrb", "d1slatrc"))
nastsav$d1slatra  # slave purchase began
nastsav$d1slatrb  #...
nastsav$d1slatrc  #...

data.frame(graph_node = "selling", 
           colname    = c("datarr32", "datarr33", "datarr34", 
                          "datarr36", "datarr37", "datarr38", 
                          "datarr39", "datarr40", "datarr41"))
nastsav$datarr32  # slave disembarkation began
nastsav$datarr33  #...
nastsav$datarr34  #...
nastsav$datarr36  # date of 2nd landing (date)
nastsav$datarr37  #...
nastsav$datarr38  #...
nastsav$datarr39  # date of 3rd landing
nastsav$datarr40  #...
nastsav$datarr41  #...

data.frame(graph_node = "end", 
           colname    = c("datarr43", "datarr44", "datarr45"))
nastsav$datarr43  # date voyage completed
nastsav$datarr44  #...
nastsav$datarr45  #...

data.frame(graph_node = "unknown", 
           colname    = c("datebuy", "datedep", "datedepam", "dateend"))
nastsav$datebuy   #???
nastsav$datedep   #???
nastsav$datedepam #???
nastsav$dateend   #...

data.frame(graph_node = "Home\nPort", 
           colname    = c("datedepa", "datedepb", "datedepc"))
nastsav$datedepa  # day voyage began
nastsav$datedepb  #...
nastsav$datedepc  #...

nastsav$dateland1 #???
nastsav$dateland2 #...
nastsav$dateland3 #...

nastsav$dateleftafr #???

nastsav$ddepam  # day departure from last place of landing
nastsav$ddepamb #...
nastsav$ddepamc #...

nastsav$datedep #???

nastsav$datedepa  # day voyage began
nastsav$datedepb  #...
nastsav$datedepc  #...

nastsav$deptregimp  # Imputed region where voyage began
nastsav$deptregimp1 # Imputed broad region where voyage began

nastsav$dlslatra # day vessel left last slaving port
nastsav$dlslatrb #...
nastsav$dlslatrc #...


nastsav$adlt1imp
nastsav$adlt2imp
nastsav$adlt3imp

nastsav$adpsale1
nastsav$adpsale2

nastsav$adult1
nastsav$adult2
nastsav$adult3
nastsav$adult4
nastsav$adult5
nastsav$adult6
nastsav$adult7

nastsav$arrport
nastsav$arrport2

nastsav$boy1
nastsav$boy2
nastsav$boy3
nastsav$boy4
nastsav$boy5
nastsav$boy6
nastsav$boy7

nastsav$boyrat1
nastsav$boyrat3
nastsav$boyrat7

nastsav$chil1imp
nastsav$chil2imp
nastsav$chil3imp

nastsav$child1
nastsav$child2
nastsav$child3
nastsav$child4
nastsav$child5
nastsav$child6
nastsav$child7
nastsav$chilrat1
nastsav$chilrat3
nastsav$chilrat7

nastsav$constreg

nastsav$crew
nastsav$crew1
#...
nastsav$crew5
nastsav$crewdied



nastsav$d1slatra
nastsav$d1slatrb
nastsav$d1slatrc

nastsav$datarr32
nastsav$datarr33
#...
nastsav$datarr45

nastsav$datebuy
nastsav$datedep
nastsav$datedepa

nastsav$dlslatrb

nastsav$datebuy
#....
nastsav$dateleftafr

nastsav$evgreen
nastsav$embport


# try to make it happen-----

# voyageid

nastsav$embreg
nastsav$embreg2
nastsav$embport# departure location
nastsav$slaximp# departure slaves
nastsav$tslavesp
nastsav$crew2# departure crew

nastsav$regarr
nastsav$regarr2
nastsav$arrport# arrival location
nastsav$slamimp# arrival slaves
nastsav$tslavesd
nastsav$crew3# arrival crew

nastsav$voy2imp # mp duration
nastsav$tonnage
nastsav$tontype


nastsav$voy2imp
#nastsav$voy1imp
nastsav$voyage
nastsav$sladvoy
nastsav$crewdied

nastsav %>%
  group_by(voy2imp,voyage) %>%
  summarise(n = n()) %>%
  .[!is.na(.$voyage),] %>%
  .[.$voy2imp != .$voyage,]

ggplot(data = nastsav, 
       aes(x = voyage, y = voy2imp)) + 
  #geom_point()+
  geom_smooth()

lm(voy2imp~voyage, nastsav) %>%
  plot()
