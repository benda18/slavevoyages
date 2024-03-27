# data

library(renv)
library(readr)
library(haven)
library(lubridate)
library(janitor)
library(dplyr)
library(ggplot2)
library(data.table)
library(usethis) # https://happygitwithr.com/existing-github-last

renv::snapshot()
renv::status()

rm(list=ls());cat('\f');gc()

# URL sources----
spss_ta.url     <- "https://www.slavevoyages.org/documents/download/tastdb-exp-2020.sav"
spss_ia.url     <- "https://www.slavevoyages.org/documents/download/I-Am1.0.sav"
codebook_ta.url <- "https://www.slavevoyages.org/documents/download/SPSS_Codebook_2023-11-06.pdf"
codebook_ia.rl  <- "https://www.slavevoyages.org/documents/download/SPSS_Codebook_2019.pdf"
# note:  must dl data manually and put in directory

# Set wd----
wd.home <- "C:/Users/bende/Documents/R/play/slavevoyages"
wd.data <- paste(getwd(), "data", sep = "/", collapse = "/")
try(setwd(wd.data))


# Extract Data----
master_names <- read_csv("AfricanNamesDatabase.csv")
master_ta <- haven::read_sav("tastdb-exp-2020.sav")
master_ia <- haven::read_sav("I-Am1.0.sav")
try(setwd(wd.home))



rm(codebook_ia.rl, codebook_ta.url, 
   spss_ia.url, spss_ta.url)


# build cw of column definitions----
cw_coldefs <- NULL

for(i in 1:ncol(master_ta)){
  if(i == 1){
    cw_coldefs <- data.frame(dataset = "master_ta", 
                             colname = names(master_ta[,i]), 
                             coldef  = attr(master_ta[,i][[1]], "label") ) %>%
      as_tibble()
  }else{
    cw_coldefs <- rbind(cw_coldefs, 
                        data.frame(dataset = "master_ta", 
                                   colname = names(master_ta[,i]), 
                                   coldef  = attr(master_ta[,i][[1]], "label") ))
  }
}

for(i in 1:ncol(master_ia)){
    cw_coldefs <- rbind(cw_coldefs, 
                        data.frame(dataset = "master_ia", 
                                   colname = names(master_ia[,i]), 
                                   coldef  = attr(master_ia[,i][[1]], "label") ))
 
}

# adjustments
cw_coldefs$is_imp   <- grepl("IMP", cw_coldefs$colname, ignore.case = T)
cw_coldefs$is_name  <- grepl("name|captain|owner", cw_coldefs$coldef, ignore.case = T)
cw_coldefs$is_date  <- grepl("day of |date|year|century|decade|quinquennium|month", cw_coldefs$coldef, ignore.case = T)
cw_coldefs$is_sex   <- grepl("male |female |males|females|men |women |boys|girls|gender unspecified", cw_coldefs$coldef, ignore.case = T)
cw_coldefs$is_geo   <- grepl("place |region |port |place |nation", cw_coldefs$coldef, ignore.case = T)
cw_coldefs$is_age   <- grepl("children|infant|women |men |boys |girls ", cw_coldefs$coldef, ignore.case = T)
cw_coldefs$is_middlepassage <- grepl("middle passage", cw_coldefs$coldef, ignore.case = T)
cw_coldefs$is_outcome <- grepl("outcome", cw_coldefs$coldef, ignore.case = T)
cw_coldefs$is_owner <- grepl("owner", cw_coldefs$coldef, ignore.case = T)
cw_coldefs$is_ship  <- grepl("ship|vessel", cw_coldefs$coldef, ignore.case = T)


grep("ID", cw_coldefs$colname, ignore.case = F, value = T)

grep("ship|vessel", cw_coldefs$coldef, ignore.case = T, value = T)

sample(cw_coldefs$coldef, size = 5, replace = F)


# explore names data----

# owners
cw_coldefs[cw_coldefs$is_owner,] %>%
  group_by(colname, coldef) %>%
  summarise()

master_names %>%
  .[!is.na(.$age) & 
      !is.na(.$height) & 
      !is.na(.$sexage) & 
      .$age <= 100,] %>%
  mutate(., 
         sex2 = ifelse(sexage %in% c("Boy", "Male", "Man"), 
                       "male", "female")) %>%
  ggplot(data = ., 
         aes(x = age, y = height)) + 
  geom_smooth(aes(color = sex2),
              se = F)+
  #geom_bin2d()+
  # geom_point()+
  scale_fill_viridis_c(option = "C", trans = "log10")

ggplot() + 
  geom_point(data = master_names[master_names$age <= 100,], 
             aes(x = age, y = height))
