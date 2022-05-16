library(tidyverse)
library(dplyr)
library(stringr)
library(googlesheets4)




#read data unit kerja
tabel_uk <- range_read("https://docs.google.com/spreadsheets/d/1_zKzlIdAXPgbtgtfrTAlyo2NJdoY5n0tVuXXa4DltfU/edit#gid=1209260409", sheet = "uk")
tabel_uk <- tabel_uk %>%  select (c(2,7,8,9))

#mengubah class kode seksi
as.character(tabel_uk$Planner.group)

##Create Variable
##Tabel KOB 2021
kob_21 <- read.csv("https://docs.google.com/spreadsheets/d/14KEND2KI7s0snPi3Uha-RMUjfIjrcJo1/export?format=csv&gid=443830354")
head(kob_21)

#tabel order 2021
IW39_2021 <- read.csv("https://docs.google.com/spreadsheets/d/14S644y8QFWWPKoiFPdSFN7rREtPyaG_W/export?format=csv&gid=752434217")

#Tabel cc
cc <- read.csv("https://docs.google.com/spreadsheets/d/15DnkUrx2Cl0Sk4L_VU1ghArLlbgi9WveUQ4Rmd5LAp4/export?format=csv&gid=0")

#Tabel cost center area
cost_center <- read.csv("https://docs.google.com/spreadsheets/d/15DnkUrx2Cl0Sk4L_VU1ghArLlbgi9WveUQ4Rmd5LAp4/export?format=csv&gid=1128384635")


#join order dengan Plant, area, detail area

cc_order <- cc %>% select(BU, Area.Proses, PLANT, Detail.area) %>%
            rename(Cost.Center = BU)
            

order_and_cc <- merge(IW39_2021, cc_order, by="Cost.Center")

#Membuat Column OVH pada daftar Order
order_area <- order_and_cc %>% 
              mutate(Jenis.aktivitas = case_when(str_detect(Description,".[o|O][v|V][h|H].|[o|O][v|V][h|H]") ~ "Overhoule",
                                                 str_detect(Description,".[o|O][v|V][e|E][r|R][h|H].|[o|O][v|V][e|E][r|R][h|H]") ~ "Overhoule",
                                                 str_detect(Description,".[p|P][a|A][t|T][c|C].|[p|P][a|A][t|T][c|C]") ~ "Patchjob",
                                                 TRUE ~ "Operasional")) %>%
  write.csv("daftar_order.csv")



#Memilih kolom kob yang dipakai
kob_21 <- kob_21 %>% select(c(2:10))

#Join KOB dan jenis aktivitas order
kob21_join <- merge(kob_21, order_area, by="Order")

#Menggunakan  library google sheets
library(googlesheets4)

#Get date format
kob21_join <- kob21_join %>% 
              mutate (post.date = as.Date(Posting.Date, origin = "1899-12-30")) %>% 
  
              mutate(MONTH = month(post.date), Tahun = 2021)

#menggabungkan kob dan nama unit kerja
KOB_UK_21 <- merge(kob21_join, tabel_uk, by="Planner.group")

write_tsv(KOB_UK_21, "KOB21")

