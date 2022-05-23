library(tidyverse)
library(dplyr)
library(stringr)
library(googlesheets4)
library(lubridate)



#read data unit kerja
tabel_uk <- range_read("https://docs.google.com/spreadsheets/d/1_zKzlIdAXPgbtgtfrTAlyo2NJdoY5n0tVuXXa4DltfU/edit#gid=1209260409", sheet = "uk")
tabel_uk <- tabel_uk %>%  select (c(2,7,8,9))

#mengubah class kode seksi
as.character(tabel_uk$Planner.group)

##Create Variable
##Tabel KOB 2020
kob_17 <- read.csv("https://docs.google.com/spreadsheets/d/13tWvRTzE3sUuqul-7oes58txiEZDO-O-/export?format=csv&gid=1211018684")


#tabel order 2021
IW39_2017 <- read.csv("https://docs.google.com/spreadsheets/d/13oAv9Kvkbg8fOegIBnNP8eOU-cBhr-wg/export?format=csv&gid=1996476794")

#Tabel cc
cc <- read.csv("https://docs.google.com/spreadsheets/d/15DnkUrx2Cl0Sk4L_VU1ghArLlbgi9WveUQ4Rmd5LAp4/export?format=csv&gid=0")

#Tabel cost center area
cost_center <- read.csv("https://docs.google.com/spreadsheets/d/15DnkUrx2Cl0Sk4L_VU1ghArLlbgi9WveUQ4Rmd5LAp4/export?format=csv&gid=1128384635")


#join order dengan Plant, area, detail area

cc_order_17 <- cc %>% select(BU, Area.Proses, PLANT, Detail.area) %>%
  rename(Cost.Center = BU)


order_and_cc_17 <- merge(IW39_2017, cc_order_17, by="Cost.Center")

#Membuat Column OVH pada daftar Order
order_area_17 <- order_and_cc_17 %>% 
  mutate(Jenis.aktivitas = case_when(str_detect(Description,".[o|O][v|V][h|H].|[o|O][v|V][h|H]") ~ "Overhoule",
                                     str_detect(Description,".[o|O][v|V][e|E][r|R][h|H].|[o|O][v|V][e|E][r|R][h|H]") ~ "Overhoule",
                                     str_detect(Description,".[p|P][a|A][t|T][c|C].|[p|P][a|A][t|T][c|C]") ~ "Patchjob",
                                     TRUE ~ "Operasional")) 





#Daftar lokasi ovh kiln 
area_kiln1 <- c("COAL MILL T1SIE RKC1",
                "CRUSHER BT. KAPUR T1",
                "CRUSHER TNH LIAT T1",
                "KILN T1 - SIE RKC1",
                "RAW MILL T1 SIE RKC1"
)

#Daftar lokasi ovh kiln 2
area_kiln2 <- c("COAL MILL T2 SI RKC2",
                "CRUSHER BT. KAPUR T2",
                "CRUSHER TNH LIAT T2",
                "KILN T2 - SIE RKC2",
                "RAW MILL T2 SIE RKC2"
)

#Daftar lokasi ovh kiln 3
area_kiln3 <- c("COAL MILL T3 SI RKC3",
                "CRUSHER BT. KAPUR T3",
                "CRUSHER TNH LIAT T3",
                "KILN T3 - SIE RKC3",
                "RAW MILL T3 SIE RKC3"
)

#Daftar lokasi ovh kiln 4
area_kiln4 <- c("COAL MILL T4SIE RKC4",
                "CRUSHER BT. KAPUR T4",
                "CRUSHER TNH LIAT T4",
                "KILN T4 - SIE RKC4",
                "RAW MILL T4 SIE RKC4"
)



#menambahkan variabel OVH_AREA
order_area_17 <- order_area_17 %>%
  mutate(kategori_ovh = case_when(
    Jenis.aktivitas == "Overhoule" & Detail.area %in% area_kiln1 ~ "Overhoule Kiln 1",
    Jenis.aktivitas == "Overhoule" & Detail.area %in% area_kiln2 ~ "Overhoule Kiln 2",
    Jenis.aktivitas == "Overhoule" & Detail.area %in% area_kiln3 ~ "Overhoule Kiln 3",
    Jenis.aktivitas == "Overhoule" & Detail.area %in% area_kiln4 ~ "Overhoule Kiln 4",
    Jenis.aktivitas == "Overhoule" & Detail.area == "FINISH MILL 1 TBN 1" ~ "Overhoule Mill 1",
    Jenis.aktivitas == "Overhoule" & Detail.area == "FINISH MILL 1 TBN 2" ~ "Overhoule Mill 3",
    Jenis.aktivitas == "Overhoule" & Detail.area == "FINISH MILL 1 TBN 3" ~ "Overhoule Mill 5",
    Jenis.aktivitas == "Overhoule" & Detail.area == "FINISH MILL 1 TBN 4" ~ "Overhoule Mill 7",
    Jenis.aktivitas == "Overhoule" & Detail.area == "FINISH MILL 2 TBN 1" ~ "Overhoule Mill 2",
    Jenis.aktivitas == "Overhoule" & Detail.area == "FINISH MILL 2 TBN 2" ~ "Overhoule Mill 4",
    Jenis.aktivitas == "Overhoule" & Detail.area == "FINISH MILL 2 TBN 3" ~ "Overhoule Mill 6",
    Jenis.aktivitas == "Overhoule" & Detail.area == "FINISH MILL 2 TBN 4" ~ "Overhoule Mill 8",
    Jenis.aktivitas == "Overhoule" & Detail.area == "FINISH MILL 9 TBN" ~ "Overhoule Mill 9",
    Jenis.aktivitas == "Overhoule" & Detail.area == "NEW COAL MILL T123" ~ "Overhoule Kiln 1",
    Jenis.aktivitas == "Overhoule" & PLANT == "Penunjang Tuban" ~ "Overhoule Penunjang Tuban",
    Jenis.aktivitas == "Overhoule" & Area.Proses == "Packer" ~ "Operasional",
    Jenis.aktivitas == "Patchjob" ~ "Patchjob",
    TRUE ~ "Operasional"))


#Memilih kolom kob yang dipakai
kob_17 <- kob_17 %>% select(c(2:10))

#Join KOB dan jenis aktivitas order
kob17_join <- merge(kob_17, order_area_17, by="Order")


#Get date format
kob17_join <- kob17_join %>% 
  mutate (post.date = as.Date(Posting.Date, origin = "1899-12-30")) %>% 
  
  mutate(MONTH = month(post.date), Tahun = 2017)

#menggabungkan kob dan nama unit kerja
KOB_UK_17 <- merge(kob18_join, tabel_uk, by="Planner.group")

write.csv(KOB_UK_17, "KOB17.csv") 



