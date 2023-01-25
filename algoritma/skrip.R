setwd("~/MSA Jotform/algoritma")

# bebersih global environment
rm(list=ls())

# libraries
library(dplyr)
library(tidyr)
library(readxl)

# memasukkan path folder
path_folder = "~/MSA Jotform/input"

# ambil nama file
files = list.files(path_folder,full.names = T)
nama  = list.files(path_folder)

# kita induksi dulu
i         = 1
nama_file = files[i]

# ambil file excel
df = read_excel(nama_file) %>% janitor::clean_names()

# ==========================================================
# kita buat dulu beberapa function
trans_submission_date = function(temp){
  temp = as.Date(temp,"%B %d, %Y")
  format(temp,"%d-%m-%Y")
}

# fungsi untuk bikin judul proper
proper_new = function(x){
  tess = stringi::stri_trans_general(x,id = "Title")
  gsub("\\_"," ",tess)
}

# ==========================================================
# kita mulai
df %>% 
  rowwise() %>% 
  mutate(submission_date = trans_submission_date(submission_date),
         date            = trans_submission_date(date)) %>% 
  ungroup() %>% 
  separate(provinsi_kota_kabupaten,
           into = c("provinsi","kota_kab"),
           sep  = ";") %>% 
  mutate(provinsi = stringr::str_trim(provinsi),
         kota_kab = stringr::str_trim(kota_kab)) %>% 
  separate(channel,
           into = c("channel","category"),
           sep  = ";") %>% 
  mutate(channel  = stringr::str_trim(channel),
         category = stringr::str_trim(category)) %>% 
  separate(nama_rumah_sakit_jika_nama_rs_belum_ada_hubungi_okky,
           into = c("area_rumah_sakit",
                    "nama_rumah_sakit_jika_belum_ada_hub_okky"),
           sep  = ";") %>% 
  mutate(area_rumah_sakit = stringr::str_trim(area_rumah_sakit),
         nama_rumah_sakit_jika_belum_ada_hub_okky = stringr::str_trim(nama_rumah_sakit_jika_belum_ada_hub_okky)) %>% 
  View()





