setwd("~/MSAjotform/algoritma")

# bebersih global environment
rm(list=ls())

# libraries
library(dplyr)
library(tidyr)
library(readxl)

# memasukkan path folder
path_folder = "~/MSAjotform/input"

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

trans_tahun = function(temp){
  temp = as.Date(temp,"%B %d, %Y")
  output = format(temp,"%Y")
  as.numeric(output)
}

trans_bulan = function(temp){
  temp = as.Date(temp,"%B %d, %Y")
  output = format(temp,"%m")
  as.numeric(output)
}

# fungsi untuk bikin judul proper
proper_new = function(x){
  tess = stringi::stri_trans_general(x,id = "Title")
  gsub("\\_"," ",tess)
}

# ==========================================================
# kita mulai
# mulai paste dari sini

df_final = 
  df %>% 
  rowwise() %>% 
  mutate(tanggal_submisi = trans_submission_date(submission_date),
         tanggal_visit   = trans_submission_date(date),
         tahun           = trans_tahun(submission_date),
         bulan           = trans_bulan(submission_date),
         date            = NULL) %>% 
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
  rename(nama_outlet = nama_outlet_horeka_rumah_sakit_umkm_gym_atau_instansi,
         jabatan_pic_outlet = jabatan,
         nama_rumah_sakit = nama_rumah_sakit_jika_belum_ada_hub_okky) %>% 
  select(tanggal_submisi,tanggal_visit,bulan,tahun,provinsi,kota_kab,dept,pic_nutrifood,
         jenis_kunjungan,area_rumah_sakit,nama_rumah_sakit,
         nama_outlet,channel,category,
         terdapat_sugar_display_condiment_bar_atau_sugar_bowl,
         outlet_approach_project_item_bulk,
         termasuk_specialty_coffee_shop_roastery_atau_terdapat_manual_brew,
         google_rate,google_review,status_brand_tropicana_slim,status_brand_nutrisari,
         status_brand_hi_lo,status_brand_lokalate,status_brand_l_men,
         jabatan_pic_outlet,nama_pic_outlet,penjualan,notes) 

df_final = 
  df_final %>% 
  mutate(id = 1:nrow(df_final)) %>%
  relocate(id,.before = tanggal_submisi) %>% 
  separate_rows(penjualan,
                sep = "\n") %>%
  filter(!grepl("total",penjualan,ignore.case = T)) %>% 
  separate(penjualan,
           into = c("item","dummy"),
           sep = "\\(Amount:") %>% 
  separate(dummy,
           into = c("price","quantity"),
           sep = "IDR, Quantity:") %>% 
  mutate(quantity = gsub("\\)","",quantity),
         item = stringr::str_trim(item),
         price = stringr::str_trim(price),
         price = gsub("\\,","",price),
         price = as.numeric(price),
         quantity = stringr::str_trim(quantity),
         quantity = as.numeric(quantity),
         total_value = price * quantity) %>% 
  rowwise() %>% 
  mutate(brand = case_when(
    grepl("ts|tropica",item,ignore.case = T) ~ "TS",
    grepl("lmen|l men|l-men",item,ignore.case = T) ~ "L-Men",
    grepl("ns|nutri",item,ignore.case = T) ~ "NS",
    grepl("lokal",item,ignore.case = T) ~ "Lokalate",
    grepl("Diabetamil",item,ignore.case = T) ~ "Diabetamil",
    grepl("hilo|hi lo",item,ignore.case = T) ~ "HILO"
  )) %>% 
  ungroup() %>% 
  mutate(area = "akan diisi sesuai input") %>% 
  relocate(area,.before = tanggal_submisi)

colnames(df_final) = proper_new(colnames(df_final))

openxlsx::write.xlsx(df_final,file = "output.xlsx")


