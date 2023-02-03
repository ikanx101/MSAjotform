# ==============================================================================
#
#   ikanx101.com proudly present
#   MSA Jotform converter 2023
#
# ==============================================================================

# ==============================================================================
# libraries
library(reshape2)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(shinydashboard)
library(tidytext)
library(shiny)
library(shinymanager)
# ==============================================================================

# ==============================================================================
# dimulai dari hati yang bersih
rm(list=ls())

# buat credential
credentials = data.frame(
    user             = c("msa", "ikanx_server"), # mandatory
    password         = c("nutrifood", "ahnaf"),  # mandatory
    admin            = c(TRUE, TRUE),
    stringsAsFactors = FALSE
)
# ==============================================================================

# ==============================================================================
# kita buat dulu beberapa function

# transform submission date
trans_submission_date = function(temp){
  temp = as.Date(temp,"%B %d, %Y")
  format(temp,"%d-%m-%Y")
}
# transform ke bentuk tahun
trans_tahun = function(temp){
  temp = as.Date(temp,"%B %d, %Y")
  output = format(temp,"%Y")
  as.numeric(output)
}
# transform ke bentuk bulan
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
# ==============================================================================

# ==============================================================================
# USER INTERFACE PART

# header
header = dashboardHeader(title = "Jotform Converter MSA ver1.0",
                         titleWidth = 300)

# sidebar menu
sidebar = dashboardSidebar(width = 300,
                           sidebarMenu(
                               menuItem(tabName = 'readme',
                                        text = 'Read Me',icon = icon('check')),
                               menuItem(tabName = 'converter',
                                        text = 'Converter',icon = icon('dollar'))
                                       )
                          )

# tab Read Me
readme = tabItem(tabName = 'readme',
                 fluidRow(
                         column(width = 12,
                                h1('Read Me'),
                                br(),
                                h4("Web apps converter ini digunakan untuk mengubah struktur data hasil export dari JotForm menjadi struktur yang mudah dipivot di Ms. Excel."),
                                h4("Pastikan bahwa file yang hendak di-convert memiliki ukuran < 5 Mb dan memiliki penamaan variabel standar (belum pernah diubah atau diedit sebelumnya)."),
                                br(),
                                h5("Jika terjadi kendala atau pertanyaan, feel free to discuss ya: fadhli.mohammad@nutrifood.co.id"),
                                br(),
                                br(),
                                h4(paste0("update 3 Februari 2023 14:06 WIB")),
                                h5("Copyright 2023"),
                                h5("Dibuat menggunakan R")
                               )
                          )
                 )

# tab converter
converter = tabItem(tabName = 'converter',
                    fluidRow(
                        column(width = 12,
                               h1('Converter Jotform'),
                               textInput("judul_area","Masukkan Nama AREA"),
                               h4("Silakan upload file Anda:"),
                               fileInput('target_upload', 'Pilih file',
                                         accept = c('xlsx')
                               ),
                               br(),
                               downloadButton("downloadData", "Download")
                               )
                            )
                    )


# body
body = dashboardBody(tabItems(readme,converter))

# ui all
ui = secure_app(dashboardPage(skin = "green",header,sidebar,body))
# ==============================================================================

# ==============================================================================
# SERVER PART
# Define server logic required to do a lot of things
server <- function(input,output,session){
    # credential untuk masalah login
    res_auth = secure_server(check_credentials = check_credentials(credentials))

    # konverter dimulai dari sini
    data_upload <- reactive({
        # tahap pertama adalah mengambil data yang diupload
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        
        # kita ambil nama kota dulu
        nama_kota_save = input$judul_area
        
        # baca data
        df <- read_excel(inFile$datapath) %>% janitor::clean_names() 
        
        # =================================
        # mulai paste dari sini
        
        # tahap kedua adalah menghitung dan mengkonversi
        df_final = 
          df %>% 
          rowwise() %>% 
          mutate(tanggal_submisi = trans_submission_date(submission_date),
                 tahun           = trans_tahun(submission_date),
                 bulan           = trans_bulan(submission_date)) %>% 
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
                 jabatan_pic_outlet = jabatan) %>% 
          select(tanggal_submisi,bulan,tahun,provinsi,kota_kab,dept,pic_nutrifood,
                 jenis_kunjungan,nama_outlet,channel,category,
                 terdapat_sugar_display_condiment_bar_atau_sugar_bowl,
                 outlet_approach_project_item_bulk,
                 termasuk_specialty_coffee_shop_roastery_atau_terdapat_manual_brew,
                 google_rate,google_review,status_brand_tropicana_slim,status_brand_nutrisari,
                 status_brand_hi_lo,status_brand_lokalate,status_brand_l_men,
                 jabatan_pic_outlet,nama_pic_outlet,penjualan,notes) 
        
        # tahap ketiga adalah membuat baris ke bawah setiap ada penjualan
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
          mutate(area = "BALI IBT") %>% 
          relocate(area,.before = tanggal_submisi)
        
        # tahap terakhir adalah mengganti nama kolom agar proper
        colnames(df_final) = proper_new(colnames(df_final))
        
        # =================================
        # akhir paste di sini
        return(df_final)
        
    })
    
    data = data_upload
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("MSA Jotform ", Sys.time(), ".xlsx", sep="")
        },
        content = function(file) {
            openxlsx::write.xlsx(data(), file)
        })
    
    
    }
# ==============================================================================

# ==============================================================================
# Run the application 
shinyApp(ui = ui, server = server)
# alhamdulillah selesai
# ==============================================================================

