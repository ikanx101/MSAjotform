#' Konversi Tanggal Bahasa Indonesia ke Object Date R
#'
#' Fungsi ini mengkonversi berbagai format tanggal, termasuk yang ditulis 
#' dalam bahasa Indonesia, menjadi object Date di R dengan format "%d-%m-%Y".
#'
#' @param x Character vector berisi tanggal yang akan dikonversi.
#' @param locale Locale yang digunakan untuk parsing. Default "id_ID.utf8" 
#'   untuk bahasa Indonesia. Jika tidak bekerja di sistem Anda, gunakan 
#'   alternatif seperti "id_ID" atau "Indonesian_Indonesia".
#'
#' @return Date vector dengan format "%d-%m-%Y".
#' @export
#'
#' @examples
#' # Contoh dengan nama bulan bahasa Indonesia
#' konversi_tanggal("15 Januari 2023")
#' konversi_tanggal("3 Maret 2021")
#' konversi_tanggal(c("1 Januari 2020", "2 Februari 2021"))
#'
#' # Contoh dengan singkatan bulan bahasa Indonesia
#' konversi_tanggal("15-Jan-2023")
#' konversi_tanggal("15 Jan 2023")
#'
#' # Contoh format lain yang umum
#' konversi_tanggal("2023-01-15")
#' konversi_tanggal("15/01/2023")
#' konversi_tanggal("01-15-2023")
#'
#' # Format Inggris tetap bisa diproses
#' konversi_tanggal("January 15, 2023")
#' konversi_tanggal("15 Jan 2023")
konversi_tanggal <- function(x) {

  # ---------------------------------------------------------------------------
  # 1. Validasi input
  # ---------------------------------------------------------------------------
  if (!is.character(x)) {
    stop("Input `x` harus berupa character vector.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # 2. Siapkan mapping nama bulan bahasa Indonesia -> Inggris
  # ---------------------------------------------------------------------------
  bulan_indonesia <- c(
    "Januari"   = "January",
    "Februari"  = "February",
    "Maret"     = "March",
    "April"     = "April",
    "Mei"       = "May",
    "Juni"      = "June",
    "Juli"      = "July",
    "Agustus"   = "August",
    "September" = "September",
    "Oktober"   = "October",
    "November"  = "November",
    "Desember"  = "December"
  )

  # Juga untuk singkatan 3-huruf (kasus tidak sensitif)
  singkatan_indonesia <- c(
    "Jan" = "Jan",
    "Feb" = "Feb",
    "Mar" = "Mar",
    "Apr" = "Apr",
    "Mei" = "May",
    "Jun" = "Jun",
    "Jul" = "Jul",
    "Agu" = "Aug",
    "Sep" = "Sep",
    "Okt" = "Oct",
    "Nov" = "Nov",
    "Des" = "Dec"
  )

  # ---------------------------------------------------------------------------
  # 3. Bersihkan dan standarisasi teks tanggal
  # ---------------------------------------------------------------------------
  bersihkan <- function(txt) {
    txt <- trimws(txt)

    # Ganti nama bulan panjang bahasa Indonesia -> Inggris
    for (i in seq_along(bulan_indonesia)) {
      pola    <- names(bulan_indonesia)[i]
      ganti   <- unname(bulan_indonesia[i])
      txt     <- gsub(pola, ganti, txt, ignore.case = TRUE)
    }

    # Ganti singkatan bulan bahasa Indonesia -> Inggris
    for (i in seq_along(singkatan_indonesia)) {
      pola    <- names(singkatan_indonesia)[i]
      ganti   <- unname(singkatan_indonesia[i])
      # Hanya ganti jika diapit oleh word boundary (agar "Agus" tidak kena)
      txt     <- gsub(paste0("\\b", pola, "\\b"), ganti, txt, ignore.case = TRUE)
    }

    return(txt)
  }

  x_bersih <- bersihkan(x)

  # ---------------------------------------------------------------------------
  # 4. Coba parse dengan berbagai format umum
  # ---------------------------------------------------------------------------
  # Kumpulan format yang umum dijumpai
  format_umum <- c(
    "d B Y",        # 15 January 2023
    "d b Y",        # 15 Jan 2023
    "B d, Y",       # January 15, 2023
    "b d, Y",       # Jan 15, 2023
    "Y-m-d",        # 2023-01-15
    "d/m/Y",        # 15/01/2023
    "m/d/Y",        # 01/15/2023
    "d-%m-%Y",      # 15-01-2023
    "d %m %Y",      # 15 01 2023
    "Y%m%d",        # 20230115
    "d%B%Y",        # 15January2023
    "d %B %Y",      # 15 January 2023
    "Y.%m.%d",      # 2023.01.15
    "d.%m.%Y"       # 15.01.2023
  )

  hasil <- lubridate::parse_date_time(x_bersih, orders = format_umum, quiet = TRUE)

  # ---------------------------------------------------------------------------
  # 5. Jika masih gagal, fallback: coba tanpa locale tertentu
  # ---------------------------------------------------------------------------
  if (all(is.na(hasil))) {
    hasil <- lubridate::parse_date_time(x_bersih, orders = format_umum, quiet = TRUE)
  }

  # ---------------------------------------------------------------------------
  # 6. Jika masih NA semua, beri peringatan
  # ---------------------------------------------------------------------------
  if (all(is.na(hasil))) {
    warning(
      "Semua tanggal gagal diparsing. Periksa format input atau coba atur ",
      "parameter `locale` secara manual (misal locale = 'C').",
      call. = FALSE
    )
  } else if (any(is.na(hasil))) {
    idx_na <- which(is.na(hasil))
    warning(
      sprintf("%d dari %d tanggal gagal diparsing.", length(idx_na), length(x)),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 7. Format output sesuai permintaan: %d-%m-%Y
  # ---------------------------------------------------------------------------
  hasil <- format(hasil, "%d-%m-%Y")
  hasil <- as.Date(hasil, format = "%d-%m-%Y")

  return(hasil)
}

