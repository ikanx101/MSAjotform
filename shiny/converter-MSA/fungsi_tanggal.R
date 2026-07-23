#' Konversi Tanggal Berbahasa Indonesia ke Object Date
#'
#' Fungsi ini mengkonversi string tanggal yang ditulis dalam Bahasa Indonesia
#' menjadi object Date di R. Mendukung berbagai format penulisan seperti:
#' - "1 Januari 2024"
#' - "01-Jan-2024"
#' - "Senin, 1 Januari 2024"
#' - "2024-01-01" (format standar)
#' - "01/01/2024"
#' dan variasi lainnya.
#'
#' @param x Character vector berisi tanggal dalam Bahasa Indonesia.
#' @param tz Timezone, default "Asia/Jakarta".
#' @param locale Locale Indonesia. Default "id_ID.UTF-8".
#'
#' @return Vector of class Date.
#' @export
#'
#' @examples
#' \dontrun{
#'   konversi_tanggal("1 Januari 2024")
#'   konversi_tanggal("Senin, 1 Januari 2024")
#'   konversi_tanggal(c("1 Januari 2024", "15 Maret 2025"))
#' }
#'
konversi_tanggal <- function(x, tz = "Asia/Jakarta", locale = "id_ID.UTF-8") {

  # Cek lubridate
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' diperlukan. Install dengan: install.packages('lubridate')")
  }

  # Mapping nama bulan Indonesia (panjang) -> angka
  bulan_indonesia <- c(
    "Januari"   = "01", "Februari"  = "02", "Maret"     = "03",
    "April"     = "04", "Mei"       = "05", "Juni"      = "06",
    "Juli"      = "07", "Agustus"   = "08", "September" = "09",
    "Oktober"   = "10", "November"  = "11", "Desember"  = "12"
  )

  # Mapping nama bulan Indonesia (singkat) -> angka
  bulan_indonesia_singkat <- c(
    "Jan" = "01", "Feb" = "02", "Mar" = "03", "Apr" = "04",
    "Mei" = "05", "Jun" = "06", "Jul" = "07", "Agu" = "08",
    "Agt" = "08", "Sep" = "09", "Okt" = "10", "Nov" = "11",
    "Des" = "12"
  )

  # Nama hari Indonesia (untuk dibersihkan)
  hari_indonesia <- c(
    "Senin", "Selasa", "Rabu", "Kamis", "Jumat", "Sabtu", "Minggu"
  )

  # --- Helper functions ---

  # Ganti nama bulan Indonesia dengan angka
  ganti_bulan <- function(txt) {
    for (nm in names(bulan_indonesia)) {
      pattern <- sprintf("\\b%s\\b", nm)
      txt <- gsub(pattern, bulan_indonesia[nm], txt, ignore.case = TRUE)
    }
    for (nm in names(bulan_indonesia_singkat)) {
      pattern <- sprintf("\\b%s\\b", nm)
      txt <- gsub(pattern, bulan_indonesia_singkat[nm], txt, ignore.case = TRUE)
    }
    return(txt)
  }

  # Hapus nama hari dan koma
  bersihkan_hari <- function(txt) {
    pattern_hari <- paste0(
      "\\b(", paste(hari_indonesia, collapse = "|"), "\\b),?\\s*"
    )
    txt <- gsub(pattern_hari, "", txt, ignore.case = TRUE, perl = TRUE)
    txt <- gsub("\\s*,\\s*", " ", txt)
    return(trimws(txt))
  }

  # Parsing manual sebagai fallback
  parse_manual <- function(txt) {
    parts <- strsplit(txt, "-")[[1]]
    if (length(parts) != 3) return(NA)
    if (!all(grepl("^[0-9]+$", parts))) return(NA)

    p1 <- as.numeric(parts[1])
    p2 <- as.numeric(parts[2])
    p3 <- as.numeric(parts[3])

    # Deteksi urutan: bagian mana yang tahun?
    if (p3 > 31) {
      # Format: dd-mm-yyyy (Indonesia)
      hari <- p1; bulan <- p2; tahun <- p3
    } else if (p1 > 31) {
      # Format: yyyy-mm-dd
      tahun <- p1; bulan <- p2; hari <- p3
    } else {
      # Default Indonesia: dmy
      hari <- p1; bulan <- p2; tahun <- p3
    }

    # Handle 2-digit year
    if (tahun < 100) {
      tahun <- ifelse(tahun > 30, 1900 + tahun, 2000 + tahun)
    }

    # Validasi rentang
    if (hari >= 1 && hari <= 31 && bulan >= 1 && bulan <= 12 && tahun >= 1900) {
      tanggal_str <- sprintf("%04d-%02d-%02d", tahun, bulan, hari)
      return(tryCatch(as.Date(tanggal_str), error = function(e) NA))
    }

    return(NA)
  }

  # --- Main processing ---

  # Step 1: Bersihkan nama hari
  x_clean <- bersihkan_hari(x)

  # Step 2: Ganti nama bulan Indonesia dengan angka
  x_numeric <- ganti_bulan(x_clean)

  # Step 3: Standarisasi separator
  x_std <- gsub("[/\\s\\.]+", "-", x_numeric)
  x_std <- gsub("-+", "-", x_std)
  x_std <- trimws(x_std)

  # Step 4: Parsing
  result <- rep(as.Date(NA), length(x))

  for (i in seq_along(x_std)) {
    if (is.na(x_std[i]) || x_std[i] == "" || x_std[i] == "-") {
      next
    }

    tanggal_conv <- NA

    # Coba pakai parse_date_time dari lubridate
    tryCatch({
      orders <- c("dmy", "ymd", "mdy", "Ymd", "dmY")
      parsed <- lubridate::parse_date_time(
        x_std[i],
        orders = orders,
        tz = tz,
        locale = locale,
        quiet = TRUE
      )
      if (!is.na(parsed)) {
        tanggal_conv <- as.Date(parsed)
      }
    }, error = function(e) {})
    
    # Coba parse_date_time dengan locale "C" sebagai fallback
    if (is.na(tanggal_conv)) {
      tryCatch({
        parsed <- lubridate::parse_date_time(
          x_std[i],
          orders = c("dmy", "ymd", "mdy"),
          tz = tz,
          locale = "C",
          quiet = TRUE
        )
        if (!is.na(parsed)) {
          tanggal_conv <- as.Date(parsed)
        }
      }, error = function(e) {})
    }

    # Fallback: parsing manual
    if (is.na(tanggal_conv)) {
      tanggal_conv <- parse_manual(x_std[i])
    }

    result[i] <- tanggal_conv
  }

  return(result)
}

