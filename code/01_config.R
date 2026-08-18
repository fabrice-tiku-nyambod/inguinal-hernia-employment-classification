# 01_config.R -- paths and constants. The only settable value is the data
# directory, via the MARKETSCAN_DIR environment variable or the fallback below.
#
# DATA_DIR must contain the MarketScan SAS files:
#   fabrice_o_YYYY.sas7bdat, fabrice_s_YYYY.sas7bdat,
#   fabrice_i_YYYY.sas7bdat, fabrice_a_YYYY.sas7bdat   (YYYY = 2016..2021)

# Set MARKETSCAN_DIR in your environment, or replace the fallback below with
# the local path to the licensed SAS extract. This is the only line to edit.
DATA_DIR <- Sys.getenv("MARKETSCAN_DIR",
                       unset = "c:/Users/tikuf/Desktop/Claude/Fabrice_Tiku_Nyambod-MarketScan-main/")

# Everything below is relative to this script.
ROOT <- normalizePath("..", mustWork = FALSE)
OUT  <- file.path(ROOT, "output")
FIGD <- file.path(OUT, "figures")
dir.create(OUT,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGD, showWarnings = FALSE, recursive = TRUE)

YEARS    <- 2016:2021
OPEN_CPT <- c("49505", "49507", "49520", "49521", "49525")
LAP_CPT  <- c("49650", "49651")
REP_CPT  <- c(OPEN_CPT, LAP_CPT)
ROBOT_HCPCS <- "S2900"

# CPI-U, annual averages, BLS series CUUR0000SA0; constant 2021 US dollars
CPI      <- c(`2016` = 240.007, `2017` = 245.120, `2018` = 251.107,
              `2019` = 255.657, `2020` = 258.811, `2021` = 270.970)
DEFLATOR <- CPI["2021"] / CPI

PLAN_LABELS <- c("1" = "Basic/major medical", "2" = "Comprehensive", "3" = "EPO",
                 "4" = "HMO", "5" = "POS", "6" = "PPO",
                 "7" = "POS with capitation", "8" = "CDHP", "9" = "HDHP")

# Medicare benchmark constants and the RVU file paths were carried over from the
# companion study "Patient Out-of-Pocket Costs at Lower-Priced Sites of
# Service in Ambulatory Inguinal Hernia Repair". This study uses neither, and
# they are not defined
# here; recover them from that project if a benchmark analysis is ever added.

# EGEOLOC -> state, from the MarketScan Data Dictionary, Attachment J. Codes not
# listed are region or division aggregates and resolve to NA, which excludes them
# from any state-level analysis. Embedded here rather than read from a sibling
# project so this code folder is self-contained when submitted.
STATE_LABELS <- c(
  "4"="Connecticut","5"="Maine","6"="Massachusetts","7"="New Hampshire","8"="Rhode Island",
  "9"="Vermont","11"="New Jersey","12"="New York","13"="Pennsylvania","16"="Illinois",
  "17"="Indiana","18"="Michigan","19"="Ohio","20"="Wisconsin","22"="Iowa","23"="Kansas",
  "24"="Minnesota","25"="Missouri","26"="Nebraska","27"="North Dakota","28"="South Dakota",
  "31"="Washington, DC","32"="Delaware","33"="Florida","34"="Georgia","35"="Maryland",
  "36"="North Carolina","37"="South Carolina","38"="Virginia","39"="West Virginia",
  "41"="Alabama","42"="Kentucky","43"="Mississippi","44"="Tennessee","46"="Arkansas",
  "47"="Louisiana","48"="Oklahoma","49"="Texas","52"="Arizona","53"="Colorado","54"="Idaho",
  "55"="Montana","56"="Nevada","57"="New Mexico","58"="Utah","59"="Wyoming","61"="Alaska",
  "62"="California","63"="Hawaii","64"="Oregon","65"="Washington")
