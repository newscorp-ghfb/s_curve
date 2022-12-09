# Add any project specific configuration here.
# Date Formats in R    https://tinyurl.com/r-date-format
add.config(
  apply.override = FALSE,
  currentYr = as.numeric(format(Sys.Date(), format="%y")),
  currentYr4 = as.numeric(format(Sys.Date(), format="%Y")),
  lastYr = as.numeric(format(Sys.Date(), format="%y")) - 1,
  LastYr4 = as.numeric(format(Sys.Date(), format="%Y"))-1,
  currentAY = as.numeric(paste(as.numeric(format(Sys.Date(), format="%y")) - 1, as.numeric(format(Sys.Date(), format="%y")), sep = "")),
  header = "Project Template" # header in reports
)
# ------------------------------------------------------------------------------
# Increasing the speed of {raster} processing with R: Part 1/3               ---
# https://tinyurl.com/3ze3hzv5                                               ---
# ------------------------------------------------------------------------------
rasterOptions(maxmemory = 1e+09)
################################################################################
## Repo Package: data management to build centralized metadata repository       ### https://github.com/franapoli/repo
## Check existence of directory and create if doesn't exist                     ### https://tinyurl.com/y3adrqwa
################################################################################
mainDir <- (".")
subDir  <- ("repo")
rp_path <- file.path(mainDir, subDir)
# ------------------------------------------------------------------------------
# create global environment to dynamically name data frames
# https://tinyurl.com/y3adrqwa                                               ###
# ------------------------------------------------------------------------------
g                   <- globalenv()           # https://tinyurl.com/r3yrspv   ###
z                   <- TRUE                  # template switch create dx_blob
# ------------------------------------------------------------------------------
syncFileDate        <- format(Sys.Date()-1, "%Y%m%d")
# ------------------------------------------------------------------------------
scholar_ext         <- ".csv.gpg"
scholar_path        <- "//apps.osfa.uga.edu/OSFA/Reports/Banner/Argos/ScholarshipUniverse_Banner_Recon"
scholar_files       <- as.data.table(list.files(scholar_path))
# ------------------------------------------------------------------------------
scholar_code_url    <- "https://apps.osfa.uga.edu/decrypt.php?path=Banner/Argos/ScholarshipUniverse_Banner_Recon/ScholarshipCodeReport_"
scholar_recn_url    <- "https://apps.osfa.uga.edu/decrypt.php?path=Banner/Argos/ScholarshipUniverse_Banner_Recon/"
# ------------------------------------------------------------------------------
# Add project specific configuration that can be overridden from load.project()
# ------------------------------------------------------------------------------
add.config(
  apply.override = TRUE
)
