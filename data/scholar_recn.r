################################################################################
## Step 00.01 load scholar_recn file                                         ###
################################################################################
scholar_recn_url <- paste0(scholar_recn_url,
    scholar_files[V1 %like% "RECN" & V1 %like% syncFileDate,])
dt_scholar_recn     <- fread(scholar_recn_url)
# -----------------------------------------------------------------------------
