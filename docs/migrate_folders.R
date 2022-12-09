# -----------------------------------------------------------------------------
if (!require(devtools)) install.packages("devtools")
# -----------------------------------------------------------------------------
## Step 00.02.b Install data.table # latest development version:            ###
# -----------------------------------------------------------------------------
install.packages("data.table")
data.table::update.dev.pkg()
# -----------------------------------------------------------------------------
folder_root             <- "C:/Users/glen.falk/OneDrive - IHS Markit/Documents"
folder_github           <- "/github"
folder_new              <- c("/cheatsheet/", "/dashboard/", "/rds/")
folder_project_template <- paste0(folder_root,folder_github, "/volatility")
folder_target           <- getwd()
# -----------------------------------------------------------------------------
dt_project_template     <- data.table::as.data.table(folder_project_template)
dt_target               <- data.table::as.data.table(folder_target)
# -----------------------------------------------------------------------------
sub_folder              <- c("/config/", "/cache/",
                             "/data/", "/diagnostics/",
                             "/docs/", "/graphs/",
                             "/lib/", "/logs/",
                             "/munge/", "/profiling/", "/renv/", "/repo/",
                             "/reports", "/src/")
# -----------------------------------------------------------------------------
dt_folder_new           <- data.table::as.data.table(matrix(folder_new))
dt_subfolder            <- data.table::as.data.table(matrix(sub_folder))
# -----------------------------------------------------------------------------
dt_folder_new           <- dt_target[,as.list(dt_folder_new), by = dt_target]
# -----------------------------------------------------------------------------
dt_folder_copy_from     <- dt_project_template[,as.list(dt_subfolder), by = dt_project_template]
dt_folder_copy_from     <- dt_folder_copy_from[,path_from:= paste0(folder_project_template,V1)][,3]
# -----------------------------------------------------------------------------
# Step 00.03.b cartesian join                      https://tinyurl.com/ysy8jpen
# -----------------------------------------------------------------------------
dt_folder_copy_to       <- dt_target[,as.list(dt_subfolder), by=dt_target]
# -----------------------------------------------------------------------------
# Step 00.03.c combine two columns of a data.table https://tinyurl.com/a9jv99cf
# -----------------------------------------------------------------------------
dt_folder_new           <- dt_folder_new[,path:= paste0(folder_target,V1)][,3]
dt_folder_copy_to       <- dt_folder_copy_to[,path_to:= paste0(folder_target,V1)][,3]
# -----------------------------------------------------------------------------
dt_folder_copy          <- cbind.data.frame(dt_folder_copy_from, dt_folder_copy_to)
# -----------------------------------------------------------------------------
# Step 00.03.d copy the files to the new folder
# -----------------------------------------------------------------------------
# mapply(dir.create, paths = dt_folder_new[,1], showWarnings = TRUE,
#        recursive = FALSE, mode = "0777")
# -----------------------------------------------------------------------------
mapply(file.copy, from=dt_folder_copy[,1], to=dt_target,
       overwrite = TRUE, recursive = TRUE)