# Source Files ------------------------------------------------------------

source("code/libraries.R")
source("code/utilities.R")
source("code/functions/functions.R")
source("code/modules/module_data.R")
source("code/modules/module_explore.R")
source("code/modules/module_sample.R")
source("code/modules/module_binning.R")
source("code/modules/module_model.R")
source("code/modules/module_validation.R")
source("code/modules/module_download.R")
source("code/server.R")
source("code/ui.R")

# Help Files --------------------------------------------------------------

#qh_files <- c("help/qh_data.Rmd", "help/qh_explore.Rmd",
#             "help/qh_sample.Rmd", "help/qh_binning.Rmd",
#             "help/qh_model.Rmd", "help/qh_validation.Rmd")
#sapply(qh_files, knit, quiet = TRUE)

#h_files <- c("help/h_data.Rmd", "help/h_explore.Rmd", "help/h_sample.Rmd",
#            "help/h_binning.Rmd", "help/h_model.Rmd", "help/h_validation.Rmd",
#            "help/h_download.Rmd")
#sapply(h_files, knit, quiet = TRUE)

# Run Application ---------------------------------------------------------

shinyApp(ui, server)