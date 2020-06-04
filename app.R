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


# Run Application ---------------------------------------------------------

shinyApp(ui, server)