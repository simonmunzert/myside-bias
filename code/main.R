# load packages, functions, and helper objects --------

setwd("../") # set working directory to project root
source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")


webshot_png_export <- TRUE  # set to TRUE to export gt tables as PNG (requires Chrome)


# run all main analysis scripts ---------------------------

source("code/03a-describe-sample.R")
source("code/03b-describe-survey-covars.R")
source("code/03c-describe-vignettes.R")
source("code/03d-describe-vignette-outcomes.R")
source("code/04a-analyze-main-vignettes.R")
source("code/04b-analyze-hypotheses-1-3-ingroup-outgroup.R")
source("code/04c-analyze-hypotheses-4-5-framing.R")
source("code/04d-analyze-hypotheses-6-exposure.R")

# revision-stage robustness and reviewer-response analyses
source("code/05-analyze-reviewer-checks.R")
source("code/06-subgroup-omnibus.R") 




  




