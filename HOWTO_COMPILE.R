
rm(list=ls())


# 1. Download OA lookups

source("utils/download_onsdp.R", echo=TRUE)

# 2. Create OA-level poststratification frame

source("oa_data/scripts/oa_data_cleaning.R", echo=TRUE)

# 3. Download BES data

# 3(a). Download BES data from UK data service here, in STATA format: https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8810#!/access-data

# 3(b). Fill in BES data path below + run

fc <- file("utils/bes_path.R")

writeLines(c("bes_path <- \" PATH TO .dta FILE IN BES DOWNLOAD\""),
           fc)

writeLines(c("bes_path <- \"C:/Users/Owen Winter/Documents/bes2019_w20_ukdspanel_v0-2.dta\""),
           fc)

close(fc)


# 4. Load BES data and merge OA-level data to match BES-level areas (MSOA:PCON)

source("data_structuring/scripts/bes_units.R", echo=TRUE)

# 5. Load validation and test wards

source("validation/scripts/validation_test_split.R", echo=TRUE)

# 6. Model turnout

source("model/scripts/turnout_model.R",echo=TRUE)

# 7. Scale turnout predictions

source("model/scaling/scripts/turnout_scaling.R",echo=TRUE)

# 8. Vote Model

source("model/scripts/vote_model.R",echo=TRUE)

# 9. Scale vote predictions

source("model/scaling/scripts/vote_scaling.R",echo=TRUE)

# 10. Validate and test predictions

source("validation/scripts/final_validation.R", echo=TRUE)




