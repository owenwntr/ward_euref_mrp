
rm(list=ls())

# 1. Download OA lookups

source("utils/download_onsdp.R", echo=TRUE)

# 2. Create OA-level poststratification frame

source("oa_data/scripts/oa_data_cleaning.R", echo=TRUE)

# 3. Load BES data and merge OA-level data to match BES-level areas (MSOA:PCON)

source("data_structuring/scripts/bes_units.R", echo=TRUE)

# 4. Load validation and test wards

source("validation/scripts/validation_test_split.R", echo=TRUE)

# 5. Model turnout



