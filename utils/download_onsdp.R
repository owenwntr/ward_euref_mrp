
onspd_url <- "https://www.arcgis.com/sharing/rest/content/items/b8920bf40db14e04a59c331d1663d26e/data"

temp <- tempfile()

download.file(onspd_url, temp)

unzip(temp,exdir="oa_data/data/onspd")
