library(tidyverse)
library(leaflet)

url <- "https://public.tableau.com/app/profile/oma.era/viz/OHTListv1/Ontario"

site <- httr::GET(url,
                  httr::add_headers(embed = "y",
                              showAppBanner = "false",
                              showShareOptions = "true",
                              display_count = "no",
                              showVizHome = "no"))

site %>%
  httr::content() %>%
  as.character() %>%
  stringr::str_detect("id")

url <- "https://public.tableau.com/vizql/w/OHTListv1/v/Ontario/bootstrapSession/sessions/2C48DF8793E149E2B278BB22CD1BA8CC-0:0"
  #"https://public.tableau.com/vizql/w/OHTListv1/v/Ontario/bootstrapSession/sessions/75AA0706205B435BB2160DF0447F855E-0:0"

t <- httr::POST(url,
                body = list(sheet_id = 1))

test <- httr::content(t, as="text", encoding = "UTF-8")

write_file(test, "test_tableau.tab")

str(test)

test_json <- jsonlite::fromJSON(test)


write.csv(test, file = "test.txt")

test2 <- readr::read_file("test2.txt")

test3 <- stringr::str_replace_all(test2, '""', '"')

test4 <- stringr::str_remove_all(test3, "71878;.*")
test_json <- jsonlite::fromJSON(test4)

test_tib <- test_json %>% as_tibble()

t <- as_tibble(test_tib$worldUpdate)$applicationPresModel

str(t)
z <- t$workbookPresModel$dashboardPresModel

str(z$zones$`4`)
str(z$zones$`15`$presModelHolder)


str(z$zones)

str(z) %>% as.character()

str(t$workbookPresModel)


#########

#test_json <- jsonlite::read_json("oht_testing/test2_no_img.txt")

test_json <- jsonlite::read_json("oht_testing/test3.json")$stuff

#tibble::as_tibble(test_json )
coords <- unlist(test_json[[1]]$dataValues)
words <- unlist(test_json[[2]]$dataValues)
w <- tibble(w = words) %>%
  rowid_to_column()


# map lhins???
coords[1:28] %>%
matrix(ncol = 2) %>%
  as_tibble() %>%
  rename(lat = 1,
         lon = 2) %>%
  leaflet() %>% addTiles() %>% addMarkers()

# map OHTs?
coords[29:194] %>%
matrix(ncol = 2) %>%
  as_tibble() %>%
  rename(lat = 1,
         lon = 2) %>%
  leaflet() %>% addTiles() %>% addMarkers(label = ~ paste0(lat,", ", lon))

# map first OHT
coords[29:194] %>%
  matrix(ncol = 2) %>%
  as_tibble() %>%
  slice_head(n=1) %>%
  rename(lat = 1,
         lon = 2) %>%
  leaflet() %>% addTiles() %>% addMarkers(label = ~ paste0(lat,", ", lon))



t <- w[624:(624+83),]


# map lhins??
words[15:42] %>%
  as.numeric() %>%
  matrix(ncol = 2) %>%
  as_tibble() %>%
  rename(lat = 1,
         lon = 2) %>%
  leaflet() %>%addTiles() %>% addMarkers()

words[43:199] %>%
  as.numeric() %>%
  matrix(ncol = 2) %>%
  as_tibble() %>%
  rename(lat = 1,
         lon = 2) %>%
  leaflet() %>%addTiles() %>% addMarkers()

z <- words[633:836]
zz <- z %>%
  matrix(ncol = 4) %>%
  as_tibble()
