library(httr)
library(jsonlite)
library(tidyverse)
library(data.tree)

# Setup -------------------------------------------------------------------
base <- "https://di.unfccc.int/api"

variables <- paste0(base, "/variables/fq/")
parties <- paste0(base, "/parties")

conversion <- paste0(base, "/conversion")
fq <- paste0(conversion, "/fq")

groups <- paste0(base, "/years/groups")
single <- paste0(base, "/years/single")

gas <- paste0(base, "/dimension-instances/gas")
category <- paste0(base, "/dimension-instances/category")
classification <- paste0(base, "/dimension-instances/classification")
measures <- paste0(base, "/dimension-instances/measure")

# Functions ---------------------------------------------------------------
get_parties <- function(annex) {
  base_url <- "https://di.unfccc.int/api/parties/"
  party_url <- paste0(base_url, annex)
  r <- GET(party_url)
  c <- content(r, "text", encoding = "utf-8")
  df <-
    fromJSON(c, flatten = TRUE) %>%
    as.data.frame() %>%
    dplyr::filter(!name == "Groups") %>%
    select(-any_of(c("categoryCode", "partyIds"))) %>%
    rename(group = name) %>%
    unnest(parties) %>%
    select(-any_of(c("noData")))

  return(df)
}

annex1_parties <- get_parties("annexOne")
nonex1_parties <- get_parties("nonAnnexOne")

get_years <- function(url = single) {
  r <- GET(url)
  c <- content(r, "text", encoding = "utf-8")
  df <-
    fromJSON(c)["annexOne"] %>%
    as.data.frame() %>%
    rename(id = annexOne.id,
           name = annexOne.name)

  return(df)
}

allyears <- get_years()

get_vars <- function(annex, url = variables) {
  r <- GET(paste0(url, annex, "?"))
  c <- content(r, "text", encoding = "utf-8")
  df <-
    fromJSON(c) %>%
    as.data.frame()

  return(df)
}

annex1_vars <- get_vars("annexOne")
nonex1_vars <- get_vars("nonAnnexOne")

get_cats <- function(annex, url = category) {
  r <- GET(url)
  c <- content(r, "text", encoding = "utf-8")
  df <-
    fromJSON(c, flatten = TRUE)[[annex]] %>%
    map_if(is.data.frame, list) %>%
    as_tibble()

  while (typeof(df$children) == "list") {
    df <-
      df %>%
      unnest(children, names_repair = "universal", keep_empty = TRUE)
  }

  df <-
    df %>%
    select(-starts_with("hide"))

  df_list <-
    seq(2, ncol(df), 2) %>%
    map(~ select(df, (.-1):.)) %>%
    bind_rows() %>%
    distinct()

  return(df_list)
}

annex1_category <- get_cats("annexOne")
nonex1_category <- get_cats("nonAnnexOne")

get_cats_tree <- function(annex, url = category) {
  r <- GET(url)
  c <- content(r)
  t <- FromListExplicit(c[[annex]][[1]])

  return(t)
}

annex1_tree <- get_cats_tree("annexOne")
nonex1_tree <- get_cats_tree("nonAnnexOne")

get_others <- function(annex, url) {
  r <- GET(url)
  c <- content(r, "text", encoding = "utf-8")
  df <-
    fromJSON(c)[annex] %>%
    as.data.frame() %>%
    unnest(contains("children"))

  return(df)
}

allunits <- get_others("units", fq)

annex1_measures <- select(get_others("annexOne", measures), c(id, name))
nonex1_measures <- select(get_others("nonAnnexOne", measures), c(id, name))

annex1_gas <- get_others("annexOne", gas)
nonex1_gas <- get_others("nonAnnexOne", gas)

annex1_class <- get_others("annexOne", classification)
nonex1_class <- get_others("nonAnnexOne", classification)

annex1_primary <-
  annex1_vars %>%
  left_join(rename(annex1_category, categoryName = name), by = c("categoryId" = "id")) %>%
  left_join(rename(annex1_class, classificationName = annexOne.name), by = c("classificationId" = "annexOne.id")) %>%
  left_join(rename(annex1_measures, measureName = name), by = c("measureId" = "id")) %>%
  left_join(rename(annex1_gas, gasName = annexOne.name), by = c("gasId" = "annexOne.id")) %>%
  left_join(rename(allunits, unitName = units.name), by = c("unitId" = "units.id")) %>%
  select(sort(colnames(.)))

nonex1_primary <-
  nonex1_vars %>%
  left_join(rename(nonex1_category, categoryName = name), by = c("categoryId" = "id")) %>%
  left_join(rename(nonex1_class, classificationName = nonAnnexOne.name), by = c("classificationId" = "nonAnnexOne.id")) %>%
  left_join(rename(nonex1_measures, measureName = name), by = c("measureId" = "id")) %>%
  left_join(rename(nonex1_gas, gasName = nonAnnexOne.name), by = c("gasId" = "nonAnnexOne.id")) %>%
  left_join(rename(allunits, unitName = units.name), by = c("unitId" = "units.id")) %>%
  select(sort(colnames(.)))

flexible_query <- function(varids, partyids, yearids) {
  base_url <- "https://di.unfccc.int/api/records/flexible-queries"
  query_body <- list("variableIds" = varids,
                     "partyIds" = partyids,
                     "yearIds" = yearids)
  query_body <- toJSON(query_body)
  r <- POST(base_url, body = query_body, encode = "raw",
                   content_type_json())
  c <- content(r, "text", encoding = "utf-8")
  df <- fromJSON(c)

  return(df)
}

annex1_name <-
  annex1_primary %>%
  mutate(annex = "A1")

nonex1_name <-
  nonex1_primary %>%
  mutate(annex = "NA1")

all_name <-
  rbind(annex1_name, nonex1_name) %>%
  select(ends_with("Name"), variableId, annex)
