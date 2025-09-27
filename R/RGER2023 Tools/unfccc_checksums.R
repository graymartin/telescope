# Functions ---------------------------------------------------------------
get_level <- function(categoryname, groupname) {
  if (str_detect(groupname, "Non")) {
    tree <- nonex1_tree
  } else {
    tree <- annex1_tree
  }

  node <- FindNode(tree, categoryname)
  lev <- node$level

  return(lev)
}

get_levels <- function(categorylist, grouplist) {
  cg <- data.frame(unlist(categorylist), unlist(grouplist))
  names(cg) <- c("c", "g")
  cgu <- distinct(cg)

  levellist <- list()
  for (i in seq_len(nrow(cgu))) {
    r <- cgu[i, ]
    levellist[[i]] <- get_level(r$c, r$g)
  }

  cgu$l <- levellist
  cg <-
    cg %>%
    left_join(cgu, by = c("c", "g"))

  return(cg[, "l"])
}

get_parent <- function(categoryname, groupname) {
  if (str_detect(groupname, "Non")) {
    tree <- nonex1_tree
  } else {
    tree <- annex1_tree
  }

  node <- FindNode(tree, categoryname)
  parentnode <- node$parent
  parentname <- parentnode$name

  return(parentname)
}

get_parents <- function(categorylist, grouplist) {
  cg <- data.frame(unlist(categorylist), unlist(grouplist))
  names(cg) <- c("c", "g")
  cgu <- distinct(cg)

  parentlist <- list()
  for (i in seq_len(nrow(cgu))) {
    r <- cgu[i, ]
    parentlist[[i]] <- get_parent(r$c, r$g)
  }

  cgu$l <- parentlist
  cg <-
    cg %>%
    left_join(cgu, by = c("c", "g"))

  return(cg[, "l"])
}


# Checksums ---------------------------------------------------------------
checksum_levels <- function(dfin, level) {
  df <-
    dfin %>%
    mutate(category = sub(" ", "  ", category)) %>% 
    dplyr::filter(grepl("^[[:digit:]]+", category)) %>%
    mutate(level = get_levels(category, group)) %>%
    mutate(parent_category = get_parents(category, group))

  df_levels <- sort(unlist(unique(df$level)))

  nl <- level
  pl <- nl - 1

  dfp <-
    df %>%
    dplyr::filter(level %in% c(pl)) %>%
    group_by(group, category, gas, unit) %>%
    summarize(category_total = sum(value, na.rm = FALSE)) %>%
    mutate(category_total = unlist(category_total)) %>%
    mutate(category_total = if_else(str_starts(unit, "t"), category_total/1000, category_total)) %>%
    mutate(unit = if_else(unit == "t CO2e", "kt CO2e", unit)) %>%
    mutate(unit = if_else(unit == "t", "kt", unit))

  dfn <-
    df %>%
    dplyr::filter(level %in% c(nl)) %>%
    group_by(group, parent_category, gas, unit) %>%
    summarize(subcategory_total = sum(value, na.rm = FALSE)) %>%
    ungroup() %>%
    rename("category" = "parent_category") %>%
    mutate(category = unlist(category)) %>%
    mutate(subcategory_total = unlist(subcategory_total)) %>%
    mutate(subcategory_total = if_else(str_starts(unit, "t"), subcategory_total/1000, subcategory_total)) %>%
    mutate(unit = if_else(unit == "t CO2e", "kt CO2e", unit)) %>%
    mutate(unit = if_else(unit == "t", "kt", unit)) %>%
    group_by(group, category, gas, unit) %>%
    summarize(subcategory_total = sum(subcategory_total, na.rm = FALSE))

  dfpn <-
    dfp %>%
    full_join(dfn)
}

if (FALSE) {
  df <-
    UNFCCC_data_proc %>%
    dplyr::filter(country == "New Zealand") %>% 
    dplyr::filter(year == 2010) %>% 
    dplyr::filter(str_detect(category, "^1")) %>% 
    mutate(category = sub(" ", "  ", category)) %>% 
    dplyr::filter(grepl("^[[:digit:]]+", category)) %>%
    mutate(level = get_levels(category, group)) %>%
    mutate(parent_category = get_parents(category, group)) %>%
    mutate(units = gsub("CO₂ equivalent", "CO2e", units)) %>%
    rename("unit" = "units")
  
  df_levels <- sort(unlist(unique(df$level)))
  
  nl <- 4
  pl <- nl - 1
  
  dfp <-
    df %>%
    dplyr::filter(level %in% c(pl)) %>%
    group_by(group, category, gas, unit) %>%
    summarize(category_total = sum(value, na.rm = FALSE)) %>%
    mutate(category_total = unlist(category_total)) %>%
    mutate(category_total = if_else(str_starts(unit, "t"), category_total/1000, category_total)) %>%
    mutate(unit = if_else(unit == "t CO2e", "kt CO2e", unit)) %>%
    mutate(unit = if_else(unit == "t", "kt", unit))
  
  dfn <-
    df %>%
    dplyr::filter(level %in% c(nl)) %>%
    group_by(group, parent_category, gas, unit) %>%
    summarize(subcategory_total = sum(value, na.rm = FALSE)) %>%
    ungroup() %>%
    rename("category" = "parent_category") %>%
    mutate(category = unlist(category)) %>%
    mutate(subcategory_total = unlist(subcategory_total)) %>%
    mutate(subcategory_total = if_else(str_starts(unit, "t"), subcategory_total/1000, subcategory_total)) %>%
    mutate(unit = if_else(unit == "t CO2e", "kt CO2e", unit)) %>%
    mutate(unit = if_else(unit == "t", "kt", unit)) %>%
    group_by(group, category, gas, unit) %>%
    summarize(subcategory_total = sum(subcategory_total, na.rm = FALSE))
  
  dfpn <-
    dfp %>%
    full_join(dfn)
}
