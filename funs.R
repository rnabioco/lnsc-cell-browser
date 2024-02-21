#' Create links for a project
#' 
#' links to atlases, publications, investigators
.create_project_links <- function(proj_yml, athr_yml, pub_yml, lnk_cls) {
  
  # Identify publications and atlases listed for each project
  # only want to include links if there is something to link to
  res <- proj_yml %>%
    imap(~ {
      proj <- .y
      
      # Links to investigators page
      athrs <- athr_yml %>%
        keep(~ is.list(.x) && proj %in% .x$project) %>%
        names()
      
      lnks <- athrs %>%
        map(~ {
          nm <- str_c(.x, ", PhD")
          
          nm_lnk <- nm %>%
            str_to_lower() %>%
            str_replace_all("( |, )", "-")
          
          str_c("[", nm, "](investigators.qmd#", nm_lnk, ")", lnk_cls)
        }) %>%
        str_c(collapse = " ")
      
      # Link to publications
      pubs <- pub_yml %>%
        map_lgl(~ proj %in% .x$project)
        
      if (any(pubs)) {
        lnks <- str_c(
          lnks, " [Publications](pubs.qmd#category=", str_to_title(proj), ")",
          lnk_cls
        )
      }
      
      # Link to atlases
      atlases <- pub_yml[pubs] %>%
        map_lgl(~ !is.null(.x$links$atlas))
      
      if (any(atlases)) {
        lnks <- str_c(
          lnks, " [Atlases](atlases.qmd#category=", proj, ")",
          lnk_cls
        )
      }
      
      lnks
    })
  
  res
}

#' Create project link
#' 
#' links to the project page
.create_project_link <- function(proj_title, lnk_name = proj_title,
                                 lnk_cls = "{.bold-link}", max_len = 64) {
  res <- proj_title %>%
    str_to_lower() %>%
    str_replace_all("( |, )", "-") %>%
    str_trunc(max_len, "right", ellipsis = "") %>%
    str_remove("-$") %>%
    str_c("[", lnk_name, "](projects.qmd#", ., ")", lnk_cls)
  
  res
}

#' Scrape pubmed for publication info
.scrape_pubmed <- function(url, attrs = NULL) {
  html <- read_html(url)
  
  # Pull PMID from url
  pmid <- url %>%
    str_extract("[0-9]+(/|)$") %>%
    str_remove("/$") %>%
    str_c("PMID ", .)
  
  # Scape author list
  # modify to only include middle initials
  athrs <- html %>%
    html_elements("a.full-name") %>%
    map_chr(html_text2) %>%
    unique()
  
  athrs <- athrs %>%
    str_split(" ") %>%
    map_chr(.format_name)
  
  # Scrap title and abstract
  ttl <- html %>%
    html_element("h1.heading-title") %>%
    html_text2()
  
  abst <- html %>%
    html_elements("p") %>%
    map_chr(html_text2) %>%
    pluck(4)
  
  # Scrape image link
  img <- html %>%
    html_element("a.figure-link") %>%
    html_attr("href")
  
  # Set publication key based on first author and key
  # this is used to create page directories
  date <- html %>%
    html_element("span.cit") %>%
    html_text2() %>%
    str_extract("^[^;]+")
  
  if (is.na(date)) cli_abort("Error extracting date from pubmed link")
  
  date <- date %>%
    as.Date(format = "%Y %b %d")
  
  yr <- year(date)
  
  date <- date %>%
    format("%B %d, %Y")
  
  athr1 <- athrs[1] %>%
    str_extract("[^ ]+$") %>%
    str_to_lower()
  
  key <- str_c(yr, "-", athr1)
  
  # Return publication info
  res <- list(
    key      = key,
    title    = ttl,
    pmid     = pmid,
    date     = date,
    year     = yr,
    authors  = athrs,
    abstract = abst
  )
  
  if (!is.na(img)) res$image <- img
  
  if (!is.null(attrs)) {
    attrs <- attrs[attrs %in% names(res)]
    res   <- res[attrs]
  }
  
  res
}

#' Format author names
.format_name <- function(nm) {
  frst <- first(nm)
  lst  <- last(nm)
  
  mid <- nm[!nm %in% c(frst, lst)]
  
  mid <- mid %>%
    str_extract("^[a-zA-Z]")
  
  res <- c(frst, lst) %>%
    str_c(collapse = " ")
  
  res
}