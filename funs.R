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
          lnks, " [Publications](pubs.qmd#category=", proj, ")",
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

.scrape_pubmed <- function(url) {
  html <- read_html(url)
  
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
    html_elements("h1.heading-title") %>%
    map_chr(html_text2) %>%
    pluck(1)
  
  abst <- html %>%
    html_elements("p") %>%
    map_chr(html_text2) %>%
    pluck(4)
  
  # Set publication key based on first author and key
  # this is used to create page directories
  yr <- html %>%
    html_elements("span.cit") %>%
    pluck(1) %>%
    html_text2() %>%
    str_extract("^[0-9]{4}(?= )")
  
  if (is.na(yr)) cli_abort("Error extracting year from pubmed link")
  
  athr1 <- athrs[1] %>%
    str_extract("[^ ]+$") %>%
    str_to_lower()
  
  key <- str_c(yr, "-", athr1)
  
  # Return publication info
  res <- list(
    key      = key,
    title    = ttl,
    year     = yr,
    authors  = athrs,
    abstract = abst
  )
  
  res
}

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