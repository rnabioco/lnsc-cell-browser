#' Create links for a project section
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
      
      proj_lnk <- str_to_title(proj)
        
      if (any(pubs)) {
        lnks <- str_c(
          lnks, " [<i class='bi bi-file-earmark'></i> Publications]",
          "(pubs.qmd#category=", proj_lnk, ")",
          lnk_cls
        )
      }
      
      # Link to atlases
      atlases <- pub_yml[pubs] %>%
        map_lgl(~ !is.null(.x$links$atlas))
      
      if (any(atlases)) {
        lnks <- str_c(
          lnks, " [<i class='bi bi-compass'></i> Atlases]",
          "(atlases.qmd#category=", proj_lnk, ")",
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
                                 lnk_cls = "{.link-bold}", max_len = 40,
                                 return_id = FALSE) {
  res <- proj_title %>%
    str_to_lower() %>%
    str_replace_all("( |, )", "-") %>%
    str_trunc(max_len, "right", ellipsis = "") %>%
    str_remove("-$")
  
  if (return_id) return(res)
  
  res <- res %>%
    str_c("[", lnk_name, "](projects.qmd#", ., ")", lnk_cls)
  
  res
}

#' Add icon to link
.icon_key <- list(
  publications = "bi bi-file-earmark",
  atlases      = "bi bi-compass",
  atlas        = "bi bi-compass",
  email        = "bi bi-envelope",
  website      = "bi bi-house"
  # website      = "bi bi-globe"
)

.add_link_icon <- function(lnk_text, icon_key = .icon_key) {
  
  icon <- .icon_key[[lnk_text]]
  
  if (!is.null(icon)) icon <- str_c("<i class='", icon, "'></i> ")
  
  res <- lnk_text %>%
    str_to_title() %>%
    str_c("[", icon, ., "]")

  res  
}

#' Create publication page
#' 
#' * Check publication information and scrape missing information from pubmed
#' * Update authors.yml with any missing authors
#' * Associate projects with publications based on authors, projects are used as
#'   categories for the page
#' * Write index.qmd page for publication
#' 
#' @param pub_info named list with publication information
#' @param athr_yml_file name of yml file to load containing author information,
#' this file will be updated with any missing authors
#' @param out_dir directory to save publication page
#' @param template name of quarto about page template to use
#' @param img_dir directory to save images
#' @param pdf_dir directory to save pdfs
#' @param proj_order order of projects, used to order page categories
#' @param atlas should page links be configured for an atlas page, e.g. put
#' atlas link at the top of the page instead of the bottom
#' @param overwrite scrape publication information and overwrite any existing
#' information
#' @return named list with updated publication information
.create_pub_page <- function(pub_info, athr_yml_file = "authors.yml",
                             out_dir = "pubs", template = "jolla",
                             img_dir = "images", pdf_dir = "docs",
                             proj_order = NULL, atlas = FALSE,
                             overwrite = FALSE) {
  
  # Create directory to save publication page
  if (!dir.exists(out_dir)) dir.create(out_dir)
  
  # Read authors.yml within loop since author list gets updated for each paper
  athr_yml <- read_yaml(athr_yml_file)
  
  # Scrape missing info from pubmed
  pub_attrs <- c(
    "key", "title", "pmid", "date", "year",
    "authors", "abstract", "image", "pdf"
  )
  
  if (any(!pub_attrs %in% names(pub_info)) || overwrite) {
    if (!overwrite) pub_attrs <- pub_attrs[!pub_attrs %in% names(pub_info)]

    new_info <- .scrape_pubmed(pub_info$pubmed, pub_attrs, pub_info$fig_number)
    pub_info <- append(pub_info[!names(pub_info) %in% pub_attrs], new_info)
  }
  
  # Download pdf
  # path must be relative to site
  pdf <- pub_info$pdf
  
  if (!is.null(pdf) && grepl("\\.pdf$", pdf)) {
    pdf_dwnld <- pub_info$pdf
    pdf       <- here(pdf_dir, str_c(pub_info$key, ".pdf"))
    
    if (!file.exists(pdf)) download.file(pdf_dwnld, pdf)
    
    pdf <- str_remove(pdf, here())
  }

  # Set link icons and text
  link_info <- list(
    pubmed = list(text = "Pubmed",   icon = "file-earmark"),
    pdf    = list(text = "PDF",      icon = "filetype-pdf"),
    atlas  = list(text = "Atlas",    icon = "compass"),
    github = list(text = "GitHub",   icon = "github"),
    geo    = list(text = "NCBI GEO")
  )
  
  links     <- pub_info["pubmed"]
  links$pdf <- pdf
  links     <- append(links, pub_info$links)
  
  subttl <- pub_info$pmid
  
  if (atlas) {
    if (!"atlas" %in% names(links)) {
      cli_warn("Atlas link not found in publication info, skipping...")
      
      return(NULL)
    }
    
    subttl <- links %>%   # use double quotes single subtitle will be wrapped
      pluck("atlas") %>%  # in single quotes
      str_c(.add_link_icon("atlas"), "(", ., "){.link-box-large}") %>%
      str_replace_all("'", "\\\"")
    
    links <- links[names(links) != "atlas"]
  }
  
  links <- links %>%
    imap(~ {
      lnk <- list(link = .x)
      
      append(lnk, link_info[[.y]])
    })
  
  # Create pub subdirectory
  pub <- here(out_dir, pub_info$key)
  
  if (!dir.exists(pub)) dir.create(pub)
  
  # Pull image
  # * If no image field in pubs.yml, search for image with matching name
  # * If image field, check if file exists
  # * If file doesn't exist, treat string as URL and try to download
  if (is.null(pub_info$image)) {
    img <- dir(img_dir, str_c("^", pub_info$key, "\\."), full.names = TRUE)
    
    if (length(img) > 1) img <- img[[1]]
    
  } else if (file.exists(str_remove(pub_info$image, "^/"))) {
    img <- pub_info$image
    
  } else {
    img_dwnld <- pub_info$image
    ext       <- str_extract(img_dwnld, "\\.[a-zA-Z]+$")
    img       <- here(img_dir, str_c(pub_info$key, ext))
    
    download.file(img_dwnld, img)
  }
  
  img <- str_remove(img, here())  # image path should be relative to site
  
  # Conditionally add image field to index.qmd if image was found
  index_qmd <- "---\ntitle: {pub_info$title}\n\n"
  
  if (!is_empty(img)) {
    index_qmd <- str_c(index_qmd, "image: {img}\n")
    
    pub_info$image <- img
  }
  
  index_qmd <- str_c(
    index_qmd,
    "subtitle: '{subttl}'\n",
    "date: '{pub_info$date}'\n",
    "categories: [{cats_str}]\n",
    "about:\n",
    "  id: about\n",
    "  template: {template}\n",
    "  links:\n"
  )
  
  cats <- athr_yml[pub_info$authors] %>%
    map(pluck, "project") %>%
    discard(is.null) %>%
    reduce(c)
  
  cats <- c(
    intersect(projs, cats),
    sort(setdiff(cats, projs))
  )
  
  cats_str <- cats %>%
    str_to_title() %>%
    c(pub_info$year) %>%
    str_c(collapse = ", ")
  
  # Parse author link info
  athrs <- pub_info$authors
  
  athrs_str <- athrs %>%
    map_chr(~ {
      lnk <- athr_yml %>%
        pluck(.x, "links", "publications")
      
      if (!is.null(lnk)) .x <- str_c("[", .x, "](", lnk, ")")
      .x
    }) %>%
    str_c(collapse = ", ")
  
  # Parse publication links from pubs.yml
  lnk_str <- links %>%
    imap(~ {
      vals <- list()
      if (!is.null(.x$text)) vals$text <- str_c("  - text: ", .x$text)
      if (!is.null(.x$icon)) vals$icon <- str_c("    icon: ", .x$icon)
      if (!is.null(.x$link)) vals$href <- str_c("    href: ", .x$link)
      
      str_c(str_c(vals, collapse = "\n"), "\n")
    }) %>%
    str_c(collapse = "")
  
  # Update authors.yml with new authors from publication
  missing_athrs <- athrs[!athrs %in% names(athr_yml)]
  
  if (!is_empty(missing_athrs)) {
    athr_yml[missing_athrs] <- list(list())
    
    athr_yml %>%
      write_yaml(here("authors.yml"))
  }
  
  # Write final index.qmd
  index_qmd <- str_glue(
    index_qmd, lnk_str, "---\n\n",
    athrs_str, "\n\n",
    pub_info$abstract
  )
  
  write_lines(index_qmd, here(pub, "index.qmd"))
  
  # Update pubs.yml with categories derived from the authors
  # return updated pub_yml
  pub_info$project <- cats
  
  pub_info
}

#' Scrape pubmed for publication info
.scrape_pubmed <- function(url, attrs = NULL, fig_number = NULL) {
  html <- read_html(url)
  
  # Pull PMID from url
  pmid <- url %>%
    str_extract("[0-9]+(/|)$") %>%
    str_remove("/$")
  
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
  
  # Scrape pdf link
  pdf <- html %>%
    html_element("a.id-link") %>%
    html_attr("href")
  
  base_url <- pdf %>%
    str_remove("/[^\\.]+$")
  
  pdf <- pdf %>%
    read_html() %>%
    html_element("a.int-view") %>%
    html_attr("href") %>%
    str_c(base_url, .)
  
  # Scrape image link
  fig_number <- fig_number %||% 1
  
  img <- html %>%
    html_elements("a.figure-link") %>%
    pluck(fig_number)
  
  if (!is.null(img)) img <- html_attr(img, "href")
  
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
  
  key <- str_c(yr, "-", athr1, "-", pmid)
  
  # Return publication info
  res <- list(
    key      = key,
    title    = ttl,
    pmid     = str_c("PMID ", pmid),
    date     = date,
    year     = as.character(yr),
    authors  = athrs,
    abstract = abst
  )
  
  if (!is.na(pdf))   res$pdf <- pdf
  if (!is.null(img)) res$image <- img
  
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