library(yaml)
library(tidyverse)
library(glue)
library(here)
library(cli)
library(rvest)

# Directories
pub_yml   <- read_yaml("pubs.yml")
athr_yml  <- read_yaml("authors.yml")
pub_dir   <- here("pubs")
atlas_dir <- here("atlases")
img_dir   <- here("images")
template  <- "jolla"

# Just need this to order categories
projs <- names(read_yaml("projects.yml"))

if (!dir.exists(pub_dir))   dir.create(pub_dir)
if (!dir.exists(atlas_dir)) dir.create(atlas_dir)

# Info for each link
link_info <- list(
  pubmed = list(text = "Publication", icon = "file-earmark"),
  atlas  = list(text = "Atlas", icon = "compass"),
  github = list(text = "GitHub", icon = "github"),
  geo    = list(text = "NCBI GEO")
)

# Template to write index.qmd
index_qmd <- "---\ntitle: {ttl}\n"

# Create index.qmd for each pub entry
pub_yml <- pub_yml %>%
  imap(~ {
    pub_info <- .x
    
    if (is.null(pub_info$title)) {
      pub_info <- .scrape_pubmed(.x$pubmed)
      pub_info <- append(.x, pub_info)
    }
    
    ttl <- pub_info$title
    pub <- here(pub_dir, pub_info$key)
    
    # Set link icons and text
    links <- append(pub_info["pubmed"], pub_info$links)
    
    links <- links %>%
      imap(~ {
        lnk <- list(link = .x)
        
        append(lnk, link_info[[.y]])
      })
    
    # Create pub directory
    if (!dir.exists(pub)) dir.create(pub)
    
    # Pull image
    # image name must match pub directory name
    # conditionally add image field
    img <- dir(img_dir, str_c("^", pub_info$key, "\\."), full.names = TRUE)
    
    if (length(img) > 1) {
      cli_warn("Multiple images found, using {img[[1]]}")
      
      img <- img[[1]]
    }
    
    img_nm  <- basename(img)
    img_lnk <- here(pub, img_nm)
    
    if (length(img_lnk) > 0) {
      index_qmd <- str_c(index_qmd, "image: {img_nm}\n")
      
      if (!file.exists(img_lnk)) file.link(img, img_lnk)
    }
    
    index_qmd <- str_c(
      index_qmd,
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
      str_c(collapse = ", ")
    
    # Parse author link info
    athrs <- pub_info$authors %>%
      map_chr(~ {
        lnk <- athr_yml %>%
          pluck(.x, "publications")
        
        if (!is.null(lnk)) .x <- str_c("[", .x, "](", lnk, ")")
        .x
      }) %>%
      str_c(collapse = ", ")
    
    # Copy files for atlases
    # symbolic links do not seem to work
    atlas_lnk <- here(atlas_dir, pub_info$key)
    
    if (!is.null(links$atlas) && !file.exists(atlas_lnk)) {
      file.copy(pub, atlas_dir, recursive = TRUE)
    }
    
    # Parse provided links for publication
    links <- links %>%
      imap(~ {
        vals <- list()
        if (!is.null(.x$text))    vals$text <- str_c("  - text: ", .x$text)
        if (!is.null(.x$icon))    vals$icon <- str_c("    icon: ", .x$icon)
        if (!is.null(.x$link))    vals$href <- str_c("    href: ", .x$link)
        
        str_c(str_c(vals, collapse = "\n"), "\n")
      }) %>%
      str_c(collapse = "")
    
    # Update authors.yml with new authors from publication
    missing_athrs <- pub_info$authors[!pub_info$authors %in% names(athr_yml)]
    
    athr_yml[missing_athrs] <- NA
    
    athr_yml %>%
      write_yaml(here("authors.yml"))
    
    # Write final index.qmd
    index_qmd <- glue(
      index_qmd, links, "---\n\n",
      athrs, "\n\n",
      pub_info$abstract
    )
    
    write_lines(index_qmd, here(pub, "index.qmd"))
    
    # Update pubs.yml with categories derived from the authors
    # return updated pub_yml
    pub_info$project <- cats
    
    pub_info
  })

# Write updated pubs.yml
# this will have info scraped from pubmed
pub_yml %>%
  write_yaml(here("pubs.yml"))
