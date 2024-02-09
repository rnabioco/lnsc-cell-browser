library(markdown)
library(yaml)
library(tidyverse)
library(glue)
library(here)
library(cli)

# Directories
pub_info  <- read_yaml("pubs.yml")
athr_info <- read_yaml("authors.yml")
pub_dir   <- here("pubs")
atlas_dir <- here("atlases")
img_dir   <- here("images")
template  <- "jolla"

if (!dir.exists(pub_dir))   dir.create(pub_dir)
if (!dir.exists(atlas_dir)) dir.create(atlas_dir)

# Info for each link
link_info <- list(
  publication = list(text = "Publication", icon = "file-earmark"),
  atlas       = list(text = "Atlas", icon = "compass"),
  github      = list(text = "GitHub", icon = "github"),
  geo         = list(text = "NCBI GEO")
)

# Template to write index.qmd
index_qmd <- "---\ntitle: {ttl}\n"

# Create index.qmd for each pub entry
pub_info %>%
  iwalk(~ {
    info  <- .x
    links <- names(info)
    links <- links[links %in% names(link_info)]
    links <- link_info[links]
    pub   <- here(pub_dir, .y)
    ttl   <- info$title
    cats  <- str_c(info$categories, collapse = ", ")
    
    # Create pub directory
    if (!dir.exists(pub)) dir.create(pub)
    
    # Pull image
    # image name must match pub directory name
    # conditionally add image field
    img <- dir(img_dir, str_c("^", .y, "\\."), full.names = TRUE)
    
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
      "categories: [{cats}]\n",
      "about:\n",
      "  id: about\n",
      "  template: {template}\n",
      "  links:\n"
    )
    
    # Parse author link info
    athrs <- info$authors %>%
      map_chr(~ {
        lnk <- athr_info[[.x]]$publications
        
        if (!is.null(lnk)) .x <- str_c("[", .x, "](", lnk, ")")
        .x
      }) %>%
      str_c(collapse = ", ")

    # Parse pub link info
    links <- links %>%
      imap(~ {
        vals <- list()
        if (!is.null(.x$text))    vals$text <- str_c("  - text: ", .x$text)
        if (!is.null(.x$icon))    vals$icon <- str_c("    icon: ", .x$icon)
        if (!is.null(info[[.y]])) vals$href <- str_c("    href: ", info[[.y]])
        
        str_c(str_c(vals, collapse = "\n"), "\n")
      }) %>%
      reduce(str_c)
    
    # Write final index.qmd
    index_qmd <- glue(
      index_qmd, links, "---\n\n",
      athrs, "\n\n",
      info$abstract
    )
    
    # Copy files for atlases
    # symbolic links do not seem to work
    atlas_lnk <- here(atlas_dir, .y)
    
    if (!is.null(info$atlas) && !file.exists(atlas_lnk)) {
      file.copy(pub, atlas_dir, recursive = TRUE)
    }
    
    write_lines(index_qmd, here(pub, "index.qmd"))
  })
