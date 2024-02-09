library(markdown)
library(yaml)
library(tidyverse)
library(glue)
library(here)
library(cli)

# Directories
proj_info <- read_yaml("projects.yml")
athr_info <- read_yaml("authors.yml")
proj_dir  <- here("projects")
img_dir   <- here("images")
template  <- "jolla"

# Info for each link
link_info <- list(
  publication = list(text = "Publication", icon = "file-earmark"),
  cellbrowser = list(text = "Cellbrowser", icon = "compass"),
  github      = list(text = "GitHub", icon = "github"),
  geo         = list(text = "NCBI GEO")
)

# Template to write index.qmd
index_qmd <- "---\ntitle: {ttl}\n"

# Create index.qmd for each project entry
proj_info %>%
  iwalk(~ {
    info  <- .x
    links <- names(info)
    links <- links[links %in% names(link_info)]
    links <- link_info[links]
    proj  <- here(proj_dir, .y)
    ttl   <- info$title
    
    # Create project directory
    if (!dir.exists(proj)) dir.create(proj)
    
    # Pull image
    # image name must match project directory name
    # conditionally add image field
    img <- dir(img_dir, str_c("^", .y, "\\."), full.names = TRUE)
    
    if (length(img) > 1) {
      cli_warn("Multiple images found, using {img[[1]]}")
      
      img <- img[[1]]
    }
    
    img_nm  <- basename(img)
    img_lnk <- here(proj, img_nm)
    
    if (length(img_lnk) > 0) {
      index_qmd <- str_c(index_qmd, "image: {img_nm}\n")
      
      if (!file.exists(img_lnk)) file.link(img, img_lnk)
    }
    
    index_qmd <- str_c(
      index_qmd,
      "about:\n",
      "  id: about\n",
      "  template: {template}\n",
      "  links:\n"
    )
    
    # Parse author link info
    athrs <- info$authors %>%
      map_chr(~ {
        if (.x %in% names(athr_info)) {
          .x <- str_c("[", .x, "](", athr_info[.x], ")")
        }
        .x
      }) %>%
      str_c(collapse = ", ")

    # Parse project link info
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
    
    write_lines(index_qmd, here(proj, "index.qmd"))
  })
