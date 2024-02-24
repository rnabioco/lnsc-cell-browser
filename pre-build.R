# Run this script prior to building site to:
# * Scrape publication information and generate publication pages
# * Update authors.yml based on publication information
# * Search for author images and update authors.yml

library(yaml)
library(tidyverse)
library(here)
library(cli)
library(jsonlite)
library(rvest)

source(here("funs.R"))

# Directories
pub_yml <- read_yaml("pubs.yml")
pub_dir <- "pubs"
img_dir <- here("images")


# PUBLICATIONS ----

# * Scrape missing publication information and update pubs.yml
# * Add missing authors to authors.yml
# * Associate publications with projects based on authors.yml
# * Create directories and write index.qmd page for each publication
# * Write index.qmd page for each atlas

# Check for duplicated publications
dup_pubs <- pub_yml %>%
  map_chr(pluck, "pubmed") %>%
  duplicated()

# Create index.qmd for each pub entry
projs <- names(read_yaml("projects.yml"))

pub_yml_new <- pub_yml[!dup_pubs] %>%
  map(
    .create_pub_page,
    athr_yml_file = "authors.yml",
    out_dir       = here(pub_dir),
    img_dir       = img_dir,
    proj_order    = projs
  )

# Write updated pubs.yml
# this will have info scraped from pubmed
if (!identical(pub_yml, pub_yml_new)) {
  pub_yml_new %>%
    write_yaml(here("pubs.yml"))
}

# Create pages for atlases
atlas_pubs <- pub_yml_new %>%
  keep(~ !is.null(pluck(.x, "links", "atlas"))) %>%
  map(
    .create_pub_page,
    athr_yml_file = "authors.yml",
    out_dir       = here("atlases"),
    img_dir       = img_dir,
    proj_order    = projs,
    atlas         = TRUE
  )


# INVESTIGATORS ----

# Check for image and update authors.yml if image is found
athr_yml  <- read_yaml("authors.yml")

athr_yml_new <- athr_yml %>%
  imap(~ {
    if (is.null(.x$image)) {
      img_nm <- .y %>%
        str_to_lower() %>%
        str_replace_all(" ", "-")
      
      img <- dir(img_dir, str_c("^", img_nm, "\\."), full.names = TRUE)
      
      if (length(img) > 1) img <- img[[1]]
      
      img <- str_remove(img, here())
      
      if (!is_empty(img)) .x$image <- img
      
    # Remove image path if it does not exist
    } else if (!file.exists(str_remove(.x$image, "^/"))) {
      .x$image <- NULL
    }
    
    .x
  })

if (!identical(athr_yml, athr_yml_new)) {
  athr_yml_new %>%
    write_yaml(here("authors.yml"))
}


# TIMELINE ----

# Filter for authors to label
lab_athr <- athr_yml_new %>%
  keep(~ !is.null(.x$project)) %>%
  names()

tl_dat <- pub_yml_new %>%
  map_dfr(~ {
    athrs <- .x$authors
    athrs <- athrs[athrs %in% lab_athr]
    athrs <- str_extract(athrs, "[^ ]+$")
    
    tibble(
      key     = .x$key,
      date    = as.Date(str_c(.x$year, "-01-01")),
      proj    = str_c(str_to_title(.x$project), collapse = ", "),
      authors = str_c(athrs, collapse = ", "),
      pmid    = str_extract(.x$pubmed, "[0-9]+(/|)$")
    )
  })

# Format publication data
tl_dat <- tl_dat %>%
  mutate(
    pmid = str_remove(pmid, "/$"),
    pmid = str_c("PMID", pmid),
    lab  = str_c(proj, "\n", pmid),
    year = as.character(year(date)),
    url  = file.path(pub_dir, key)
  )

# Save as json file
tl_dat %>%
  write_json("timeline.json")
