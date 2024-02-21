# Run this script prior to building site to:
# * Scrape publication information and generate publication pages
# * Update authors.yml based on publication information
# * Search for author images and update authors.yml

library(yaml)
library(tidyverse)
library(here)
library(cli)
library(rvest)

source(here("funs.R"))

# Directories
pub_yml   <- read_yaml("pubs.yml")
pub_dir   <- here("pubs")
atlas_dir <- here("atlases")
img_dir   <- here("images")
template  <- "jolla"

# Just need this to order categories
projs <- names(read_yaml("projects.yml"))

if (!dir.exists(pub_dir))   dir.create(pub_dir)
if (!dir.exists(atlas_dir)) dir.create(atlas_dir)


# PUBLICATIONS ----

# * Scrape missing publication information and update pubs.yml
# * Add missing authors to authors.yml
# * Associate publications with projects based on authors.yml
# * Create directories and write index.qmd page for each publication
# * Write index.qmd page for each atlas

# Link text and icons
link_info <- list(
  pubmed = list(text = "Publication", icon = "file-earmark"),
  atlas  = list(text = "Atlas", icon = "compass"),
  github = list(text = "GitHub", icon = "github"),
  geo    = list(text = "NCBI GEO")
)

# Template to write index.qmd
index_qmd <- "---\ntitle: {ttl}\n"

# Check for duplicated publications
dup_pubs <- pub_yml %>%
  map_chr(pluck, "pubmed") %>%
  duplicated()

# Create index.qmd for each pub entry
pub_yml_new <- pub_yml[!dup_pubs] %>%
  imap(~ {
    pub_info <- .x
    
    # Read authors.yml within loop since author list gets updated for each paper
    athr_yml <- read_yaml("authors.yml")
    
    # Scrape missing info from pubmed
    pub_attrs <- c(
      "key", "title", "pmid", "date", "year",
      "authors", "abstract", "image"
    )
    
    if (any(!pub_attrs %in% names(pub_info))) {
      pub_attrs <- pub_attrs[!pub_attrs %in% names(.x)]
      pub_info  <- .scrape_pubmed(.x$pubmed, pub_attrs)
      pub_info  <- append(.x, pub_info)
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
    # * THIS WILL NOT CORRECTLY HANDLE MULTIPLE PUBS WITH SAME YEAR/AUTHOR
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
    
    # Conditionally add image field to index.qmd
    if (!is_empty(img)) {
      index_qmd <- str_c(index_qmd, "image: {img}\n")
      
      pub_info$image <- img
    }
    
    index_qmd <- str_c(
      index_qmd,
      "subtitle: {pub_info$pmid}\n\n",
      "date: '{pub_info$date}'\n\n",
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
        if (!is.null(.x$text))    vals$text <- str_c("  - text: ", .x$text)
        if (!is.null(.x$icon))    vals$icon <- str_c("    icon: ", .x$icon)
        if (!is.null(.x$link))    vals$href <- str_c("    href: ", .x$link)
        
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
    
    # Copy files for atlases
    # symbolic links do not seem to work
    atlas_lnk <- here(atlas_dir, pub_info$key)
    
    if (!is.null(links$atlas) && !file.exists(atlas_lnk)) {
      file.copy(pub, atlas_dir, recursive = TRUE)
    }
    
    # Update pubs.yml with categories derived from the authors
    # return updated pub_yml
    pub_info$project <- cats
    
    pub_info
  })

# Write updated pubs.yml
# this will have info scraped from pubmed
if (!identical(pub_yml, pub_yml_new)) {
  pub_yml_new %>%
    write_yaml(here("pubs.yml"))
}


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
      authors = str_c(athrs, collapse = "\n"),
      pmid    = str_extract(.x$pubmed, "[0-9]+(/|)$")
    )
  })

# Format publication data
tl_dat <- tl_dat %>%
  mutate(
    pmid = str_remove(pmid, "/$"),
    pmid = str_c("PMID ", pmid),
    lab  = str_c(authors, "\n", pmid),
    year = as.character(year(date))
  ) %>%
  group_by(date, year) %>%
  summarize(lab = str_c(lab, collapse = "\n\n"), .groups = "drop")

# Create publication timeline
tl <- tl_dat %>%
  ggplot(aes(date, 0)) +
  geom_line(
    linewidth = 3
  ) +
  geom_point(
    size  = 8,
    fill  = "black"
  ) +
  geom_text(
    aes(y = 0.5, label = year),
    size  = 24 / .pt,
    color = "white"
  ) +
  geom_text(
    aes(y = -0.4, label = lab),
    hjust = 0,
    vjust = 1,
    size  = 12 / .pt,
    color = "white"
  ) +
  coord_cartesian(
    xlim = c(min(tl_dat$date), max(tl_dat$date) + years(1)),
    ylim = c(-3, 1)
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#303030", color = "#303030")
  )

ggsave(
  tl,
  filename = here(img_dir, "pub_tl.png"),
  width    = 18,
  height   = 4,
  dpi      = 300,
  device   = "png"
)
