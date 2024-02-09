.create_project_links <- function(athr_info, pub_info, lnk_cls) {
  
  # Identify publications and atlases listed for each project
  # only want to include links if there is something to link to
  pubs <- pub_info %>%
    map_chr(pluck, "categories") %>%
    unique()
  
  atlases <- pub_info %>%
    map(pluck, "atlas") %>%
    discard(is.null) %>%
    names()
  
  atlases <- pub_info[atlases] %>%
    map_chr(pluck, "categories") %>%
    unique()
  
  # Create links
  res <- athr_info %>%
    map(pluck, "project") %>%
    discard(is.null) %>%
    imap(~ {
      nm     <- str_c(.y, ", PhD")
      nm_lnk <- str_to_lower(nm)
      nm_lnk <- str_replace_all(nm_lnk, "( |, )", "-")
      
      lnk <- str_c("[", nm, "](investigators.qmd#", nm_lnk, ")", lnk_cls)
      
      if (.x %in% pubs) {
        lnk <- str_c(lnk, " [Publications](pubs.qmd#category=", .x, ")", lnk_cls)
      }
      
      if (.x %in% atlases) {
        lnk <- str_c(lnk, " [Atlases](atlases.qmd#category=", .x, ")", lnk_cls)
      }
      
      lnk
    })
  
  res
}