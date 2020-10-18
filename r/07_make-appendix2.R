source("r/header.R")

si <- session_info(include_base = FALSE)

r_package_table <- map2_dfr(si$packages$package, si$packages$loadedversion, ~ {
  data.frame(
    key = as.character(as.BibEntry(citation(.x))$key)
  ) %>%
    mutate(
      package = c(.x, rep("", nrow(.) - 1L)),
      version = c(.y, rep("", nrow(.) - 1L)),
      key = paste0("@", key, "")
    )
})

file.create("ms/appendix.bib")
si$packages$package %>%
  walk(~ {
    write.bibtex(citation(.x), file = "ms/appendix.bib", append = TRUE)
  })
