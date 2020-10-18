source("r/header.R")

# Download bibliography from Zotero
bib <- ReadZotero("5814932", .params = list(key = "ED0yHWd4Qtd6ereWuiLgYN2p", collection = "T93F8SNM", limit = 1000), temp.file = "ms/stomata-tradeoff.bib", delete.file = FALSE)
