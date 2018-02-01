# library

if (grepl('linux', R.version$os)) {
  .libPaths(c("./lib", '/SYSTEM/R/3.3.3/lib64/R/library'))
  print('libPaths() modified')
}

lapply(.libPaths(), dir)

library(magrittr)

libraries <- c('dplyr', 'purrr', 'wnl', 'NonCompart')

lapply(libraries, library, character.only = TRUE)

# lapply(libraries, install.packages)

.libPaths() %>% 
  map(dir) %>%
  c() %>%
  print()

if (length(intersect(dir(), 'result')) == 0) system('mkdir result')

# 

# library(dplyr)
# library(purrr)
# library(wnl)
# library(NonCompart)


