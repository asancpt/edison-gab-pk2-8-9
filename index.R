#!/SYSTEM/R/3.3.3/bin/Rscript
Sys.getenv("RSTUDIO_PANDOC")

library(magrittr)

# init ----

if (grepl('linux', R.version$os)) .libPaths(c("./lib", '/SYSTEM/R/3.3.3/lib64/R/library')) %>% print()

print(lapply(.libPaths(), dir))

if (length(intersect(dir(), 'result')) == 0) system('mkdir result')

# setup ----

libraries <- c('dplyr', 'purrr', 'wnl', 'NonCompart', 'rmarkdown') # lapply(libraries, install.packages)

lapply(libraries, library, character.only = TRUE)

render('README.Rmd', output_file = 'README.html')

system('cp README.html figure1.png figure2.png result')







