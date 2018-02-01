#!/SYSTEM/R/3.3.3/bin/Rscript
Sys.setenv(RSTUDIO_PANDOC="/SYSTEM/pandoc/1.17.0.3/bin")
print(Sys.getenv("RSTUDIO_PANDOC"))

print(capabilities())
print(sessionInfo())


library(magrittr)

# init ----

if (grepl('linux', R.version$os)) .libPaths(c("./lib", '/SYSTEM/R/3.3.3/lib64/R/library')) %>% print()

print(lapply(.libPaths(), dir))

if (length(intersect(dir(), 'result')) == 0) system('mkdir result')

# setup ----

libraries <- c('dplyr', 'purrr', 'wnl', 'NonCompart', 'markdown') # lapply(libraries, install.packages)
lapply(libraries, library, character.only = TRUE)
knitr::knit('README.Rmd')
markdownToHTML('README.md', "result/README.html", 
               options = c("toc", "mathjax"))
system('cp result/figure1.png result/figure2.png ./')

# system('cp README.md result')

# render('README.Rmd', output_file = 'README.html')

# system('cp R/PK02.R README.Rmd result')

# setwd('./result')


# system('rm result/*.R result/*.Rmd')

# render('README.Rmd', output_file = 'result/README.html')


