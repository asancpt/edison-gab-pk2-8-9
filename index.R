#!/SYSTEM/R/3.5.1/bin/Rscript
# `simrc` should contain `module load gcc/5.3.0` and `module load R/3.5.1`

if (length(intersect(dir(), 'result')) == 0) { system('mkdir result') }
options(bitmapType='cairo')

# libraries ----

if (Sys.info()['sysname'] == 'Linux') { .libPaths('./lib') }
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(wnl)
library(NonCompart)
library(knitr)

# Arguments

input_deck <- 'number = PK02 ;
'

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) == 0) { arguments <- c("-inp", input_deck, "-file", "TBD.csv") }

table_args <- matrix(arguments, ncol = 2, byrow = TRUE) %>%
  as_tibble() %>%
  mutate(V1 = sub('-', '', V1)) %>%
  spread(V1, V2) %>%
  print()

pk_number <- read_delim(table_args$inp, delim = '=', col_names = c('param', 'value')) %>% 
  mutate_all(funs(trimws)) %>% 
  mutate(value = sub(' ;', '', value)) %>% 
  print()

# Make a report ----

knitr::knit2html(input = sprintf("%s-report.Rmd", pk_number$value), 
                 output = "result/report.html", 
                 options = c("toc", "mathjax"), 
                 force_v1 = TRUE, 
                 encoding = 'UTF-8')

system('cp -r figure result')

print(capabilities())
print(sessionInfo())
