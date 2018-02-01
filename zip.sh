cp index.R $1.R
zip -r releases/$1.zip $1.R R README.Rmd data-raw
rm $1.R

