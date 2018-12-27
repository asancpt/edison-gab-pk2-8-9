cp index.R $1.R
zip -FSr -ll releases/$1.zip lib $1.R *.R *.Rmd data-raw simrc
rm $1.R

