#!/bin/bash

echo "Cheks the package: devtools::build(...)."
echo "Creates the *.check/ directory."
Rscript -e "library(devtools); load_all(); check(manual = TRUE, vignettes = FALSE, check_dir = '.')"

echo "Builds the package: devtools::build(...)."
echo "Creates the *.tar.gz file."
Rscript -e "library(devtools); load_all(); build(path = '.', manual = TRUE, vignettes = FALSE)"

echo "Creates the *.zip file."
TARGZ=$(ls wzRfun*.tar.gz | tail -n 1)
ZIP=$(basename "$TARGZ" .tar.gz)
rm -v "$ZIP.zip"
cd wzRfun.Rcheck/
zip -r "../$ZIP.zip" wzRfun
cd ../

echo "Compressed files available."
ls wzRfun_*

# Upload.
echo "Uploading compressed files to server."
rsync -avzp \
      wzRfun*.tar.gz \
      wzRfun*.zip \
      --progress \
      --rsh="ssh -p$PATAXOP" "$PATAXO:~/public_html/pacotes/"

echo "Creates the package page: pkgdown::build_site(...)."
echo "Creates the *.check/ directory."
Rscript -e "library(devtools); load_all(); library(pkgdown); build_site()"

echo "Uploading site to server."
rsync -avzp \
      ./docs/* \
      --progress \
      --rsh="ssh -p$PATAXOP" "$PATAXO:~/public_html/pacotes/wzRfun"

exit 0
