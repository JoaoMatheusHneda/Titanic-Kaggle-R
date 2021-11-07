getwd()
library(rmarkdown)
rmarkdown:::convert_ipynb(input='Codigos_R.ipynb',
                        output = xfun::with_ext('Codigos_R.ipynb', "Rmd"))
