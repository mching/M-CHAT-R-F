rm(list = ls())

library(ProjectTemplate)
library(binom)
load.project()

## if you don't have ProjectTemplate, you can install via
# install.packages("ProjectTemplate")

## if no ProjectTemplate, you use read.spss
# library(foreign)
# pre <- read.spss("./data/pre.sav")
# post <- read.spss("./data/post.sav")

for (dataset in project.info$data)
{
  message(paste('Showing top 5 rows of', dataset))
  print(head(get(dataset)))
}

