rm(list = ls())

library('ProjectTemplate')
load.project()

## if no ProjectTemplate, use read.spss
# library(foreign)
# data <- read.spss("./data/pre.sav")


for (dataset in project.info$data)
{
  message(paste('Showing top 5 rows of', dataset))
  print(head(get(dataset)))
}

