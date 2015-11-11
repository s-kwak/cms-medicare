library(dplyr)
library(tidyr)

setwd('C:/Users/am90005960/Desktop/Medicare Data/NDC')

# read in files
package <- tbl_df(read.table('package.txt', sep = '\t', stringsAsFactors = FALSE,
                             header = TRUE))

product <- tbl_df(read.table('product.txt', sep = '\t', stringsAsFactors = FALSE,
                             header = TRUE, fill = TRUE))

#filter for uniques
package_u <- package %>%
  select(-PRODUCTID) %>%
  distinct()

product_u <- product %>%
  select(PRODUCTNDC:PROPRIETARYNAME, LABELERNAME:SUBSTANCENAME) %>%
  select(-PRODUCTTYPENAME) %>%
  distinct()

# merge
ndc <- package_u %>%
  merge(product_u, by.x = 'PRODUCTNDC', by.y = 'PRODUCTNDC',
        all.x = TRUE, all.y = FALSE) %>%
  select(PRODUCTNDC, NDCPACKAGECODE, PROPRIETARYNAME:SUBSTANCENAME, PACKAGEDESCRIPTION) %>%
  filter(!is.na(PROPRIETARYNAME))

# create ndc11 code

ndc <- ndc %>%
  separate(NDCPACKAGECODE, c('ndc_a', 'ndc_b', 'ndc_c'), sep = '-', remove = FALSE) %>%
  mutate(ndc_a2 = sprintf('%05d', as.numeric(ndc_a)),
         ndc_b2 = sprintf('%04d', as.numeric(ndc_b)),
         ndc_c2 = sprintf('%02d', as.numeric(ndc_c))) %>%
  mutate(NDC11 = paste(ndc_a2, ndc_b2, ndc_c2, sep = '')) %>%
  select(-ndc_a, -ndc_b, -ndc_c, -ndc_a2, -ndc_b2, -ndc_c2)
