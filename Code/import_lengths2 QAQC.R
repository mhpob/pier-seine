library(readxl); library(data.table)

# excel_sheets('p:/obrien/seine/cbl_seine_database.xlsx')


## Import length data from QaQcd source ----

lengths <- read_excel('C:/Users/benba/Documents/GitHub/pier-seine/Data/CBL_seine_database_2019_QA_QC.xlsx',
                      sheet = 'Counts_Lengths QA_QC',
                      guess_max = 2000)

lengths <- setDT(lengths)[, 1:33]
setnames(lengths, tolower)

lengths <- melt(lengths, 
                id.vars = c('record_num', 'scientific', 'count'),
                value.name = 'length')
lengths <- lengths[!is.na(length)]
lengths[, ':='(scientific = tolower(scientific),
               variable = NULL,
               count = NULL)]

## Make everything a number ----
lengths[, length := gsub('[A-z]|\\*|>', '', length)]
### There's stil one special case, where length is "1-1.5"
lengths[length == ' 1-1.5 ', length := 1.5]
lengths[, length := as.numeric(length)]



## Import site data ----

site_info <- read_excel('C:/Users/benba/Documents/GitHub/pier-seine/Data/CBL_seine_database_2019_QA_QC.xlsx',
                        sheet = 'Site_Info')
setDT(site_info)
setnames(site_info, tolower)
setnames(site_info, 'salinity (surface, ppt)', 'sal')

site_info[, ':='(temp = as.numeric(temp),
                 sal = as.numeric(sal),
                 wk = week(date))]

lengths <- lengths[site_info, on = c(record_num = 'record num')]


fwrite(lengths, 'data/derived/lengths2.csv')
