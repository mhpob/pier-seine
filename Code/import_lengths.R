library(readxl); library(data.table)

# excel_sheets('p:/obrien/seine/cbl_seine_database.xlsx')

lengths <- read_excel('p:/obrien/seine/cbl_seine_database.xlsx',
                      sheet = 'Counts_Lengths',
                      guess_max = 2000)

lengths <- setDT(lengths)[, 1:33]

lengths <- melt(lengths, 
                id.vars = c('RECORD_NUM', 'SCIENTIFIC', 'COUNT'),
                value.name = 'length')
lengths <- lengths[!is.na(length)]
lengths[, ':='(variable = NULL,
               COUNT = NULL)]

lengths[, length := gsub('[A-z]', '', length)]
lengths[, length := as.numeric(length)]


site_info <- read_excel('p:/obrien/seine/cbl_seine_database.xlsx',
                        sheet = 'Site_Info')
setDT(site_info)



lengths <- lengths[site_info, on = c(RECORD_NUM = 'RECORD NUM')]

setnames(lengths, tolower(names(lengths)))

fwrite(lengths, 'data/derived/lengths.csv')
