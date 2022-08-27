
library(ggplot2)
library(scales)
library(data.table)
setDTthreads(threads = parallel::detectCores()-1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   1. download all log files (to HDD)                      ----
#_______________________________________________________________________________

# construct URLs
#x <- Sys.Date() - 365                     # start
x <- as.Date('2012-10-01')                # start (download ALL logs. This is 1st date logging started)
y <- Sys.Date() - 1                       # end
z <- seq(x, y, by = 'day')                # date range from start to end

# only download the files you don't have:
dir.create("CRANlogs")
z <- setdiff(as.character(z), tools::file_path_sans_ext(dir("CRANlogs"), T))
a <- as.POSIXlt(z)$year + 1900
b <- paste0('http://cran-logs.rstudio.com/', a, '/', z, '.csv.gz')   # urls

# start downloads
xxx <- Sys.time()
for (i in seq_along(b))
{
  print(paste0(i, "/", length(b), ' or ', round(i/length(b)*100, 1), ' %'))
  download.file(b[i], paste0('CRANlogs/', z[i], '.csv.gz'))
}
Sys.time() - xxx   # 7 hours


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                          2. import csvs - in batches                     ----
#_______________________________________________________________________________

x <- list.files("CRANlogs", full.names = T)   # list of csvs. Local machine can only take 3.5 months' data
y <- c('date', 'package', 'country'
       #, 'time'
       )                                      # import specific columns ONLY
z <- 1e2                                      # import ONLY 100 csvs at a time
a <- length(x)                                # no of total csvs to import
b <- seq(1, a, z)                             # index ith of start of each batch
c <- "aggregated counts"
dir.create(c)                                 # create folder to store aggregated counts

# import csvs, aggregate then export as summary csv
xxx <- Sys.time()
for (i in b)
{
  # print progress
  print(paste(x[i], "-", round(i/length(x)*100, 1), ' %'))
  
  # import csvs
  df <- lapply( x[i:min(i+z-1, a)], \(j) fread(j,  select = y) )
  
  # rbind
  df <- rbindlist(df)
  
  # define variable types
  df[, `:=` (#date = parse_date_time2(date, "%Y-%m-%d")
              # , package = factor(package)
              # , country = factor(country)
              year_month = lubridate::year(date)*100 + lubridate::month(date)
              # , week = lubridate::year(date)*100 + lubridate::isoweek(date)   # week starts on Monday. Leave as numeric for speed
              # , day_of_week = lubridate::wday(date, week_start = 1)           # 1 = Monday
              # , hour = substr(time, 1,2)
              )
     ]
  
  # aggregate by country
  country <- df[, .(downloads = .N), .(year_month, country)]
  
  # aggregate by package
  package <- df[, .(downloads = .N), .(year_month, package)]
  
  # export as csv
  lapply(list(country, package), \(j) fwrite(j
                                             , paste0(c
                                                      , "/downloads by month, "
                                                      , names(j)[2]
                                                      , " - "
                                                      , " batch beginning "
                                                      , substr(x[i:min(i+z-1, a)][1], 10, 19)
                                                      , ".csv"
                                                      )
                                             )
         )
  
}
Sys.time() - xxx  #





