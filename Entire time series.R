
library(ggplot2)
library(ggrepel)
library(scales)
library(doParallel)
library(data.table)
setDTthreads(threads = parallel::detectCores()-1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      download all log files (to HDD)                      ----
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
#                         import log csvs - in batches                  ----
#_______________________________________________________________________________

# parameters
x <- list.files("CRANlogs", full.names = T)   # list of csvs. Local machine can only take 3.5 months' data
y <- c('date'
       #, 'time'
       )                                      # import specific columns ONLY
z <- c('package', 'country')                  # group by cols
a <- 1e2                                      # import ONLY n csvs at a time
b <- length(x)                                # no of total csvs to import
c <- seq(1, b, a)                             # index ith of start of each batch
d <- "aggregated counts"
dir.create(d)                                 # create folder to store aggregated counts

# register cores for parallel running
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl, cores = n_cores)


# import csvs, aggregate then export as summary csv
xxx <- Sys.time()
for (i in c)
{
  # print progress
  print(paste(x[i], "-", round(i/length(x)*100, 1), ' %'))
  
  # # import csvs
  # df <- lapply( x[i:min(i+a-1, b)], \(j) fread(j,  select = c(y,z)) )
  
  # parallel running - import csvs
  df <- foreach(j = x[i:min(i+a-1, b)]
                , .packages = c("data.table")
                ) %dopar%
    {
      fread(j,  select = c(y,z))
    }
  
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

  # aggregate downloads by...
  e <- lapply(z, \(j) df[, .(downloads = .N), c('year_month', j)])
  
  # export as csv
  lapply(e, \(j) fwrite(j
                        , paste0(d
                                 , "/downloads by month, "
                                 , names(j)[2]
                                 , " - "
                                 , " batch beginning "
                                 , substr(x[i:min(i+1-1, b)][1], 10, 19)
                                 , ".csv"
                                 )
                        )
         )
  
  rm(df)
  
}
Sys.time() - xxx  # 1.4 hrs (w/o parallel) 1.2 hrs (w parallel)


# stop hoarding cluster
stopCluster(cl)

rm(n_cores, cl, df)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                         import aggregated counts                     ----
#_______________________________________________________________________________

# parameters
x <- list.files(d, full.names = T)            # list of aggregated counts' csvs
y <- lapply(z, \(i) x[grepl(i, x)])           # separate csv names of different group by variable
df <- vector('list', length(z))               # initialise empty list

xxx <- Sys.time()
# import csvs then aggregate again
for (i in seq_along(y))
{
  # import csvs
  a <- lapply( y[[i]], \(j) fread(j) )
  
  # rbind
  a <- rbindlist(a)
  
  # aggregate again (as same month may have been split across multiple csvs)
  df[[i]] <- a[, .(downloads = sum(downloads)), c('year_month', z[[i]])]
  
}
Sys.time() - xxx  # 1.1 secs

names(df) <- z

# format year month col
lapply(df, \(i) i[, year_month := paste0(substr(year_month,1,4), '-', substr(year_month,5,6))])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                              plot                     ----
#_______________________________________________________________________________

dir.create('graphs')


#-------------------- by variable
for(i in seq_along(df))
{
  # find top 4 overall value in group by variable
  x <- 4
  y <- df[[i]][, total_downloads := sum(downloads)
               , eval(z[i])
               ][, rank := frank(-total_downloads, ties.method = 'dense')
                 ][rank %between% c(1, x)
                   ]
  
  # create label for plot
  y[y[, .I[year_month == max(year_month)]], label := get(z[i])]
  
  # graph title
  a <- paste('Downloads of top', x, z[i], 'by month')
  
  # graph
  b <- ggplot(y
              , aes_string(x = 'year_month', y = 'downloads', color = z[i], group = z[i])
              ) +
          geom_line() +
          theme_minimal() +
          geom_label_repel(aes(label = label)
                           , nudge_x = 1
                           , na.rm = TRUE
                           ) +
          theme(legend.position="none") +
          ylab("Downloads") +
          theme(axis.text.x  = element_text(angle=50, vjust=0.5)) +
          scale_y_continuous(labels = comma) 
          # ggtitle(a)  # ggtitle crashes with geom_label_repel. R is making me choose between the 2
  
  
  # save graph
  ggsave(b
         , file = paste0("graphs/", a, '.jpg')
         , width = 40, height = 23, dpi = 400, units = 'cm'
         , bg = "white"
         )
  
  print(b)
}


#-------------------- Total downloads (not by variable)

# summarise
x <- df[['country']][, .(downloads = sum(downloads)), year_month]

# graph title
y <- 'Downloads by year_month'

# graph
a <- ggplot(x, aes(x = year_month, y = downloads, group = 1)) +
        geom_line() +
        theme_minimal() +
        # geom_text(aes(label=round(y)), hjust=0, vjust=0) +        # data point label
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +  # x axis angle
        scale_y_continuous(labels = comma) +
        ggtitle(y)

# save graph
ggsave(a
       , file = paste0("graphs/", y, '.jpg')
       , width = 40, height = 23, dpi = 400, units = 'cm'
       , bg = "white"
       )

print(a)


#-------------------- Compare downloads of specific packages by month

# summarise
x <- c("data.table", "dplyr", "plyr", "dtplyr")

# subset
y <- df[['package']][package %in% x]

# create label for plot
y[y[, .I[year_month == max(year_month)]], label := package]

# graph title
a <- paste('Downloads of specific packages by month')

# graph
b <- ggplot(y
            , aes_string(x = 'year_month', y = 'downloads', color = 'package', group = 'package')
            ) +
        geom_line() +
        theme_minimal() +
        geom_label_repel(aes(label = label)
                         , nudge_x = 1
                         , na.rm = TRUE
                         ) +
        theme(legend.position="none") +
        ylab("Downloads") +
        theme(axis.text.x  = element_text(angle=50, vjust=0.5)) +
        scale_y_continuous(labels = comma) 
        # ggtitle(a)  # ggtitle crashes with geom_label_repel. R is making me choose between the 2


# save graph
ggsave(b
       , file = paste0("graphs/", a, '.jpg')
       , width = 40, height = 23, dpi = 400, units = 'cm'
       , bg = "white"
       )

print(b)

