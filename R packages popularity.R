
library(ggplot2)
library(scales)
library(lubridate)
library(doParallel)
library(data.table)
setDTthreads(threads = parallel::detectCores()-1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   1. download all log files (to RAM) ----
#_______________________________________________________________________________

# construct URLs
x <- Sys.Date() - 372                          # start
y <- Sys.Date() - 7                            # end
z <- seq(x, y, by = 'day')                     # all dates
a <- as.POSIXlt(z)$year + 1900                 # year
b <- paste0('http://cran-logs.rstudio.com/', a, '/', z, '.csv.gz')   # URLs
c <- c('date', 'time', 'package', 'country')   # download specific columns ONLY

# # download csvs (serially)
# system.time(
# d <- lapply(b, \(i) fread(i, select = c))
# )


# setup computing cluster (parallel running)
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl, cores = n_cores)

# download csvs (parallel running)
system.time(
            d <- foreach(i = seq_along(b)
                         , .packages = c("data.table")
                         ) %dopar%
              {
                fread(b[[i]], select = c)
              }
            )

# stop hoarding cluster
stopCluster(cl)

# clean environment
rm(list = setdiff(letters[1:26], 'd')
   , n_cores, cl
   )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                       2. rbind csvs (RAM method)  ----
#_______________________________________________________________________________

# rbind
system.time( df <- rbindlist(d) )
rm(d)

# save as .RData
system.time( save(df, file = "CRANlogs/CRANlogs.RData") )

# for later analyses: load the saved data.table
# load("CRANlogs/CRANlogs.RData")

# define variable types
df[, `:=` (#date = parse_date_time2(date, "%Y-%m-%d")
           # , package = factor(package)
           # , country = factor(country)
            day_of_week = lubridate::wday(date, week_start = 1)  # 1 = Monday
           , week = lubridate::isoweek(date)                     # week starts on Monday
           , hour = substr(time, 1,2)
           )
   ]




# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #                   1. download all log files (to HDD) ----
# #_______________________________________________________________________________
# 
# # construct URLs
# x <- Sys.Date() - 365                     # start
# y <- Sys.Date() - 1                       # end
# z <- seq(x, y, by = 'day')                # all dates
# a <- as.POSIXlt(z)$year + 1900            # year
# b <- paste0('http://cran-logs.rstudio.com/', a, '/', z, '.csv.gz')   # urls
# 
# # only download the files you don't have:
# dir.create("CRANlogs")
# c <- setdiff(as.character(z), tools::file_path_sans_ext(dir("CRANlogs"), T))
# 
# # start downloads
# xxx <- Sys.time()
# for (i in seq_along(c))
# {
#   print(paste0(i, "/", length(c)))
#   download.file(b[i], paste0('CRANlogs/', c[i], '.csv.gz'))
# }
# print(paste('download took', round(difftime(Sys.time(), xxx, units = 'mins'), 2), 'mins'))
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #                          2. import csvs (from HDD) ----
# #_______________________________________________________________________________
# 
# x <- list.files("CRANlogs", full.names = T)     # list of csvs
# y <- vector('list', length(x))                  # initialise
# 
# # import csvs
# xxx <- Sys.time()
# for (i in x)
# {
#   print(paste("Reading", i, "..."))
#   y[[i]] <- fread(i)
# }
# print(paste('importing csvs took', round(difftime(Sys.time(), xxx, units = 'mins'), 2), 'mins'))
# 
# # rbind
# system.time( df <- rbindlist(y) )
# 
# # define variable types
# df[, `:=` (#date = parse_date_time2(date, "%Y-%m-%d")
#            # , package = factor(package)
#            # , country = factor(country)
#             weekday = lubridate::wday(date, week_start = 1)  # 1 = Monday
#            , week = lubridate::isoweek(date)                 # week starts on Monday
#            )
#    ]
# 
# # setkey(df, package, date, week, country)
# 
# # save as .RData
# system.time( save(df, file = "CRANlogs/CRANlogs.RData") )
# 
# # for later analyses: load the saved data.table
# # load("CRANlogs/CRANlogs.RData")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               3. plot  ----
#_______________________________________________________________________________

#-------------------- top 20 downloads
x <- df[, .(downloads = .N), package][order(-downloads)]
x[, .I[package == "data.table"]]  # data.table's ranking in overall downloads

y <- 10    # top n to display
z <- x[1:y][['package']]

ggplot(x[1:y], aes(x=package, y=downloads)) +
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = z) +
  xlab('') +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  ggtitle(paste('Top', y, 'downloads'))



#-------------------- All downloads by date
x <- df[, .(downloads = .N), week][, week := as.factor(week)][order(week)]

ggplot(x, aes(x = week, y = downloads, group = 1)) +
  geom_line() +
  # geom_text(aes(label=round(y)), hjust=0, vjust=0) +        # data point label
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +  # x axis angle
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  ggtitle('Downloads by week')


#-------------------- Compare downloads of specific packages by week
x <- c("data.table", "dplyr", "plyr", "dtplyr")
y <- df[package %in% x
        , .(downloads = .N)
        , .(week, package)
        ][, weekday := as.factor(week)
          ]

ggplot(y, aes(x = week, y = downloads, color = package, group = package)) +
  geom_line() +
  ylab("Downloads") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, size=8, vjust=0.5)) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  ggtitle('Downloads of specific packages by week')


#-------------------- Distribution (of mean downloads) by day of week
system.time(
  x <- df[, .(downloads = .N)
          , .(day_of_week, week)
          ][, .(downloads = mean(downloads))
            , .(day_of_week)
            ][, weekday := as.factor(day_of_week)
              ]
) # 2.56 secs


ggplot(x, aes(x = day_of_week, y = downloads)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  ggtitle('Distribution of average downloads by day of week')


#-------------------- Distribution (of mean downloads) by hour of day

x <- df[, .(downloads = .N)
          , .(hour, date)
          ][, .(downloads = mean(downloads))
            , .(hour)
            ]
   
ggplot(x, aes(x = hour, y = downloads)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  ggtitle('Distribution of average downloads by hour of day')



