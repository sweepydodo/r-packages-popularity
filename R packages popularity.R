
library(ggplot2)
library(scales)
library(lubridate)
# library(doParallel)
library(data.table)
setDTthreads(threads = parallel::detectCores()-1)


# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #                   1. download all log files (to RAM) ----
# #_______________________________________________________________________________
# 
# # construct URLs
# x <- Sys.Date() - 372                          # start
# y <- Sys.Date() - 7                            # end
# z <- seq(x, y, by = 'day')                     # all dates
# a <- as.POSIXlt(z)$year + 1900                 # year
# b <- paste0('http://cran-logs.rstudio.com/', a, '/', z, '.csv.gz')   # URLs
# c <- c('date', 'time', 'package', 'country')   # download specific columns ONLY
# 
# # # download csvs (serially)
# # system.time(
# # d <- lapply(b, \(i) fread(i, select = c))
# # )
# 
# 
# # setup computing cluster (parallel running)
# n_cores <- detectCores() - 1
# cl <- makeCluster(n_cores)
# registerDoParallel(cl, cores = n_cores)
# 
# # download csvs (parallel running)
# system.time(
#             d <- foreach(i = seq_along(b)
#                          , .packages = c("data.table")
#                          ) %dopar%
#               {
#                 fread(b[[i]], select = c)
#               }
#             )
# 
# # stop hoarding cluster
# stopCluster(cl)
# 
# # clean environment
# rm(list = setdiff(letters[1:26], 'd')
#    , n_cores, cl
#    )
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #                       2. rbind csvs (RAM method)  ----
# #_______________________________________________________________________________
# 
# # rbind
# system.time( df <- rbindlist(d) )
# rm(d)
# 
# # save as .RData
# system.time( save(df, file = "CRANlogs/CRANlogs.RData") )
# 
# # for later analyses: load the saved data.table
# # load("CRANlogs/CRANlogs.RData")
# 
# # define variable types
# df[, `:=` (#date = parse_date_time2(date, "%Y-%m-%d")
#            # , package = factor(package)
#            # , country = factor(country)
#             day_of_week = lubridate::wday(date, week_start = 1)  # 1 = Monday
#            , week = lubridate::isoweek(date)                     # week starts on Monday
#            , hour = substr(time, 1,2)
#            )
#    ]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   1. download all log files (to HDD) ----
#_______________________________________________________________________________

# construct URLs
x <- Sys.Date() - 365                     # start
y <- Sys.Date() - 1                       # end
z <- seq(x, y, by = 'day')                # all dates
a <- as.POSIXlt(z)$year + 1900            # year
b <- paste0('http://cran-logs.rstudio.com/', a, '/', z, '.csv.gz')   # urls

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
Sys.time() - xxx


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                          2. import csvs (from HDD) ----
#_______________________________________________________________________________

x <- list.files("CRANlogs", full.names = T)     # list of csvs. Local machine can only take 3.5 months' data
y <- c('date', 'time', 'package', 'country')    # import specific columns ONLY
df <- vector('list', length(x))                 # initialise

# import csvs
xxx <- Sys.time()
for (i in seq_along(x))
{
  # print progress
  print(paste(x[i], "-", round(i/length(x)*100, 1), ' %'))
  
  # import csvs
  df[[i]] <- fread(x[i],  select = y)
}
Sys.time() - xxx  # 5.4 mins (4.5 month' data)

# rbind
system.time( df <- rbindlist(df) )   # 1.7 mins
dim(df)   # 820,648,249  4


# define variable types
system.time(
df[, `:=` (#date = parse_date_time2(date, "%Y-%m-%d")
           # , package = factor(package)
           # , country = factor(country)
            day_of_week = lubridate::wday(date, week_start = 1)  # 1 = Monday
           , week = lubridate::isoweek(date)                 # week starts on Monday
           , hour = substr(time, 1,2)
           )
   ]
)    # 12 mins

# save as .RData
system.time( save(df, file = "CRANlogs.RData") )  # 14 mins

# for later analyses: load the saved data.table
system.time( load("CRANlogs.RData") )  # 6.9 mins
#-- notice it was faster to import csvs using data.table's fread
#-- then it was to read from .RData


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               3. plot  ----
#_______________________________________________________________________________

#-------------------- top 20 downloads

# summarise
x <- df[, .(downloads = .N), package][order(-downloads)]
x[, .I[package == "data.table"]]  # data.table's ranking in overall downloads

# parameters
y <- 10    # top n to display
z <- x[1:y][['package']]

# graph title
a <- paste('Top', y, 'downloads')

# graph
b <-
  ggplot(x[1:y], aes(x=package, y=downloads)) +
    geom_bar(stat = "identity") + 
    scale_x_discrete(limits = z) +
    xlab('') +
    theme_minimal() +
    scale_y_continuous(labels = comma) +
    ggtitle(a)

# save graph
ggsave(b
       , file = paste0("graphs/", a, '.jpg')
       , width = 40, height = 23, dpi = 400, units = 'cm'
       )

print(b)


#-------------------- All downloads by date

# summarise
x <- df[, .(downloads = .N), week][, week := as.factor(week)][order(week)]

# graph title
y <- 'Downloads by week'

# graph
z <-
  ggplot(x, aes(x = week, y = downloads, group = 1)) +
    geom_line() +
    # geom_text(aes(label=round(y)), hjust=0, vjust=0) +        # data point label
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +  # x axis angle
    theme_minimal() +
    scale_y_continuous(labels = comma) +
    ggtitle(y)

# save graph
ggsave(z
       , file = paste0("graphs/", y, '.jpg')
       , width = 40, height = 23, dpi = 400, units = 'cm'
       )

print(z)


#-------------------- Compare downloads of specific packages by week

# summarise
x <- c("data.table", "dplyr", "plyr", "dtplyr")
y <- df[package %in% x
        , .(downloads = .N)
        , .(week, package)
        ][, weekday := as.factor(week)
          ]

# graph title
z <- 'Downloads of specific packages by week'

# graph
a <-
  ggplot(y, aes(x = week, y = downloads, color = package, group = package)) +
    geom_line() +
    ylab("Downloads") +
    theme_bw() +
    theme(axis.text.x  = element_text(angle=90, size=8, vjust=0.5)) +
    theme_minimal() +
    scale_y_continuous(labels = comma) +
    ggtitle(z)

# save graph
ggsave(a
       , file = paste0("graphs/", z, '.jpg')
       , width = 40, height = 23, dpi = 400, units = 'cm'
       )

print(a)


#-------------------- Distribution (of mean downloads) by day of week

# summarise
x <- df[, .(downloads = .N)
        , .(day_of_week, week)
        ][, .(downloads = mean(downloads))
          , .(day_of_week)
          ][, weekday := as.factor(day_of_week)
            ]
# graph title
y <- 'Distribution of average downloads by day of week'

# graph
z <-
  ggplot(x, aes(x = day_of_week, y = downloads)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    scale_y_continuous(labels = comma) +
    ggtitle(y)

# save graph
ggsave(z
       , file = paste0("graphs/", y, '.jpg')
       , width = 40, height = 23, dpi = 400, units = 'cm'
       )

print(z)


#-------------------- Distribution (of mean downloads) by hour of day

# summarise
x <- df[, .(downloads = .N)
          , .(hour, date)
          ][, .(downloads = mean(downloads))
            , .(hour)
            ]

# graph title
y <- 'Distribution of average downloads by hour of day'

# graph
z <-
  ggplot(x, aes(x = hour, y = downloads)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    scale_y_continuous(labels = comma) +
    ggtitle(y)

# save graph
ggsave(z
       , file = paste0("graphs/", y, '.jpg')
       , width = 40, height = 23, dpi = 400, units = 'cm'
       )

print(z)


#-------------------- Distribution of total downloads by country

# summarise
x <- df[, .(downloads = .N), country
        ][, pc := paste(round( downloads / sum(downloads)*100, 1), '%')
          ][order(-downloads)
            ]

# parameters
y <- 10    # top n to display
z <- x[1:y][['country']]

# graph title
a <- paste('Total downloads by country', '- top', y)

# graph
b <-
  ggplot(x[1:y], aes(x=country, y=downloads)) +
    geom_bar(stat = "identity") + 
    geom_text( data = x[1:y], aes(x = country, y = downloads, label = pc), vjust = -1 ) +
    scale_x_discrete(limits = z) +
    xlab('') +
    theme_minimal() +
    scale_y_continuous(labels = comma) +
    ggtitle(a)

# save graph
ggsave(b
       , file = paste0("graphs/", a, '.jpg')
       , width = 40, height = 23, dpi = 400, units = 'cm'
       )

print(b)


#-------------------- Total downloads by week, country

# summarise
x <- df[, .(downloads = .N), .(week, country)
        ][, total_country_downloads := sum(downloads)
          , country
          ][, total_country_downloads_rank := frank(-total_country_downloads, ties.method = 'dense')
            ][, week := as.factor(week)
              ][order(country, week)
                ]

# parameters
y <- 5    # top n to display

# graph title
z <- paste('Total downloads by week, country', '- top', y, 'country')

# graph
a <-
  ggplot(x[total_country_downloads_rank %between% c(1, y)]
         , aes(x = week, y = downloads, group = country)
         ) +
    geom_line(aes(colour = country)) +
    # geom_text(aes(label=round(y)), hjust=0, vjust=0) +        # data point label
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +  # x axis angle
    theme_minimal() +
    scale_y_continuous(labels = comma) +
    ggtitle(z)

# save graph
ggsave(a
       , file = paste0("graphs/", z, '.jpg')
       , width = 40, height = 23, dpi = 400, units = 'cm'
       )

print(a)












