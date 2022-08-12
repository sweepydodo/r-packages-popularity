
library(ggplot2)
library(lubridate)
library(data.table)
setDTthreads(threads = parallel::detectCores()-1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                     1. download all log files  ----
#_______________________________________________________________________________

# construct URLs
x <- Sys.Date() - 365                     # start
y <- Sys.Date() - 1                       # end
z <- seq(x, y, by = 'day')                # all dates
a <- as.POSIXlt(z)$year + 1900            # year
b <- paste0('http://cran-logs.rstudio.com/', a, '/', z, '.csv.gz')   # urls

# only download the files you don't have:
dir.create("CRANlogs")
c <- setdiff(as.character(z), tools::file_path_sans_ext(dir("CRANlogs"), T))

# start downloads
for (i in seq_along(c))
{
  print(paste0(i, "/", length(c)))
  download.file(b[i], paste0('CRANlogs/', c[i], '.csv.gz'))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                          2. import csvs  ----
#_______________________________________________________________________________

x <- list.files("CRANlogs", full.names = T)     # list of csvs
y <- vector('list', length(x))                  # initialise

# import csvs
for (i in x)
{
  print(paste("Reading", i, "..."))
  y[[i]] <- fread(i)
}

# rbind
df <- rbindlist(y)

# define variable types
df[, `:=` (#date = parse_date_time2(date, "%Y-%m-%d")
           # , package = factor(package)
           # , country = factor(country)
            weekday = lubridate::wday(date, week_start = 1)  # 1 = Monday
           , week = lubridate::isoweek(date)                 # week starts on Monday
           )
   ]

# setkey(df, package, date, week, country)

# save as .RData
save(df, file = "CRANlogs/CRANlogs.RData")

# for later analyses: load the saved data.table
# load("CRANlogs/CRANlogs.RData")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               3. plot  ----
#_______________________________________________________________________________

# Overall downloads of packages
x <- df[, (downloads = .N), package][order(-downloads)]
x[package == "data.table"]

# plot 1: Compare downloads of selected packages on a weekly basis
x <- c("data.table", "dplyr", "plyr", "dtplyr")
y <- df[package %in% x
        , .(downloads = .N)
        , .(week, package)
        ]

ggplot(y, aes(x = week, y = downloads, color = package, group = package)) +
  geom_line() +
  ylab("Downloads") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, size=8, vjust=0.5))



agg1 <- dat[J(c("psych", "TripleR", "RSA")), length(unique(ip_id)), by=c("week", "package")]
ggplot(agg1, aes(x=week, y=V1, color=package, group=package)) + geom_line() + ylab("Downloads") + theme_bw() + theme(axis.text.x  = element_text(angle=90, size=8, vjust=0.5))








x <- c('os', 'country')

lapply(x, \(i) df[, .(rows = .N), get(i)
                  ][, pc := round(rows / sum(rows)*100, 1)
                    ][order(-rows)
                      ]
       )
