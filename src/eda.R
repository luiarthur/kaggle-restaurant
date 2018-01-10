library(sqldf)
'%+%' = function(a,b) paste0(a,b)

groupBy = function(X, cname) {
  column = as.character(X[,cname])
  id = unique(column)
  lapply(id, function(x) X[which(column==x),])
}

DATA_DIR = '../data/'


visit_history = read.csv(DATA_DIR %+% 'air_visit_data.csv')

air = read.csv(DATA_DIR %+% 'air_reserve.csv')
air$visit_date = sapply(air$visit, function(x) strsplit(as.character(x)," ")[[1]][1])

air_agg <- sqldf('
  SELECT
    air_store_id, visit_date, SUM(reserve_visitors) AS sum_visitors
  FROM
    air
  GROUP BY
    air_store_id, visit_date
')
head(air_agg)

unique_air = as.character(unique(air_agg$air_store))

air_agg_sep = lapply(unique_air, function(x) air_agg[which(air_agg$air_store_id==x),])
N = sapply(air_agg_sep, nrow)

plot(air_agg_sep[[1]]$sum_visitors, xaxt='n', ylab='visitors', xlab='')
axis(1, at=1:N[1], air_agg_sep[[1]]$visit_date, las=2, cex.axis=.8)



### Historical Visits ###
hist_all <- sqldf('
  SELECT visit_history.*, air_agg.sum_visitors
  FROM visit_history LEFT JOIN air_agg
  WHERE 
    visit_history.air_store_id == air_agg.air_store_id AND
    visit_history.visit_date == air_agg.visit_date
')

head(hist_all)
x = groupBy(hist_all, 'air_store_id')

plot.ts(x[[1]]$visitors)
axis(1, at=1:N[1], air_agg_sep[[1]]$visit_date, las=2, cex.axis=.8)

plot(x[[9]]$sum_visitors, x[[9]]$visitors)
