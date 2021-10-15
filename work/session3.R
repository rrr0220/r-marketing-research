

# sesseion3-1 -------------------------------------------------------------

# データの作成
k.stores = 20
k.weeks = 104
store.df = data.frame(matrix(NA, ncol=10, nrow=k.stores*k.weeks))
names(store.df) = c("storeNum", "Year", "Week", "p1sales", "p2sales", 
                    "p1price", "p2price", "p1prom", "p2prom", "country")

dim(store.df)

# 列へのデータ代入
store.num = 101:(100+k.stores)
store.num
store.cty = c(rep("US", 3), rep("DE", 5), rep("GB", 3), rep("BR", 2),
              rep("JP", 4), rep("AU", 1), rep("CN", 2))
store.cty
length(store.cty)

store.df$storeNum = rep(store.num, each=k.weeks)
store.df$storeNum
store.df$country = rep(store.cty, each=k.weeks)
store.df$country
rm(store.num, store.cty)  # clean up

store.df$Week = rep(1:52, time=k.stores*2)  # 1~52をk.stores*2回繰り返す
store.df$Week
store.df$Year = rep(rep(1:2, each=k.weeks/2), times=k.stores)  # (1をk.weeks/2回、2をk.weeks/2回)をk.storesを繰り返す
store.df$Year

str(store.df)

store.df$storeNum = factor(store.df$storeNum)
store.df$country = factor(store.df$country)
str(store.df)

head(store.df)
head(store.df, 120)
tail(store.df, 120)


# シュミレーションデータの作成
set.seed(98250)

store.df$p1prom = rbinom(n=nrow(store.df), size=1, p=0.1)
store.df$p2prom = rbinom(n=nrow(store.df), size=1, p=0.15)

store.df$p1price = sample(x=c(2.19, 2.29, 2.49, 2.79, 2.99), size=nrow(store.df), replace=TRUE)
store.df$p2price = sample(x=c(2.29, 2.49, 2.59, 2.99, 3.19), size=nrow(store.df), replace=TRUE)
head(store.df)

tmp.sales1 = rpois(nrow(store.df), lambda=120)  # lambda = mean of sales per week
tmp.sales2 = rpois(nrow(store.df), lambda=100)

tmp.sales1 = tmp.sales1 * log(store.df$p2price) / log(store.df$p1price)
tmp.sales2 = tmp.sales2 * log(store.df$p1price) / log(store.df$p2price)

store.df$p1sales = floor(tmp.sales1 * (1 + store.df$p1prom*0.3))
store.df$p2sales = floor(tmp.sales2 * (1 + store.df$p2prom*0.4))
head(store.df)

library(car)
some(store.df, 10)
