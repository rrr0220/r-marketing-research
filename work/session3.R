

# session3-1 -------------------------------------------------------------

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
str(store.df)


# session3-2 --------------------------------------------------------------

# 離散変数
table(store.df$p1price)
p1.table = table(store.df$p1price)
str(p1.table)
plot(p1.table)

table(store.df$p1price, store.df$p1prom)
p1.table2 = table(store.df$p1price, store.df$p1prom)
p1.table2[, 2] / (p1.table2[, 1] + p1.table2[, 2])

# 連続関数
min(store.df$p1sales)
max(store.df$p2sales)
mean(store.df$p1prom)
median(store.df$p2sales)
var(store.df$p1sales)
sd(store.df$p1sales)
IQR(store.df$p1sales)
mad(store.df$p1sales)
quantile(store.df$p1sales, probs=c(0.25, 0.50, 0.75))
quantile(store.df$p1sales, probs=c(0.05, 0.95))
quantile(store.df$p1sales, probs=0:10/10)
quantile(store.df$p1sales, probs=seq(from=0, to=1, by=0.1))
quantile(store.df$p1sales, probs=seq(0, 1, 0.1))

mysummary.df = data.frame(matrix(NA, nrow=2, ncol=2))
names(mysummary.df) = c("Median Sales", "IQR")
rownames(mysummary.df) = c("Product 1", "Product 2")
mysummary.df["Product 1", "Median Sales"] = median(store.df$p1sales)
mysummary.df["Product 2", "Median Sales"] = median(store.df$p2sales)
mysummary.df["Product 1", "IQR"] = IQR(store.df$p1sales)
mysummary.df["Product 2", "IQR"] = IQR(store.df$p2sales)
mysummary.df



# session3-3 --------------------------------------------------------------

summary(store.df)
summary(store.df$Year)
summary(store.df, digits=2)

describe(store.df)
describe(store.df[, c(2, 4:9)])

apply(store.df[, 2:9], MARGIN=2, FUN=mean)
apply(store.df[, 2:9], MARGIN=1, FUN=mean)
apply(store.df[, 2:9], MARGIN=2, FUN=sum)
apply(store.df[, 2:9], MARGIN=2, FUN=sd)
apply(store.df[, 2:9], MARGIN=2, FUN=function(x){mean(x)-median(x)})


# session3-4 --------------------------------------------------------------

# ヒストグラム

hist(store.df$p1sales)

hist(store.df$p1sales,
     main="Product 1 Weekly Sales Frequencies, All stores",
     xlab="Product 1 Sales (Units)",
     ylab="Count")

hist(store.df$p1sales,
     main="Product 1 Weekly Sales Frequencies, All stores",
     xlab="Product 1 Sales (Units)",
     ylab="Count",
     breaks=30,
     col="lightblue")

hist(store.df$p1sales,
     main="Product 1 Weekly Sales Frequencies, All stores",
     xlab="Product 1 Sales (Units)",
     ylab="Count",
     breaks=30,
     col="lightblue",
     freq=FALSE,
     xaxt="n")
axis(side=1, at=seq(60, 300, by=20))
lines(density(store.df$p1sales, bw=10),
      type="l",
      col="darkred",
      lwd=2)

hist(store.df$p2sales,
     main="Product 2 Weekly Sales Frequencies, All stores",
     xlab="Product 2 Sales (Units)",
     ylab="Count",
     breaks=30,
     col="lightblue",
     freq=FALSE,
     xaxt="n")
axis(side=1, at=seq(40, 300, by=20))
lines(density(store.df$p2sales, bw=10),
      type="l",
      col="darkred",
      lwd=2)


# 二つ並べる場合

def.par <- par(no.readonly = TRUE)  # リセット用
par(mfcol=c(1, 2))
# layout(matrix(1:2, ncol=2))
?par

hist(store.df$p1sales,
     main="Product 1 Weekly Sales Frequencies, All stores",
     xlab="Product 1 Sales (Units)",
     ylab="Count",
     breaks=30,
     col="lightblue",
     freq=FALSE,
     xlim=c(40, 260),
     ylim=c(0, 0.025))

lines(density(store.df$p1sales, bw=10),
      type="l",
      col="darkred",
      lwd=2)

hist(store.df$p2sales,
     main="Product 2 Weekly Sales Frequencies, All stores",
     xlab="Product 2 Sales (Units)",
     ylab="Count",
     breaks=30,
     col="lightblue",
     freq=FALSE,
     xlim=c(40, 260),
     ylim=c(0, 0.025))
     
lines(density(store.df$p2sales, bw=10),
      type="l",
      col="darkred",
      lwd=2)

par(def.par)  # デフォルトに戻す


# 箱ひげ図

boxplot(store.df$p2sales, xlab="Weekly sales", ylab="P2",
        main="Weekly sales of P2, All stores", horizontal=TRUE)

boxplot(store.df$p2sales ~ store.df$storeNum, horizontal=TRUE,
        ylab="Store", xlab="Weekly unit sales", las=1,
        main="Weekly sales of P2, by store")

boxplot(p2sales ~ p2prom, data=store.df, horizontal=TRUE, yaxt="n",
        ylab="P2 promoted in store?", xlab="Weekly sales",
        main="Weekly sales of P2 witn and without promotion")
axis(side=2, at=c(1, 2), labels=c("No", "Yes"))


library(beanplot)
beanplot(p2sales ~ p2prom, data=store.df, horizontal=TRUE, yaxt="n",
         what=c(0, 1, 1, 0), log="", side="second",
         ylab="P2 promoted in store?", xlab="Weekly sales",
         main="Weekly sales of P2 with and without promotion",
         col="lightblue")
axis(side=2, at=c(1,2), labels=c("No", "Yes"))


# Q-Qプロット
qqnorm(store.df$p1sales)
qqline(store.df$p1sales)

qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))


# 分布関数
plot(ecdf(store.df$p1sales),
     main="Cumulative distribution of P1 Weekly Sales",
     ylab="Cumulative Proportion",
     xlab=c("P1 weekly sales, all stores",
            "90% of weeks sold <= 171 units"),
     yaxt="n")
axis(side=2, at=seq(0, 1, by=0.1), las=1,
     labels=paste(seq(0, 100, by=10), "%", sep=""))
abline(h=0.9, lty=3)
abline(v=quantile(store.df$p1sales, pr=0.9), lty=3)

?quantile


# byとaggregate

by(store.df$p1sales, store.df$storeNum, mean)
by(store.df$p1sales, list(store.df$storeNum, store.df$Year), mean)

aggregate(store.df$p1sales, by=list(country=store.df$country), sum)

p1sales.sum = aggregate(store.df$p1sales, by=list(country=store.df$country), sum)
p1sales.sum

# 地図の利用

library(rworldmap)
library(RColorBrewer)

p1sales.map = joinCountryData2Map(p1sales.sum, joinCode="ISO2", nameJoinColumn="country")

mapCountryData(p1sales.map, nameColumnToPlot="x",
               mapTitle="Total P1 sales by Country",
               colourPalette=brewer.pal(7, "Greens"),
               catMethod="fixedWidth", addLegend=FALSE)







