

# session4-1 --------------------------------------------------------------

# 顧客データの作成
set.seed(21821)
ncust = 1000
cust.df = data.frame(cust.id=factor(1:ncust))
head(cust.df)

cust.df$age = rnorm(n=ncust, mean=35, sd=5)
cust.df$credit.score = rnorm(n=ncust, mean=3*cust.df$age+620, sd=50)
cust.df$email = factor(sample(c("yes", "no"), size=ncust, replace=TRUE, prob=c(0.8, 0.2)))
cust.df$distance.to.store = exp(rnorm(n=ncust, mean=2, sd=1.2))

# データフレームのチェック
dim(cust.df)
head(cust.df)
tail(cust.df)
some(cust.df)
str(cust.df)
summary(cust.df)
describe(cust.df)

cust.df$online.visits = 
  rnbinom(ncust, size=0.3,
          mu=15+ifelse(cust.df$email=="yes", 15, 0)-0.7*(cust.df$age-median(cust.df$age)))

cust.df$online.trans = 
  rnbinom(ncust, size=cust.df$online.visits, prob=0.3)

cust.df$online.spend =
  exp(rnorm(ncust, mean=3, sd=0.1)) * cust.df$online.trans

cust.df$store.trans =
  rnbinom(ncust, size=5, mu=3/sqrt(cust.df$distance.to.store))

cust.df$store.spend =
  exp(rnorm(ncust, mean=3.5, sd=0.4)) * cust.df$store.trans

# データフレームのチェック
dim(cust.df)
head(cust.df)
tail(cust.df)
some(cust.df)
str(cust.df)
summary(cust.df)
describe(cust.df)

# 満足度調査への回答を作成
sat.overall = rnorm(ncust, mean=3.1, sd=0.7)
summary(sat.overall)
sat.service = floor(sat.overall + rnorm(ncust, mean=0.5, sd=0.4))
sat.selection = floor(sat.overall + rnorm(ncust, mean=-0.2, sd=0.6))
summary(cbind(sat.service, sat.selection))

sat.service[sat.service > 5] = 5
sat.service[sat.service < 1] = 1
sat.selection[sat.selection > 5] = 5
sat.selection[sat.selection < 1] = 1
summary(cbind(sat.service, sat.selection))

# 非回答データ
no.response = as.logical(rbinom(ncust, size=1, prob=cust.df$age/100))
no.response
sat.service[no.response] = NA
sat.selection[no.response] = NA
summary(cbind(sat.service, sat.selection))

cust.df$sat.service = sat.service
cust.df$sat.selection = sat.selection
summary(cust.df)

rm(ncust, sat.overall, sat.service, sat.selection, no.response)

# 散布図でデータ間の関係を調べる
str(cust.df)

plot(x=cust.df$age, y=cust.df$credit.score)

plot(cust.df$age, cust.df$credit.score,
     col="blue",
     xlim=c(15, 55), ylim=c(500, 900),
     main="Active Customers as of June 2014",
     xlab="Customer Age (years)", ylab="Customer Credit Score")
abline(h=mean(cust.df$credit.score), col="dark blue", lty="dotted")
abline(v=mean(cust.df$age), col="dark blue", lty="dotted")

methods(plot)

par(mfcol=c(1, 2))

plot(cust.df$store.spend, cust.df$online.spend,
     main="Customers as of June 2014",
     xlab="Prior 12 months in-store sales ($)",
     ylab="Prior 12 months online sales ($)",
     cex=0.7)

plot(cust.df$store.spend, cust.df$online.spend,
     main="Customers as of June 2014",
     xlab="Prior 12 months in-store sales ($)",
     ylab="Prior 12 months online sales ($)")

par(mfcol=c(1, 1))

hist(cust.df$store.spend,
     breaks=(0:ceiling(max(cust.df$store.spend)/10))*10,
     main="Customers as of June 2014",
     xlab="Prior 12 months in-store sales ($)",
     ylab="Count of customers")

?ceiling

my.col = c("black", "green3")
my.pch = c(1, 19)

head(cust.df$email)
as.numeric(head(cust.df$email))

my.col[as.numeric(head(cust.df$email))]
my.col[head(cust.df$email)]

par(mfcol=c(1, 2))

plot(cust.df$store.spend, cust.df$online.spend,
     cex=0.7,
     col=my.col[cust.df$email],
     pch=my.pch[cust.df$email],
     main="Customers as of June 2014",
     xlab="Prior 12 months in-store sales ($)",
     ylab="Prior 12 months online sales ($)")

plot(cust.df$store.spend, cust.df$online.spend,
     cex=0.7,
     col=my.col[cust.df$email],
     main="Customers as of June 2014",
     xlab="Prior 12 months in-store sales ($)",
     ylab="Prior 12 months online sales ($)")

par(mfcol=c(1, 1))

# 凡例をつける

par(mfcol=c(1, 2))

plot(cust.df$store.spend, cust.df$online.spend,
     cex=0.7,
     col=my.col[cust.df$email],
     pch=my.pch[cust.df$email],
     main="Customers as of June 2014",
     xlab="Prior 12 months in-store sales ($)",
     ylab="Prior 12 months online sales ($)")
legend(x="topright",
       legend=paste("email on file:", levels(cust.df$email)),
       col=my.col, pch=my.pch)

# 対数軸に変換
plot(cust.df$store.spend + 1, cust.df$online.spend +1,
     log="xy",
     cex=0.7,
     col=my.col[cust.df$email],
     pch=my.pch[cust.df$email],
     main="Customers as of June 2014",
     xlab="Prior 12 months in-store sales ($)",
     ylab="Prior 12 months online sales ($)")
legend(x="topright",
       legend=paste("email on file:", levels(cust.df$email)),
       col=my.col, pch=my.pch)

par(mfcol=c(1, 1))

# 複数のグラフを１つのオブジェクトにまとめる

par(mfrow=c(2, 2))
plot(cust.df$distance.to.store, cust.df$store.spend, main="store")
plot(cust.df$distance.to.store, cust.df$online.spend, main="online")
plot(cust.df$distance.to.store, cust.df$store.spend + 1, log="xy", main="store, log")
plot(cust.df$distance.to.store, cust.df$online.spend + 1, log="xy", main="online, log")
par(mfcol=c(1, 1))



# session4-4 --------------------------------------------------------------

pairs(formula = ~ age + credit.score + email +
        distance.to.store + online.visits + online.trans +
        online.spend + store.trans + store.spend,
      data=cust.df)

pairs(cust.df[, c(2:10)])

library(car)
scatterplotMatrix(formula = ~ age + credit.score + email +
                    distance.to.store + online.visits + online.trans +
                    online.spend + store.trans + store.spend,
                  data=cust.df,
                  diagonal="histgram")



# session4-5 --------------------------------------------------------------

# 共分散
cov(cust.df$age, cust.df$credit.score)

# Pearsonの積率相関係数
cor(cust.df$age, cust.df$credit.score)

cov(cust.df$age, cust.df$credit.score)/((sd(cust.df$age))*(sd(cust.df$credit.score)))

# 相関係数の検定
cor.test(cust.df$age, cust.df$credit.score)

# 相関行列
cor(cust.df[, c(2, 3, 5:12)])
cor(cust.df[, c(2, 3, 5:12)], use="complete.obs")

library(corrplot)
library(gplots)
corrplot.mixed(corr=cor(cust.df[, c(2, 3, 5:12)], use="complete.obs"),
               upper="ellipse", 
               tl.pos="lt")
?corrplot.mixed

corrplot.mixed(corr=cor(cust.df[, c(2, 3, 5:12)], use="complete.obs"),
               upper="ellipse", 
               tl.pos="lt",
               lower.col=colorpanel(50, "red", "gray60", "blue4"),
               upper.col=colorpanel(50, "red", "gray60", "blue4"))


# 変数変換
set.seed(49931)
x = runif(1000, min=-10, max=10)
plot(x, x^2)
cor(x, x^2)

cor(cust.df$distance.to.store, cust.df$store.spend)
cor(1/cust.df$distance.to.store, cust.df$store.spend)
cor(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)

plot(cust.df$distance.to.store, cust.df$store.spend)
plot(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)


# Box-Cox変換
library(car)
powerTransform(cust.df$distance.to.store)
lambda = coef(powerTransform(cust.df$distance.to.store))
bcPower(cust.df$distance.to.store, lambda)
?coef

par(mfrow=c(1, 2))
hist(cust.df$distance.to.store,
     xlab="Distance to Nearest Store",
     ylab="Count of Customers",
     main="Original Distribution")
hist(bcPower(cust.df$distance.to.store, lambda),
     xlab="Box-Cox Transform of Distance",
     ylab="Count of Customers",
     main="Transformed Distribution")

l.dist = coef(powerTransform(cust.df$distance.to.store))
l.spend = coef(powerTransform(cust.df$store.spend+1))
cor(bcPower(cust.df$distance.to.store, l.dist),
    bcPower(cust.df$store.spend+1, l.spend))

par(mfrow=c(1, 1))

# session4-6 --------------------------------------------------------------

plot(cust.df$sat.service, cust.df$sat.selection,
     xlab="Customer Satisfaction with Service",
     ylab="Customer Satisfaction with Selecyion",
     main="Customers as of June 2014")

plot(jitter(cust.df$sat.service), jitter(cust.df$sat.selection),
     xlab="Customer Satisfaction with Service",
     ylab="Customer Satisfaction with Selecyion",
     main="Customers as of June 2014")


resp = !is.na(cust.df$sat.service)
cor(cust.df$sat.service[resp], cust.df$sat.selection[resp])
polychoric(cbind(cust.df$sat.service[resp], cust.df$sat.selection[resp]))
