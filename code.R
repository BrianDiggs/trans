library("ggplot2")
library("scales")

    qplot(log10(carat), log10(price), data=diamonds)
    qplot(carat, price, data=diamonds) +
      scale_x_log10() + scale_y_log10()

library("gridExtra")
p1 <- qplot(log10(carat), log10(price), data=diamonds)
p2 <- qplot(carat, price, data=diamonds) +
  scale_x_log10() + scale_y_log10()
png("data.vs.trans.compare.png", width=480, height=240)
grid.arrange(p1, p2, ncol=2)
dev.off()

    reverselog_trans <- function(base = exp(1)) {
        trans <- function(x) -log(x, base)
        inv <- function(x) base^(-x)
        trans_new(paste0("reverselog-", format(base)), trans, inv,
                  log_breaks(base = base), domain = c(1e-100, Inf))
    }

    dat <- data.frame(x=1:20, y=1:20)

    ggplot(dat, aes(x,y)) + geom_point() +
        scale_x_continuous(trans="reverselog")

    ggplot(dat, aes(x,y)) + geom_point() +
        scale_x_continuous(trans=reverselog_trans(base=2))

png("example-ln.png", ,width = 480, height = 480)
ggplot(dat, aes(x,y)) + geom_point() +
    scale_x_continuous(trans="reverselog")
dev.off()
png("example-ln2.png", width = 480, height = 480)
ggplot(dat, aes(x,y)) + geom_point() +
    scale_x_continuous(trans=reverselog_trans(base=2))
dev.off()


