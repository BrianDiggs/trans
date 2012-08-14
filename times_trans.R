#options(width=65)
library("ggplot2")
library("scales")
library("chron")

methods(class="times")


    Time <- times(c("18:37:11", "16:51:34", "15:05:57", "13:20:20",
                    "11:34:43", "09:49:06", "08:03:29", "06:17:52",
                    "04:32:15", "02:46:38", "01:01:01"))

Time
str(Time)
dput(Time)

as.numeric(Time)
times(as.numeric(Time))
identical(Time, times(as.numeric(Time)))

pretty(Time)
pretty_breaks()(range(Time))

    fmt <- function(x) {
        format(x, simplify = !any(diff(x) < 1/(24*60)))
    }

    times_trans <- function() {
        fmt <- function(x) {
            format(x, simplify = !any(diff(x) < 1/(24*60)))
        }
        trans_new("chrontimes",
                  transform = as.numeric,
                  inverse = times,
                  breaks = pretty_breaks(),
                  format = fmt,
                  domain=c(0,1))
    }

    dat <- data.frame(time = Time,
                      value = c(7L, 6L, 9L, 11L, 10L, 1L,
                                4L, 2L, 3L, 5L, 8L))

    ggplot(dat, aes(time, value)) + geom_point()

ggsave("default_scale.png", width = 6, height = 6)

    ggplot(dat, aes(time, value)) + geom_point() +
      scale_x_continuous(trans=times_trans())

ggsave("trans_scale.png", width = 6, height = 6)


    timesreverse_trans <- function() {
        trans <- function(x) {-as.numeric(x)}
        inv <- function(x) {times(-x)}
        fmt <- function(x) {format(x, simplify = !any(diff(x) < 1/(24*60)))}
        trans_new("chrontimes-reverse",
                  transform = trans,
                  inverse = inv,
                  breaks = pretty_breaks(),
                  format = fmt,
                  domain=c(0,1))
    }


    scale_x_times <- function(..., trans=NULL) {
        scale_x_continuous(trans=times_trans(), ...)
    }

    scale_y_times <- function(..., trans=NULL) {
        scale_y_continuous(trans=timesreverse_trans(), ...)
    }

    ggplot(dat, aes(time, value)) + geom_point() +
      scale_x_times()

ggsave("scale_x_times.png", width = 6, height = 6)

    ggplot(dat, aes(value, time)) + geom_point() +
      scale_y_times()

ggsave("scale_y_times.png", width = 6, height = 6)

