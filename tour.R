# setwd("~/github/animinTour")
library(animint)
library(mvtnorm)
library(tourr)

s <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), ncol = 3)
m <- rmvnorm(300, sigma = s)
vars <- c("x", "y", "z")
d <- setNames(data.frame(m), vars)
d$density <- dmvnorm(m)
d$id <- seq_len(nrow(m))
# https://gist.github.com/cpsievert/4673815529b7a1e6c1aa
mat <- rescale(as.matrix(m))

tour <- new_tour(mat, grand_tour(), NULL)
steps <- c(0, rep(1/15, 200))
stepz <- cumsum(steps)
tour_dat <- function(step_size) {
  step <- tour(step_size)
  proj <- center(mat %*% step$proj)
  df <- data.frame(x = proj[,1], y = proj[,2], id = d$id)
  list(dat = df, proj = data.frame(step$proj, vars = vars))
}
dats <- lapply(steps, tour_dat)
datz <- Map(function(x, y) cbind(x$dat, step = y), dats, stepz)
dat <- do.call("rbind", datz)
projz <- Map(function(x, y) cbind(x$proj, step = y), dats, stepz)
projs <- do.call("rbind", projz)
projs$X1 <- round(projs$X1, 3)
projs$X2 <- round(projs$X2, 3)
p1 <- ggplot() +
  geom_point(data = d, size = 5, aes(x = x, y = density, clickSelects = id)) +
  geom_point(data = d, size = 5, color = "red",
             aes(x = x, y = density, clickSelects = id, showSelected = id)) +
  labs(x = NULL, y = NULL)
p2 <- ggplot() + 
  geom_point(data = dat, size = 5, alpha = 0.35,
             aes(x = x, y = y, showSelected = step)) +
  geom_point(data = dat, size = 5, color = "red", 
             aes(x = x, y = y, showSelected = step, showSelected2 = id)) +
  geom_segment(data = projs, alpha = 0.25, color = "blue",
               aes(x = 0, y = 0, xend = X1, yend = X2, showSelected = step)) +
  geom_text(data = projs, size = 15,
            aes(x = X1, y = X2, label = vars, showSelected = step)) +
  labs(x = NULL, y = NULL)
plist <- list(
  plot1 = p1,
  plot2 = p2,
  time = list(variable = "step", ms = 100),
  duration = list(step = 500),
  selector.types = list(
    id = "multiple"
  ),
  first = list(id = NULL)
)
animint2dir(plist, ".")
