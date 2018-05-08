# most recent traced value
latest_value <- function (data) {
  data$values[data$iterations_complete]
}

# has the latest datapoint exceeded ylim?
latest_out_of_range <- function (data) {
  value <- latest_value(data)
  value < data$ylim[1] | value > data$ylim[2]
}

# expand the range if needed
pminmax <- function (range, x) {
  c(min(range[1], x),
    max(range[2], x))
}

# get the new value of ylim
update_ylim <- function (data) {
  data$ylim <- pminmax(data$ylim,
                       latest_value(data))
  data
}

# setup the plot
setup_plot <- function (data) {

  plot.new()
  plot.window(xlim = c(1, data$max_iterations),
              ylim = data$ylim)
  axis(1)
  axis(2, las = 2)
  title(xlab = "iterations")
  mtext(data$name,
        side = 3, line = 1, adj = 0.95,
        cex = 2, col = grey(0.4))

}

# create a data structure to track progress
setup_data <- function (name, max_iterations, initial = 0, ylim = c(-2, 2)) {
  list(name = name,
       ylim = ylim,
       iterations_complete = 0,
       max_iterations = max_iterations,
       values = c(initial, rep(NA, max_iterations - 1)))
}

# fake a step of mcmc
mcmc_step <- function (data) {
  iteration <- data$iterations_complete + 1
  data$values[iteration] <- rnorm(1)
  data$iterations_complete <- iteration
  Sys.sleep(0.1)
  data
}

# redo the whole plot so far (e.g. if the limits have been reached)
rebuild_plot <- function (data) {
  setup_plot(data)
  add_lines(data, "all")
}

# add some of all lines to the plot
add_lines <- function (data, which = c("all", "latest")) {

  which <- match.arg(which)

  # which segments to draw
  iteration <- data$iterations_complete
  index <- switch (which,
                   all = seq_len(iteration),
                   latest = iteration - 1:0)

  # draw the lines
  lines(index,
        data$values[index],
        lwd = 1,
        col = greta:::greta_col())

}

# main function to update the plot, rebuilding it if the ylim is no longer valid
update_plot <- function (data) {

  # can't plot one point
  if (data$iterations_complete > 1) {

    # rebuild the plot if it's exceeded ylim
    if (latest_out_of_range(data)) {
      data <- update_ylim(data)
      rebuild_plot(data)
    } else {
      add_lines(data, "latest")
    }

  }
  data
}

demo <- function () {

  data <- setup_data("alpha", 500)
  setup_plot(data)

  # main loop
  for (iteration in seq_len(data$max_iterations)) {
    data <- mcmc_step(data)
    data <- update_plot(data)
  }

}

# demo()
