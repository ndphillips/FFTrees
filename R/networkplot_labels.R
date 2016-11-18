locations

locations <- data.frame(cbind(runif(10, -10, 10),
                   runif(10, -10, 10)))

xlim <- c(-10, 10)
ylim <- c(-10, 10)


locations$label.x <- NA
locations$label.y <- NA

desired.radius <- 1.5
h <- 1

for (i in 1:nrow(locations)) {

  # Get location of current node
  i.loc <- unlist(locations[i,])

  # Get dataframe of potential locations
  potential <- data.frame(x = i.loc[1] + cos(seq(0, 2 * pi, length = 72)) * desired.radius,
                          y = i.loc[2] + sin(seq(0, 2 * pi, length = 72)) * desired.radius,
                          point.penalty = 0,
                          label.penalty = 0,
                          total.penalty = 0)


  # Give penalties based on distance from nearby points
  pen.dist <- .05

  point.x.pen <- sapply(1:nrow(potential), FUN = function(x) {

    sum(abs(potential$x[x] - locations[,1]) < diff(xlim) * pen.dist) - 1

  })

  point.y.pen <- sapply(1:nrow(potential), FUN = function(x) {

    sum(abs(potential$y[x] - locations[,2]) < diff(ylim) * pen.dist) - 1

  })

  potential$point.penalty <- point.x.pen + point.y.pen

  # Add penalty from existing labels

  if (i > 1) {

    label.x.pen <- sapply(1:nrow(potential), FUN = function(x) {

      sum(abs(potential$x[x] - locations$label.x[1:(i - 1)]) < diff(xlim) * pen.dist)

    })

    label.y.pen <- sapply(1:nrow(potential), FUN = function(x) {

      sum(abs(potential$y[x] - locations$label.y[1:(i - 1)]) < diff(ylim) * pen.dist)

    })

    label.pen <- label.x.pen + label.y.pen

    potential$label.penalty <- label.pen

  }

  potential$total.penalty <- with(potential, point.penalty + label.penalty)

  slot.i <- which(potential$total.penalty == min(potential$total.penalty))

  if(length(slot.i) > 1) {slot.i <- sample(slot.i, 1)}

  locations$label.x[i] <- potential$x[slot.i]
  locations$label.y[i] <- potential$y[slot.i]


}

# Add label locations to locations


plot(1, xlim = xlim, ylim = ylim, type = "n")

# Add points
points(locations[,1],
       locations[,2],
       pch = paste(1:nrow(locations)))

# Add segments

# Add labels

for(i in 1:nrow(locations)) {

  text(locations$label.x[i],
       locations$label.y[i],
       labels = letters[i])

  }

#

