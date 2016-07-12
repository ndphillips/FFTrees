# simfftplot
#' Plots the results of the simfft function
#'
#' @param simfft.result A dataframe returned by the simfft function
#' @param roc A logical value indicating whether to plot an pseudo ROC curve (roc = T) or a plot showing how often cues were used (roc = F)
#' @param which.data Which data should be plotted in the roc? Can be "train" or "test" (this is ignored if roc = F)
#' @param lr,cart Logical values indicating whether or not to plot the logistic regression and CART ROCs (ignored if roc = F)
#' @importFrom stats anova sd
#' @importFrom graphics text points abline legend mtext plot contour
#' @importFrom grDevices topo.colors rgb
#'
#' @export
#'

simfftplot <- function(simfft.result,
                       roc = F,
                       lr = F,
                       cart = F,
                       which.data = "test"
                       )

{

  #
  # bcancer.fft.sim
  # roc = T
  # lr = T
  # cart = T
  # which.data = "train"
  #

  roc.p <- .8

    fft.t.col <- "#A1C72032"
    lr.t.col <- "#0C5BB032"
    cart.t.col <- "#EE001132"

    fft.col <- "#A1C720FF"
    lr.col <- "#0C5BB0FF"
    cart.col <- "#EE0011FF"


    if(roc == F) {

    # FFT selected cues
    {

    par(mar = c(5, 4, 4, 1) + .1)

    cues.selected <- unlist(strsplit(unlist(simfft.result$fft.level.name), split = ";"))

    cue.select.rank <- data.frame("cue" = unique(cues.selected))

    cue.select.rank$select.p <- sapply(unique(cues.selected), FUN = function(x) {sum(cues.selected == x) / nrow(simfft.result)})

    cue.select.rank <- cue.select.rank[order(cue.select.rank$select.p, decreasing = T),]

    n.cues.to.plot <- min(c(10, nrow(cue.select.rank)))

    plot(1, xlim = c(0, n.cues.to.plot + 1), ylim = c(0, 1), xlab = "cue", ylab = "p(used in FFT)",
         type = "n", bty = "n", xaxt = "n", yaxt = "n", main = "Cue FFT selection probability")

    axis(2, at = seq(0, 1, .2), lwd = 0)

    abline(h = seq(0, 1, .1), lty = 2, lwd = c(1, .5), col = "gray")


    for(i in 1:n.cues.to.plot) {

      cue.title <- substr(cue.select.rank$cue[i], 1, 5)

      mtext(cue.title,
            side = 1,
            line = .5,
            at = i

      )
    }

    cols <- topo.colors(101, alpha = .7)
    col.vec <- cols[round(1 - cue.select.rank$select.p[1:10], 2) * 100 + 1]


    rect(1:n.cues.to.plot - .25, rep(0, n.cues.to.plot), 1:n.cues.to.plot + .25, cue.select.rank$select.p[1:n.cues.to.plot],
         col = col.vec
    )
    }

    }

    if(roc == T) {

    # ROC Curve
    {

    add.contour <- function(x, y, p.vec, col.vec, ...) {

      if(sd(x) != 0 & sd(y) != 0) {

      f <- MASS::kde2d(x, y, n = 20, lims = c(0, 1, 0, 1))
      f$z <- f$z / sum(f$z)

      z.vec <- as.vector(f$z)
      z.vec <- sort(z.vec, decreasing = T)
      z.cum.vec <- cumsum(z.vec)

      levels.vec <- rep(NA, length(p.vec))

      for (i in 1:length(p.vec)) {

        z.cum.vec.t <- abs(z.cum.vec - p.vec[i])
        index <- min(which(z.cum.vec.t == min(z.cum.vec.t)))

        levels.vec[i] <- z.vec[index]

      }

      contour(f, add = T, levels = levels.vec, labels = p.vec, col = col.vec[1], ...)

      }

      points(mean(x), mean(y), col = col.vec[1], cex = 2, pch = 3, lwd = 3)
      points(x, y, col = col.vec[2], pch = 16)

    }

    par(mar = c(5, 4, 4, 1) + .1)

    if(which.data == "train") {main.text <- "Simulated training (fitting) performance"}
    if(which.data == "test") {main.text <- "Simulated testing (prediction) Performance"}



    plot(1, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "False Alarm Rate (FAR)",
         ylab = "Hit Rate (HR)", main = main.text, yaxt = "n", xaxt = "n")

    axis(side = 2, at = seq(0, 1, .1), lwd = 0, las = 1, lwd.ticks = 1)
    axis(side = 1, at = seq(0, 1, .1), lwd = 0, las = 1, lwd.ticks = 1)


    abline(h = seq(0, 1, .1), lwd = c(1, .5), col = gray(.9))
    abline(v = seq(0, 1, .1), lwd = c(1, .5), col = gray(.9))

    abline(a = 0, b = 1, lty = 2)

    if(which.data == "test") {

    add.contour(simfft.result$fft.far.test,
                simfft.result$fft.hr.test,
                p.vec = roc.p,
                col.vec = c(fft.col, fft.t.col))




    if(is.element("lr.far.test", names(simfft.result)) & lr) {

      add.contour(simfft.result$lr.far.test,
                  simfft.result$lr.hr.test,
                  p.vec =  roc.p, col.vec = c(lr.col, lr.t.col))

    }

    if(is.element("cart.far.test", names(simfft.result)) & cart) {

      add.contour(simfft.result$cart.far.test, simfft.result$cart.hr.test, p.vec =  roc.p, col.vec = c(cart.col, cart.t.col))

    }


    }

    if(which.data == "train") {

      add.contour(simfft.result$fft.far.train,
                  simfft.result$fft.hr.train,
                  p.vec = roc.p,
                  col.vec = c(fft.col, fft.t.col))




      if(is.element("lr.far.train", names(simfft.result)) & lr) {

        add.contour(simfft.result$lr.far.train,
                    simfft.result$lr.hr.train,
                    p.vec =  roc.p,
                    col.vec = c(lr.col, lr.t.col))

      }

      if(is.element("cart.far.train", names(simfft.result)) & cart) {

        add.contour(simfft.result$cart.far.train,
                    simfft.result$cart.hr.train,
                    p.vec =  roc.p,
                    col.vec = c(cart.col, cart.t.col))

      }


    }




    if(lr | cart) {

    legend(x = .7,
           y = .3,
           legend = c("FFT", "Log Reg", "CART"),
           bty = "n",
           yjust = .5,
           col = c(fft.col, lr.col, cart.col),
           lwd = 2

           )

    }








    }

    }




}


