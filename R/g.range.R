`g.range` <-
function (cal.deskott)
#################################################################################
#  Dato un oggetto di classe kott.cal.design, calcola il range degli g-weights  #
#  sul dataframe originario e su tutti i random samples.                        #
#################################################################################
{
    if (!inherits(cal.deskott, "kott.cal.design")) 
        stop("Object ", substitute(cal.deskott), " must be of class kott.cal.design")
    nrg <- attr(cal.deskott, "nrg")
    w.cal.char <- as.character(attr(cal.deskott, "weights"))[2]
    w.char <- unlist(strsplit(w.cal.char, ".cal"))
    range.r <- function(cal.deskott, w, w.cal, r) {
    ################################################
    #  Dato un oggetto di classe kott.cal.design,  #
    #  calcola il range degli g-weights sul suo    #
    #  r-esimo random sample.                      #
    ################################################
        wr <- cal.deskott[, paste(w, r, sep = "")]
        w.calr <- cal.deskott[, paste(w.cal, r, sep = "")]
        wr.notzero <- (wr != 0)
        w.calr.notzero <- (w.calr != 0)
        ranger <- range(abs(w.calr[w.calr.notzero]/wr[wr.notzero]))
        # check: ci sono pesi finali non nulli corrispondenti a pesi iniziali nulli?
        if (any(wr[w.calr.notzero] == 0)) 
            ranger[2] <- Inf
        # check: ci sono pesi finali nulli corrispondenti a pesi iniziali non nulli?
        if (any(w.calr[wr.notzero] == 0)) 
            ranger[1] <- 0
        ranger
    }
    # original data
    out.matrix <- rbind(range.r(cal.deskott, w.char, w.cal.char,
        NULL))
    # replicated data
    lapply(1:nrg, function(r) out.matrix <<- rbind(out.matrix,
        range.r(cal.deskott, w.char, w.cal.char, r)))
    dimnames(out.matrix) <- list(NULL, c("g.min", "g.max"))
    names.col <- data.frame(sample = c("original", paste("replica.",
        1:nrg, sep = "")))
    cbind(names.col, out.matrix)
}