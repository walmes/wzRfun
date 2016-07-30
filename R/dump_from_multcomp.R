insert_absorb <-
function (x, Letters = c(letters, LETTERS), separator = ".", 
    decreasing = FALSE, comps = NULL, lvl_order) 
{
    obj_x <- deparse(substitute(x))
    if (is.null(comps)) {
        namx <- names(x)
        namx <- gsub(" ", "", names(x))
        if (length(namx) != length(x)) 
            stop("Names required for ", obj_x)
        split_names <- strsplit(namx, "-")
        stopifnot(sapply(split_names, length) == 2)
        comps <- t(as.matrix(as.data.frame(split_names)))
    }
    rownames(comps) <- names(x)
    lvls <- lvl_order
    n <- length(lvls)
    lmat <- array(TRUE, dim = c(n, 1), dimnames = list(lvls, 
        NULL))
    if (sum(x) == 0) {
        ltrs <- rep(get_letters(1, Letters = Letters, separator = separator), 
            length(lvls))
        names(ltrs) <- lvls
        colnames(lmat) <- ltrs[1]
        msl <- ltrs
        ret <- list(Letters = ltrs, monospacedLetters = msl, 
            LetterMatrix = lmat)
        class(ret) <- "multcompLetters"
        return(ret)
    }
    else {
        signifs <- comps[x, , drop = FALSE]
        absorb <- function(m) {
            for (j in 1:(ncol(m) - 1)) {
                for (k in (j + 1):ncol(m)) {
                  if (all(m[which(m[, k]), k] & m[which(m[, k]), 
                    j])) {
                    m <- m[, -k, drop = FALSE]
                    return(absorb(m))
                  }
                  else if (all(m[which(m[, j]), k] & m[which(m[, 
                    j]), j])) {
                    m <- m[, -j, drop = FALSE]
                    return(absorb(m))
                  }
                }
            }
            return(m)
        }
        for (i in 1:nrow(signifs)) {
            tmpcomp <- signifs[i, ]
            wassert <- which(lmat[tmpcomp[1], ] & lmat[tmpcomp[2], 
                ])
            if (any(wassert)) {
                tmpcols <- lmat[, wassert, drop = FALSE]
                tmpcols[tmpcomp[2], ] <- FALSE
                lmat[tmpcomp[1], wassert] <- FALSE
                lmat <- cbind(lmat, tmpcols)
                colnames(lmat) <- get_letters(ncol(lmat), Letters = Letters, 
                  separator = separator)
                if (ncol(lmat) > 1) {
                  lmat <- absorb(lmat)
                  colnames(lmat) <- get_letters(ncol(lmat), Letters = Letters, 
                    separator = separator)
                }
            }
        }
    }
    lmat <- lmat[, order(apply(lmat, 2, sum))]
    lmat <- sweepLetters(lmat)
    lmat <- lmat[, names(sort(apply(lmat, 2, function(x) return(min(which(x))))))]
    colnames(lmat) <- get_letters(ncol(lmat), Letters = Letters, 
        separator = separator)
    lmat <- lmat[, order(apply(lmat, 2, sum))]
    lmat <- sweepLetters(lmat)
    lmat <- lmat[, names(sort(apply(lmat, 2, function(x) return(min(which(x)))), 
        decreasing = decreasing))]
    colnames(lmat) <- get_letters(ncol(lmat), Letters = Letters, 
        separator = separator)
    ltrs <- apply(lmat, 1, function(x) return(paste(names(x)[which(x)], 
        sep = "", collapse = "")))
    msl <- matrix(ncol = ncol(lmat), nrow = nrow(lmat))
    for (i in 1:nrow(lmat)) {
        msl[i, which(lmat[i, ])] <- colnames(lmat)[which(lmat[i, 
            ])]
        absent <- which(!lmat[i, ])
        if (length(absent) < 2) {
            if (length(absent) == 0) 
                next
            else {
                msl[i, absent] <- paste(rep(" ", nchar(colnames(lmat)[absent])), 
                  collapse = "")
            }
        }
        else {
            msl[i, absent] <- unlist(lapply(sapply(nchar(colnames(lmat)[absent]), 
                function(x) return(rep(" ", x))), paste, collapse = ""))
        }
    }
    msl <- apply(msl, 1, paste, collapse = "")
    names(msl) <- rownames(lmat)
    ret <- list(Letters = ltrs, monospacedLetters = msl, LetterMatrix = lmat, 
        aLetters = Letters, aseparator = separator)
    class(ret) <- "multcompLetters"
    return(ret)
}
get_letters <-
function (n, Letters = c(letters, LETTERS), separator = ".") 
{
    n.complete <- floor(n/length(Letters))
    n.partial <- n%%length(Letters)
    lett <- character()
    separ = ""
    if (n.complete > 0) {
        for (i in 1:n.complete) {
            lett <- c(lett, paste(separ, Letters, sep = ""))
            separ <- paste(separ, separator, sep = "")
        }
    }
    if (n.partial > 0) 
        lett <- c(lett, paste(separ, Letters[1:n.partial], sep = ""))
    return(lett)
}
sweepLetters <-
function (mat, start.col = 1, Letters = c(letters, LETTERS), 
    separator = ".") 
{
    stopifnot(all(start.col %in% 1:ncol(mat)))
    locked <- matrix(rep(0, ncol(mat) * nrow(mat)), ncol = ncol(mat))
    cols <- 1:ncol(mat)
    cols <- cols[c(start.col, cols[-start.col])]
    if (any(is.na(cols))) 
        cols <- cols[-which(is.na(cols))]
    for (i in cols) {
        tmp <- matrix(rep(0, ncol(mat) * nrow(mat)), ncol = ncol(mat))
        tmp[which(mat[, i]), ] <- mat[which(mat[, i]), ]
        one <- which(tmp[, i] == 1)
        if (all(apply(tmp[, -i, drop = FALSE], 1, function(x) return(any(x == 
            1))))) {
            next
        }
        for (j in one) {
            if (locked[j, i] == 1) {
                next
            }
            chck <- 0
            lck <- list()
            for (k in one) {
                if (j == k) {
                  next
                }
                else {
                  rows <- tmp[c(j, k), ]
                  dbl <- rows[1, ] & rows[2, ]
                  hit <- which(dbl)
                  hit <- hit[-which(hit == i)]
                  dbl <- rows[1, -i, drop = FALSE] & rows[2, 
                    -i, drop = FALSE]
                  if (any(dbl)) {
                    chck <- chck + 1
                    lck[[chck]] <- list(c(j, hit[length(hit)]), 
                      c(k, hit[length(hit)]))
                  }
                }
            }
            if ((chck == (length(one) - 1)) && chck != 0) {
                for (k in 1:length(lck)) {
                  locked[lck[[k]][[1]][1], lck[[k]][[1]][2]] <- 1
                  locked[lck[[k]][[2]][1], lck[[k]][[2]][2]] <- 1
                }
                mat[j, i] <- FALSE
            }
        }
        if (all(mat[, i] == FALSE)) {
            mat <- mat[, -i, drop = FALSE]
            colnames(mat) <- get_letters(ncol(mat), Letters = Letters, 
                separator = separator)
            return(sweepLetters(mat, Letters = Letters, separator = separator))
        }
    }
    onlyF <- apply(mat, 2, function(x) return(all(!x)))
    if (any(onlyF)) {
        mat <- mat[, -which(onlyF), drop = FALSE]
        colnames(mat) <- get_letters(ncol(mat), Letters = Letters, 
            separator = separator)
    }
    return(mat)
}
