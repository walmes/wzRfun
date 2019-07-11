#' @export
#' @name enc_titles
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @title Applies \code{enc{}{}} to non ASCII words
#' @description Non ASCII words (commom in portuguese) trigers warnings
#'     in \code{R CMD CHECK}. To prevent this, \code{enc{}{}} must be
#'     used. This function gets a character and returns \code{enc{}{}}
#'     for all non ASCII words.
#' @param x character[1] A title to be converted.
#' @return character[1] The converted character provided.
#' @examples
#'
#' library(proto)
#' library(gsubfn)
#'
#' cat(use_enc("Um título com conteúdo acenturado para testar a função"), "\n")
#'

#' @rdname enc_titles
#' @export
use_enc <- function(x) {
    if (!requireNamespace("proto", quietly = TRUE)) {
        stop(paste0("`proto` needed for this function to work.",
                    " Please install it."),
             call. = FALSE)
    }
    if (!requireNamespace("gsubfn", quietly = TRUE)) {
        stop(paste0("`gsubfn` needed for this function to work.",
                    " Please install it."),
             call. = FALSE)
    }
    f <- proto::proto(fun = function(this, x1, x2, x3) {
        # Se tiver o \\enc{ é porque já foi substituído.
        g <- grepl(x = c(x1, x2, x3),
                   pattern = "(\\\\enc\\{|\\}\\{)")
        # Testa se ocorre um "\enc{" ou um "}{".
        if (any(g)) {
            # Se sim, retorna como está.
            paste0(x1, x2, x3)
        } else {
            # Se não, aplica o \enc{}{}.
            sprintf(fmt = "%s\\enc{%s}{%s}%s",
                    x1, x2, iconv(x2, to = "ASCII//TRANSLIT"), x3)
        }
    })
    gsubfn::gsubfn(pattern = "([[:ascii:]]*)([^[:ascii:]]+)([[:ascii:]]*)",
                   replacement = f,
                   x = x)
}

#' @rdname enc_titles
#' @export
#' @param file character[1] A R file with documentation written in
#'     roxygen syntax. \code{enc_nize()} assumes that title is signed
#'     with \code{@title} tag. If it is not the case, the function give
#'     a warning. This function returns \code{NULL} and modifies files
#'     in disc. Be careful with its use.
enc_nize <- function(file) {
    # Lê o arquivo.
    text <- readLines(file)
    # Delimita para a região do @title.
    i <- grep(x = text, pattern = "^#' +@title")
    j <- grep(x = text, pattern = "^#' +@[a-z]+")
    if (length(i) * length(j) > 0) {
        j <- min(j[j > i]) - 1
        r <- i:j
        txt <- text[r]
        if (j > i) {
            txt <- paste(c("#'",
                           gsub(x = txt,
                                pattern = "#' +",
                                replacement = "")),
                         collapse = " ")
        }
        # Faz a substituição quando o padrão bater.
        txt <- use_enc(txt)
        # Quebra texto para não ultrapassar 72 de comprimento.
        txt <- strwrap(txt, width = 72)
        # Indenta negativo da segunda linha para frente.
        if (length(txt) > 1) {
            txt[-1] <- paste0("#'     ", txt[-1])
        }
        cat(txt, sep = "\n", "\n")
        # Escreve as modificações no arquivo.
        text <- text[-r]
        text <- append(x = text, values = txt, after = i - 1)
        writeLines(text = text, con = file)
    } else {
        warning("No `@title` tag were found.")
    }
    invisible(NULL)
}

#' @rdname enc_titles
#' @export
#' @details \code{enc_titles} applies \code{enc_nize} to all files in
#'     the \code{./R/} directory of a package. So, it is assumed that
#'     working directory is a root package directory. \code{enc_titles}
#'     modifies files. So be careful with its use.
enc_titles <- function() {
    # Todos os arquivos.
    url <- list.files(path = "./R", full.names = TRUE)
    # Aplica recursivamente.
    sapply(url, FUN = enc_nize)
}
