# @author Walmes Zeviani, \email{walmes@@ufpr.br}.
# @description Returns the name of the file where to write the
#     documentation of an object.
# @param name character[1] Name of the object.
# @param file character[1] A desired file name or a file that exists to
#     be appended.
# @param append logical[1] \code{TRUE} informs that the content will be
#     appended into the file.
# @return character[1] The file name where the documentation will be
#     written.
where_save <- function(name, file, append = FALSE) {
    # Faz arquivo ter o nome do objeto.
    if (missing(file)) {
        file <- sprintf("%s.R", name)
    }
    # Onde salvar.
    if (!is.na(file)) {
        if (file.exists("DESCRIPTION")) {
            file <- sprintf("R/%s", file)
        }
        if (!append & file.exists(file)) {
            stop(sprintf(paste("File `%s` already exists.",
                               "Use `append = TRUE` or remove it."),
                         file))
        }
    }
    return(c(file = file))
}
# where_save("fun")
# where_save("fun", file = "as.R")
# where_save("fun", file = "func.R", append = FALSE)
# where_save("fun", file = "func.R", append = TRUE)

#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @name roxy_obj
#' @title Builds the documentation in \code{roxygen2} based on object
#'     properties
#' @description This function gets a function and writes the skeleton of
#'     the documentation in roxygen syntax. The call of this function
#'     assumes that the working directory is a package directory, that
#'     is, where DESCRIPTION file is.
#' @param object An object that is a function.
#' @param file character[1] An optional file name to write the
#'     documentation into. If it is not provided, the name of the
#'     function is used to name the file. If \code{NA}, no file is
#'     created and the documentation will be printed on console.
#' @param export logical[1] If \code{TRUE} means that the function will
#'     be exported because the label \code{@export} will be added to the
#'     documentation. Only has effect for \code{roxy_fun}.
#' @param source character[1] A string that is the source of the
#'     dataset. Only has effect for \code{roxy_data}.
#' @param author character[1] The author of the function to be assigned
#'     to the \code{@author} field.
#' @param keywords character[>=1] Keywords to the function that will be
#'     assigned to the \code{@keywords} field.
#' @param extra character[1] Additional information to be written, for
#'     example \code{"@import lattice"}.
#' @param editor character[1] A name of an editor to open the file after
#'     its creation. See \code{\link[utils]{edit}} for more details.
#' @param print logical[1] If \code{TRUE}, prints the skeleton in the
#'     console.
#' @param append logical[1] If \code{TRUE} the documentation is appended
#'     to the file informed.
#' @param find_file logical[1] If \code{TRUE}, shows the absolute path
#'     to the file formatted for a call of \code{find-file} in Emacs
#'     LISP, so the file can be opened just typing \code{C-x C-e} at the
#'     end of the statement.
#' @return This function does not have return value. It only
#'     creates/modifies files and prints content.
#' @examples
#'
#' \dontrun{
#'
#' #-----------------------------------------
#' # Using roxy_fun().
#'
#' fun <- function(x, y, ...) {
#'     return(x + y)
#' }
#'
#' file.remove("bla.R")
#' file.remove("fun.R")
#'
#' roxy_fun(fun)
#' roxy_fun(fun, append = TRUE)
#'
#' file.remove("fun.R")
#' roxy_fun(fun, find_file = TRUE)
#'
#' file.remove("fun.R")
#' roxy_fun(fun, editor = "emacs")
#'
#' roxy_fun(fun, file = NA)
#'
#' file.remove("fun.R")
#' roxy_fun(fun, print = TRUE)
#'
#' file.remove("fun.R")
#' roxy_fun(fun, export = FALSE)
#'
#' file.remove("fun.R")
#' roxy_fun(fun, author = "Walmes Zeviani, \\email{walmes@@ufpr.br}.")
#'
#' roxy_fun(fun, file = "bla.R")
#'
#' file.remove("bla.R")
#' roxy_fun(fun, file = "bla.R", extra = "@import lattice")
#'
#' file.remove("fun.R")
#' roxy_fun(object = fun, editor = "emacs")
#'
#' }
#'
#' \dontrun{
#'
#' #-----------------------------------------
#' # Using roxy_data().
#'
#' s <- "Smith; Sanders (1234)"
#' file.remove("iris.R")
#' roxy_data(iris,
#'           print = TRUE,
#'           source = s,
#'           editor = "emacs",
#'           keywords = c("BLA", "BLU"),
#'           find_file = TRUE,
#'           extra = c("@docType dataset",
#'                     "@details bla bla bla"))
#'
#' }

#' @rdname roxy_obj
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
roxy_fun <- function(object,
                     file,
                     export = TRUE,
                     author,
                     keywords,
                     extra,
                     editor,
                     print = FALSE,
                     append = FALSE,
                     find_file = FALSE) {
    # Nome da função.
    name <- deparse(substitute(object))
    file <- where_save(name, file, append)
    # Argumentos da função.
    params <- names(formals(object))
    # Conteúdo da documentação.
    ctnt <- c(sprintf("@name %s", name),
              if (!missing(author)) {
                  sprintf("@author %s", author)
              },
              if (export) {
                  "@export"
              },
              "@title",
              "@description",
              sprintf("@param %s", params),
              "@return",
              if (!missing(keywords)) {
                  paste(c("@keywords", keywords), collapse = " ")
              },
              if (!missing(extra)) {
                  extra
              },
              "@examples")
    ctnt <- paste("#\'", ctnt)
    if (is.na(file)) {
        cat(ctnt, sep = "\n")
    } else {
        # Exporta a documentação para o arquivo.
        cat(ctnt, sep = "\n", file = file, append = append)
        if (print) {
            cat(ctnt, sep = "\n")
        }
        # Exporta a função para o arquivo.
        dump(name, file = file, append = TRUE)
        # Abre arquivo no editor.
        if (!missing(editor)) {
            editor <- match.arg(arg = editor,
                                choices = c("vi",
                                            "emacs",
                                            "pico",
                                            "xemacs",
                                            "xedit"))
            do.call(editor, list(file = file))
        }
    }
    if (find_file) {
        cat(sprintf("(find-file \"%s\")",
                    paste(path.expand(getwd()), file, sep = "/")),
            "\n")
    }
    invisible(NULL)
}

#' @rdname roxy_obj
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @importFrom utils tail
roxy_data <- function(object,
                      file,
                      source = NULL,
                      keywords,
                      author,
                      extra,
                      editor,
                      print = FALSE,
                      append = FALSE,
                      find_file = FALSE) {
    # Nome do objeto.
    name <- deparse(substitute(object))
    file <- where_save(name, file, append)
    # Determina a classe.
    cld <- tail(class(object), n = 1)
    # Esqueleto do @format para cada classe.
    frmat <- switch(cld,
                    "data.frame" = {
                        f <- sprintf(paste(
                            "@format A \\code{data.frame} with %d",
                            "observations and %d variables, in which"),
                            nrow(object), ncol(object))
                        f <- strwrap(f, width = 69)
                        f <- c(f[1], paste("    ", f[-1]))
                        c(f,
                          "", "\\describe{", "",
                          rbind(sprintf("\\item{\\code{%s}}{  }",
                                        names(object)), ""),
                          "}")
                    },
                    "numeric" = {
                        sprintf(
                            "@format A vector with %d values.",
                            length(object))
                    },
                    "matrix" = {
                        sprintf(
                            "@format A matrix with %d rows and %d columns.",
                            nrow(object), ncol(object))
                    },
                    stop(paste("This function still does not deal with this class of `object`.")))
    # Conteúdo da documentação.
    ctnt <- c(sprintf("@name %s", name),
              if (!missing(author)) {
                  sprintf("@author %s", author)
              },
              "@title",
              "@description",
              frmat,
              if (!missing(extra)) {
                  extra
              },
              if (!missing(keywords)) {
                  paste(c("@keywords", keywords), collapse = " ")
              },
              if (!is.null(source)) {
                  s <- strwrap(paste("@source", source),
                               width = 69)
                  c(s[1], paste("    ", s[-1]))
              },
              "@examples")
    # Evitar espaço no final de linhas vazias.
    ctnt <- c(paste(ifelse(ctnt == "", "#\'", "#\' "),
                    ctnt, sep = ""), "NULL")
    if (is.na(file)) {
        cat(ctnt, sep = "\n")
    } else {
        # Exporta a documentação para o arquivo.
        cat(ctnt, sep = "\n", file = file, append = append)
        if (print) {
            cat(ctnt, sep = "\n")
        }
        # Abre arquivo no editor.
        if (!missing(editor)) {
            editor <- match.arg(arg = editor,
                                choices = c("vi",
                                            "emacs",
                                            "pico",
                                            "xemacs",
                                            "xedit"))
            do.call(editor, list(file = file))
        }
    }
    if (find_file) {
        cat(sprintf("(find-file \"%s\")",
                    paste(path.expand(getwd()), file, sep = "/")),
            "\n")
    }
    invisible()
}
