connectors <- function(x) {
    # Função que retorna em "con" os conectores para cada nó, colocados
    # na frente dos nomes de arquivos ou diretórios, sendo "`--"
    # para os nós final e "|--" para os nós intermediários. Em "pre"
    # são retornados aquilo que deve ser prefixo dos conectores para
    # os descendentes do nó caso ele seja um diretório, sendo "   " para
    # nó final e "|  " para nós intermediários.
    tms <- c(length(x) - 1, 1)
    list(con = rep(c("|-- ", "`-- "), tms),
         pre = rep(c("|   ", "    "), tms))
}

path2bigNestedList <- function(path, type = "none", all.files = TRUE) {
    # Tipos de ordenamento em que "none" é o que vem do list.files() e
    # "dir_last" é para ordenar deixando os diretórios para o final.
    type <- match.arg(type, choices = c("none", "dir_last"))
    # Se all.files == TRUE então diretórios é arquivos ocultos serão
    # exibidos.
    all.files <- all.files[1]
    # path: é um caminho de diretório.
    if (length(path) == 1) {
        # pre: o que antecede o contector para apropriada indentação e
        # conexão dos arquivos.
        # conn: conector que antecede o nome do arquivo/diretório.
        # path: caminho do arquivo/diretório.
        # child: o que é passado para dos descentes desse nó, caso ele
        # seja um diretório, como "pre" para haver apropriada indentação
        # e conexão entre arquivos/diretórios.
        path <- c(pre = "", conn = "", path = path, child = "")
    }
    # É diretório?
    isdir <- isTRUE(file.info(path["path"])$isdir)
    # Tem conteúdo?
    isnempty <- (length(dir(path["path"])) >= 1)
    # Só entra no if diretórios não vazios.
    if (isdir && isnempty) {
        # Retina a possível barra no do nome de diretórios para previnir
        # acidentes.
        path["path"] <- sub(x = path["path"],
                            pattern = "/$",
                            replacement = "")
        # Cria o texto do nó = pre + conn + path.
        path["text"] <- paste(basename(path[1:3]), collapse = "")
        # Lista arquivos e diretórios dentro do path informado.
        lf <- list.files(path["path"],
                         all.files = all.files,
                         no.. = TRUE,
                         include.dirs = TRUE,
                         full.names = FALSE)
        if (type == "dir_last") {
            lf <- lf[order(file.info(
                paste(path["path"], lf, sep = "/"))$isdir)]
        }
        # Diretórios vazios não retornarão nada do list.files, então
        # não correr parte do código.
        if (length(lf) >= 1) {
            # Obtém os conectores e prefixos para cada
            # arquivo/diretório.
            aux <- connectors(lf)
            # Cria matriz com os elementos necessários, em cada linha um
            # arquivo/diretório, e nas colunas os elementos do nó.
            paths <- cbind(pre = paste(path["child"], collapse = ""),
                           conn = aux$con,
                           path = paste(path["path"], lf, sep = "/"),
                           child = paste(path["child"],
                                         aux$pre,
                                         sep = ""))
            # No caso de ter apenas uma linha, que dá problema com
            # apply() que precisa de nrow > 1, trabalha como vetor para
            # obter o texto do nó = pre + conn + path.
            if (nrow(paths) == 1) {
                u <- paste(paths[1, 1:3], collapse = "")
            } else {
                u <- apply(paths[, 1:3], MARGIN = 1,
                           FUN = function(x) {
                               paste(basename(x), collapse = "")
                           })
            }
            # Transforma o que eram linhas em elementos de lista.
            paths <- split(cbind(paths, u), 1:nrow(paths))
            # Garante os nomes corretos.
            paths <- lapply(paths, "names<-", names(path))
            # Chamada recursiva com os mesmo argumentos.
            lf <- lapply(paths,
                         FUN = path2bigNestedList,
                         type = type,
                         all.files = all.files)
        }
        # Adiciona o path inicial.
        lf <- c(sprintf("%s/", path["text"]), lf)
    } else {
        # Quando não existirem mais diretórios, retorna só o texto do
        # nó. A barra no final sinaliza os diretórios.
        lf <- sprintf("%s%s",
                      path["text"],
                      ifelse(isdir, yes = "/", no = ""))
    }
    return(lf)
}
# path <- "./slave.Rcheck/slave/R"
# path <- "./slave.Rcheck/slave/"
# path <- "./slave.Rcheck/"
# path <- "./"
# path <- "~/repos/labestData"
#
# a <- path2bigNestedList(path)
# b <- unlist(a, recursive = TRUE)
# cat(b, sep = "\n")

#' @name tree
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @title Show a Directory as an ASCII Tree
#' @description This function returns an ascii art of the directory
#'     tree, like the command
#'     \href{http://mama.indstate.edu/users/ice/tree/}{\code{tree}} in
#'     Linux.
#' @param path a string indicating the path where the tree starts.
#' @param type a string that is type of display. Default is
#'     \code{"dir_last"} to show the directories at the end of the
#'     tree. The alphanumeric order is returned using \code{"none"}.
#' @param all.files a logical, if \code{TRUE} hidden files and
#'     directories will be printed.
#' @return Prints on the console the tree of the directory.
#' @seealso \code{\link[base]{dir}}, \code{\link[base]{list.files}}.
#' @examples
#'
#' \donttest{
#'
#' # Tree of your currend directory.
#' getwd()
#' tree()
#' tree(type = "none")
#' tree(all.files = TRUE)
#'
#' # Tree of home folder.
#' tree("~/")
#'
#' # Tree of a installed package.
#' tree(system.file(package = "lattice"))
#'
#' }
tree <- function(path = "./",
                 type = "dir_last",
                 all.files = FALSE) {
    cat(unlist(path2bigNestedList(path = path,
                                  type = type,
                                  all.files = all.files),
               recursive = TRUE),
        sep = "\n")
}
