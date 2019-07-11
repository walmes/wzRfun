# @author Walmes Zeviani, \email{walmes@@ufpr.br}.
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

# @author Walmes Zeviani, \email{walmes@@ufpr.br}.
# @param path character[1] É um caminho de diretório válido para o qual
#     será construída representação em forma de árvore modo ASCII.
# @param type character[1] Tipos de ordenamento em que "none" é o que
#     vem do \code{list.files()} e "dir_last" é para ordenar deixando
#     os diretórios para o final.
# @param all.files logical[1] Se \code{TRUE} então diretórios é
#     arquivos ocultos serão exibidos.
# @param level integer[1] Valor inteiro positivo indicando quantos
#     níveis/camadas de diretórios exibir.
# @return list[] Lista de tamanho indeterminado no qual os elementos
#     da lista podem ser sublistas e assim recursivamente. O conteúdo
#     da lista são characteres com os nomes de arquivos ou diretórios.
path2bigNestedList <- function(path,
                               type = "none",
                               all.files = TRUE,
                               level = 3) {
    if (level < 0) return(NULL)
    type <- match.arg(type[1], choices = c("none", "dir_last"))
    all.files <- all.files[1]
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
    # Só entra no `if` diretórios não vazios.
    if (isdir && isnempty) {
        # Retira a possível barra do nome de diretórios para previnir
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
            p <- paste(path["path"], lf, sep = "/")
            lf <- lf[order(file.info(p)$isdir)]
        }
        # Diretórios vazios não retornarão nada do list.files, então
        # não correr parte do código.
        if (length(lf) >= 1) {
            # Obtém os conectores e prefixos para cada
            # arquivo/diretório.
            aux <- connectors(lf)
            # Cria matriz com os elementos necessários, em cada linha um
            # arquivo/diretório, e nas colunas os elementos do nó.
            paths <- cbind(
                pre = paste(path["child"], collapse = ""),
                conn = aux$con,
                path = paste(path["path"], lf, sep = "/"),
                child = paste(path["child"], aux$pre, sep = ""))
            # Concatena.
            u <- apply(paths[, 1:3, drop = FALSE],
                       MARGIN = 1,
                       FUN = function(x) {
                           paste(basename(x), collapse = "")
                       })
            # Transforma o que eram linhas em elementos de lista.
            paths <- split(cbind(paths, u), 1:nrow(paths))
            # Garante os nomes corretos.
            paths <- lapply(paths, "names<-", names(path))
            # ATTENTION: Chamada recursiva com os mesmos argumentos.
            lf <- lapply(paths,
                         FUN = path2bigNestedList,
                         type = type,
                         all.files = all.files,
                         level = level - 1)
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
    # Mantém apenas as entradas não nulas.
    lf <- Filter(Negate(is.null), lf)
    return(lf)
}

#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @title Shows directories content as an ASCII tree graph
#' @description This function mimics the
#'     \href{http://mama.indstate.edu/users/ice/tree/}{\code{tree}}
#'     shell command in Linux. It shows directories in a tree structure
#'     as an ASCII art. Not all options were enabled.
#' @param path character[1] Root path for the construction of the ASCII
#'     representation.
#' @param level integer[1] A positive integer that is how many levels to
#'     show. Default is 3. Use \code{Inf} to not limit.
#' @param type character[1] This is the display mode. Default is
#'     \code{"dir_last"} that keeps files at the beginning and
#'     directories to the end. The value \code{"none"} uses alphabetical
#'     order..
#' @param all.files logical[1] Using \code{TRUE}, hidden files and
#'     directories are showed.
#' @return Returns the directory list as invisible.
#' @seealso \code{\link{tree}()} for an implementation that calls
#'     \code{tree} on Linux. Also, \code{\link[base]{dir}()},
#'     \code{\link[base]{list.files}()} and
#'     \code{\link[base]{list.dirs}()} can be useful in many
#'     circumstances.
#' @examples
#'
#' \dontrun{
#'
#' # Tree of your currend directory.
#' getwd()
#' dir_tree()
#' dir_tree(type = "none")
#' dir_tree(all.files = TRUE)
#'
#' # Tree of a installed package.
#' dir_tree(system.file(package = "lattice"), level = 1)
#' dir_tree(system.file(package = "lattice"), level = 2)
#' dir_tree(system.file(package = "lattice"), level = 3)
#' dir_tree(system.file(package = "lattice"), level = 4)
#'
#' m <- dir_tree(system.file(package = "MASS"), level = 2)
#' str(m)
#' }
dir_tree <- function(path = "./",
                     level = 3,
                     type = "dir_last",
                     all.files = FALSE) {
    dir_list <- path2bigNestedList(path = path,
                                   type = type,
                                   all.files = all.files,
                                   level = level)
    cat(unlist(dir_list, recursive = TRUE), sep = "\n")
    invisible(dir_list)
}
