# Internal use. Copies the default data files to the current working directory
get_files = function() {
    file.copy(dir(path=paste0(PATH, "/files"), full.names=T), ".")
}

# Internal use. Gets the relevant information about an exercise
get_exercise = function (id) {
    tests <- dbGetQuery(connect(),
                        paste("SELECT condicao, dica FROM teste
                              WHERE id_exercicio=", id,
                              " ORDER BY ordem ASC", sep=""));
    preconditions <- dbGetQuery(connect(), 
                          paste("SELECT precondicoes FROM exercicio 
                                WHERE id_exercicio=", id, sep=""));
    return(list(tests = tests, preconditions = preconditions))
}

# corretoR recebe: 
# texto 
# E devolve um um vector logico com o resultado dos testes
# Caso o codigo tenha erros de sintaxe, retorna NULL
# Usa variavel global PATH (definida no PHP?)  

#' Core notaR functions
#' 
#' The following functions are the core mechanics for the notaR package. \code{correctoR}
#' receives an exercise and an attempt to answer it. 
#' @export
#' @rdname notaR
#' @example correctoR(1, "x <- 1:5")
#' @param exercise Either the number of the exercise or a named list containing the exercise preconditions and tests
#' @param attempt The attempted solution for the exercise
#' @return either a vector containing the passed/failed tests, or NULL in case the attempted answer did not run
correctoR <- function (exercise, attempt) {
    if (is.numeric(exercise)) {
        exercise = get_exercise(exercise)
    } else {
        stopifnot(class(exercise) == "list" & all(names(exercise) %in% c("tests", "preconditions")))
    }
    # Creates the isolated correction environment
    corrEnv <- new.env()
    # Creates the isolated preconditions environment
    precEnv <- new.env()
    # Copies the files used by exercises
    get_files()

    # Runs the exercise preconditions
    if ("preconditions" %in% names(exercise))
        eval(parse(text=exercise$preconditions), envir=precEnv);

    # Executa o texto da resposta
    # try pega erros de sintaxe
    getError <- try(eval(parse(text=attempt), envir=corrEnv));
    if (class(getError) == "try-error") return (NULL);

    # Executa os testes cadastrados, sequencialmente
    notaMax <- nrow(exercise$tests)
    notas <- rep(FALSE, notaMax)
    for (i in 1:notaMax) {
        # A avaliacao pode retornar TRUE, FALSE ou erro
        # No momento, erro esta sendo tratado como FALSE
        # Edit fev 2013: 
        # O [1] no final tem a funcao de evitar condicoes com comprimento 0.
        # Agora essas condicoes se tornam [1] NA, que serao transformados em FALSE abaixo
        notas[i] <- (try(eval(parse(text=exercise$tests[i,1]), envir=corrEnv))[1] == TRUE)[1];
    }
    notas[is.na(notas)] <- FALSE
    return(notas);
}

# For internal use: generates a small HTML with a report about the current exercise
reportR <- function (exercise, correction) {
    if (is.numeric(exercise)) {
        exercise = get_exercise(exercise)
    } 
    correctionMax <-nrow(exercise$tests)
    Rel <- "";
    if (! is.null(correction) && sum(correction) != correctionMax) { 
        Rel <- paste(Rel, "<font color='#8c2618'>ATEN&Ccedil;&Atilde;O!</font><br>")
        # Envia a primeira mensagem de erro
        primeiro.erro <- min(which(!correction))
        Rel <- paste(Rel, "<br>", exercise$tests[primeiro.erro,2], 
                     "<br>Corrija essa condi&ccedil;&atilde;o para continuar a corre&ccedil;&atilde;o.",	sep="");
    }
    if (is.null(correction)) {
        Rel <- paste(Rel, "<p>Cuidado! Seu exerc&iacute;cio n&atilde;o executou. Ser&aacute; que ele cont&eacute;m algum
                     erro de sintaxe? Veja dicas no linque de \"ajuda\" para corrigir os problemas.</p>", sep="");
    } else { Rel <- paste(Rel, "<p>Seu aproveitamento: <b>", round(100*sum(correction)/correctionMax),"%</b>.</p>", sep=""); }
    return(Rel)
}

# gravarNota recebe
# nome.aluno (pode ser NULL, no caso de ouvintes)
# numero.aula, numero.exercicio
# nota: logical vector contendo o resultado da correcao
# texto: resposta dada pelo aluno
# Valor de retorno: char, especificando mensagem de sucesso ou erro 
# na insercao da nota
gravarNota <- function (nome.aluno, id.exerc, texto, nota = corretoR(id.exerc, texto), ignore=F) {
    con = connect()
    # Definicoes iniciais
    id.aluno <- dbGetQuery(con, paste("SELECT id_aluno FROM aluno 
                                      WHERE nome_aluno ='", nome.aluno,"'", sep=""));
    Date <- format(Sys.time(), "%F %R");

    if (no.results(id.aluno)) { 
        id.aluno <- "NULL";
    }
    else {
        prazo <- dbGetQuery(con,
                            paste("SELECT prazo FROM prazo
                                  JOIN turma USING (id_turma) JOIN aluno USING (id_turma)
                                  WHERE id_exercicio=", id.exerc, " AND id_aluno=", id.aluno));
                                  if(no.results(prazo)) 
                                      prazo = "Inf"
                                  # Condicoes para gravar a nota
                                  if (prazo != "Inf" & Date > prazo & ! ignore) return ("<p><font color='#8c2618'>O prazo para entrega j&aacute; expirou!</font> A nota n&atilde;o foi gravada.</p>")
    }

    peso <- dbGetQuery (con,
                        paste("SELECT peso FROM teste
                              WHERE id_exercicio = ", id.exerc,
                              " ORDER BY ordem ASC ", sep=""));

    # Escapa os single quotes do texto
    texto <- gsub("'", '"', texto)
    if (is.null(nota)) nota <-rep(0, length(t(peso)));
    res <- dbSendQuery(con,paste("INSERT INTO nota (id_aluno, id_exercicio, data, nota, texto) 
                                 VALUES (",id.aluno,",",id.exerc,",'",Date,"',",
                                         round(100*weighted.mean(nota, t(peso))), ",'",paste(texto, collapse="\n"),"')",sep=""))
    melhorNota <- dbGetQuery(con,
                             paste("SELECT max(nota) FROM nota
                                   WHERE id_aluno = ",id.aluno, " AND id_exercicio=",
                                   id.exerc, sep=""));

    if (id.aluno =="NULL") return ("<p><font color='#8c2618'>Voc&ecirc; n&atilde;o est&aacute; logado.</font> A nota n&atilde;o foi gravada.</p>")
    Rel <- paste("<p>Nota cadastrada! Sua melhor nota nesse exerc&iacute;cio &eacute; <b>", melhorNota, 
                 "%</b>.", sep="")
    if (prazo != "Inf") Rel <- paste (Rel, "<br>O prazo para enviar novas tentativas &eacute; ", prazo, ".", sep="");
    return (paste(Rel, "</p>"));
}

# Recebe o exercicio, corrige, grava a nota e gera um output formatado em HTML
notaR <- function (nome.aluno, id.exerc, arquivo, ignore=F) {
    texto <- readLines(arquivo, encoding="utf8");
    nota <- corretoR (id.exerc, texto);
    # Tenta de novo com charset latin1:
    if (is.null(nota)) {
        texto <- readLines(arquivo, encoding="latin1");
        nota <- corretoR (id.exerc, texto);
    }
    # Grava a nota no banco:
    notaGravada <- gravarNota(nome.aluno, id.exerc, texto, nota, ignore)
    # Gera o relatorio de notas:
    Rel <- relatorioNota(id.exerc, nota, texto);
    return(paste(Rel, notaGravada,sep=""))
}
# Exemplos: 
# con <- connect('notaR', 'notaRPW', 'notaR')
# PATH <- '/var/www/notaR/'
# notaR('chalom', 2,"xpto.R")
