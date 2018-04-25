#' Variavel global usada pela interface Web
#'
#' A variavel PATH deve ser definida na interface web para carregar os arquivos de dados.
#' Para uso local, mantenha o PATH definido como "."
PATH="."

# For internal use:
connect <- function (dbuser, dbpass, dbname) {
    require(RMySQL)
    try(dbDisconnect(con), silent=TRUE)
    return(dbConnect(MySQL(), user=dbuser, password=dbpass, dbname=dbname))
}
# Construida na chamada PHP como 
# con <- connect($DBUSER, $DBPASS, $DBNAME)

# Funcao acessoria para testar se um objeto MySQL nao tem resultados
no.results <- function(object) {
    length(object[,1]) == 0
}


# corretoR recebe: 
# texto 
# E devolve um um vector logico com o resultado dos testes
# Caso o codigo tenha erros de sintaxe, retorna NULL
# Usa variavel global PATH (definida no PHP?)  
corretoR <- function (id.exerc, texto) {
    # Definicoes iniciais
    corrEnv <- new.env()
    # copia todos os arquivos de dados para que possam ser usados pelo corretor
    file.copy(dir(path=paste0(PATH, "/files"), full.names=T), ".")
    # Funcoes dsiponiveis dentro do ambiente de correcao
    eval(parse(file=paste0(PATH,"/acessorias.R")), envir=corrEnv)
    # TO DO: mover eq para acessorias
    assign("eq", function(a, b) isTRUE(all.equal(a,b, tol=1e-7, check.attributes=FALSE)), envir=corrEnv)

    testes <- dbGetQuery(con,
                         paste("SELECT condicao FROM teste
                               WHERE id_exercicio=", id.exerc,
                               " ORDER BY ordem ASC", sep=""));
    precondi <- dbGetQuery(con, 
                           paste("SELECT precondicoes FROM exercicio 
                                 WHERE id_exercicio=", id.exerc, sep=""));

    # Executa as precondicoes
    if(!no.results(precondi)) eval(parse(text=precondi), envir=corrEnv);

    # Executa o texto da resposta
    # try pega erros de sintaxe
    getError <- try(eval(parse(text=texto), envir=corrEnv));
    if (class(getError) == "try-error") return (NULL);

    # Executa os testes cadastrados, sequencialmente
    notaMax <-dim(testes)[1]
    notas <- rep(FALSE, notaMax)
    for (i in 1:notaMax) {
        # A avaliacao pode retornar TRUE, FALSE ou erro
        # No momento, erro esta sendo tratado como FALSE
        # Edit fev 2013: 
        # O [1] no final tem a funcao de evitar condicoes com comprimento 0.
        # Agora essas condicoes se tornam [1] NA, que serao transformados em FALSE abaixo
        notas[i] <- (try(eval(parse(text=testes[i,1]), envir=corrEnv))[1] == TRUE)[1];
    }
    notas[is.na(notas)] <- FALSE
    return(notas);
}

# Gera um output formatado em HTML a respeito de um exercicio corrigido
relatorioNota <- function (id.exerc, nota, texto) {
    # Definicoes iniciais
    dica <- dbGetQuery (con,
                        paste("SELECT dica FROM teste
                              WHERE id_exercicio = ", id.exerc,
                              " ORDER BY ordem ASC ", sep=""));
    peso <- dbGetQuery (con,
                        paste("SELECT peso FROM teste
                              WHERE id_exercicio = ", id.exerc,
                              " ORDER BY ordem ASC ", sep=""));
    notaMax <-dim(dica)[1]
    Rel <- "";
    if (! is.null(nota) && sum(nota) != notaMax) { 
        Rel <- paste(Rel, "<font color='#8c2618'>ATEN&Ccedil;&Atilde;O!</font><br>")
        # Envia a primeira mensagem de erro
        primeiro.erro <- min(which(!nota))
        Rel <- paste(Rel, "<br>", dica[primeiro.erro,1], 
                     "<br>Corrija essa condi&ccedil;&atilde;o para continuar a corre&ccedil;&atilde;o.",	sep="");
    }
    if (is.null(nota)) {
        Rel <- paste(Rel, "<p>Cuidado! Seu exerc&iacute;cio n&atilde;o executou. Ser&aacute; que ele cont&eacute;m algum
                     erro de sintaxe? Veja dicas no linque de \"ajuda\" para corrigir os problemas.</p>", sep="");
    } else { Rel <- paste(Rel, "<p>Seu aproveitamento: <b>", round(100*weighted.mean(nota, t(peso))),"%</b>.</p>", sep=""); }
    #		Rel <- paste(Rel, "<p>Sua resposta:<br>", paste(texto, collapse="<br>"),"</p>", sep="");
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


#########################################################################
#########################################################################
#########################################################################
#########################################################################

## Graficos

startpng <-function(name) png(width=800, height=550, filename=paste(PATH,"img",name,sep="/"))

### Por Hora
porDow <- function() {
    startpng("dow.png")
    x <- dbGetQuery(con, "select count(*), date_format(data - INTERVAL 3 HOUR, '%w') 
                    from nota group by date_format(data - INTERVAL 3 HOUR, '%w')")
                    x[,1] <- x[,1]/sum(x[,1])
                    par(fg='#FF6666', family='Verdana')
                    plot(x[,1]~x[,2], type='l', bty='n', xaxt='n', yaxt='n', xlab='Dia', ylab='% entregas', col='#007788', lwd=3)
                    axis(1, at=0:6, labels=c("dom", "seg", "ter", "qua", "qui", "sex", "sab"), lwd=3)
                    axis(2, lwd=3)
                    dev.off()
}

porHora <- function() {
    startpng("porhora.png")
    x <- dbGetQuery(con, "select count(*), date_format(data, '%H') 
                    from nota group by date_format(data, '%H')")
                    x[,1] <- x[,1]/sum(x[,1])
                    par(fg='#FF6666', family='Verdana')
                    plot(spline(x[,2],x[,1]), type='l', bty='n', xaxt='n', yaxt='n', xlab='Hora', ylab='% entregas', col='#007788', lwd=3)
                    axis(1, at=2*0:11, lwd=3)
                    axis(2, lwd=3)
                    dev.off()
}
porExercicio <- function(turma) {
    startpng(paste("exercicio",turma,".png",sep=""))
    n_turma <- dbGetQuery(con, paste("select count(distinct id_aluno) from aluno join nota using(id_aluno) where id_turma=",turma))
    x <- dbGetQuery(con, paste("select nome, count(distinct id_aluno) from nota join aluno using(id_aluno)  join exercicio using (id_exercicio) join prazo using (id_exercicio, id_turma) where id_turma=",turma,"group by nome"))
    if(dim(x)[1] == 0) {dev.off(); return();}
    y <- dbGetQuery(con, paste("select nome, count(distinct id_aluno) from nota join aluno using(id_aluno)  join exercicio using (id_exercicio) join prazo using (id_exercicio, id_turma) where id_turma=",turma," and nota=100 group by nome"))
    x <- merge(x, y, by="nome", all=TRUE)
    x[,2:3] <- x[,2:3] / as.numeric(n_turma)
    Encoding(x[,1]) <- "latin1"
    #extrai o numero do exercicio
    f <- function(s) strsplit(s, " ")[[1]][1]
    x[,1] <- sapply(x[,1], f)
    x[is.na(x)] <- 0
    par(fg='#FF6666', family='Verdana', cex.axis=0.8)
    plot(x[,2], type='l', bty='n', xaxt='n', yaxt='n', xlab='Exercicio', ylab='% incompleto/completo', col='#007788', lwd=3, ylim=c(0,1), yaxs='i')
    points(x[,3], type='l', col='#770088', lwd=3)
    axis(1, at=1:dim(x)[1], labels=x[,1], lwd=3)
    axis(2, at=c(0,0.5, 1), lwd=3)
    grid(ny=NA, lty=1, lwd=2, col="gray")
    dev.off()
}
