# Internal use
startpng <-function(name) png(width=800, height=550, filename=paste(PATH,"img",name,sep="/"))

#' Graphs
#'
#' These functions return graphs detailing at which times the exercises are being solved
#' @export
#' @import grDevices graphics stats
#' @rdname graphs
byDow <- function() {
    startpng("dow.png")
    con = connect()
    x <- dbGetQuery(con, "select count(*), date_format(data - INTERVAL 3 HOUR, '%w') 
                    from nota group by date_format(data - INTERVAL 3 HOUR, '%w')")
                    x[,1] <- x[,1]/sum(x[,1])
                    par(fg='#FF6666', family='Verdana')
                    plot(x[,1]~x[,2], type='l', bty='n', xaxt='n', yaxt='n', xlab='Dia', ylab='% entregas', col='#007788', lwd=3)
                    axis(1, at=0:6, labels=c("dom", "seg", "ter", "qua", "qui", "sex", "sab"), lwd=3)
                    axis(2, lwd=3)
                    dev.off()
}

#' @export
#' @rdname graphs
byHour <- function() {
    startpng("porhora.png")
    con = connect()
    x <- dbGetQuery(con, "select count(*), date_format(data, '%H') 
                    from nota group by date_format(data, '%H')")
                    x[,1] <- x[,1]/sum(x[,1])
                    par(fg='#FF6666', family='Verdana')
                    plot(spline(x[,2],x[,1]), type='l', bty='n', xaxt='n', yaxt='n', xlab='Hora', ylab='% entregas', col='#007788', lwd=3)
                    axis(1, at=2*0:11, lwd=3)
                    axis(2, lwd=3)
                    dev.off()
}

byExercise <- function(turma) {
    startpng(paste("exercicio",turma,".png",sep=""))
    con = connect()
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
