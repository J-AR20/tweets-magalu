
erro <- TRUE

while(erro) {
  res <- try({
    source("08-buscar-quem-segue.R")
  })
  erro <- !is.list(res)
}

