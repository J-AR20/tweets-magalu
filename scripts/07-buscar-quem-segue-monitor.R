
erro <- TRUE

while(erro) {
  res <- try({
    source("scripts/07-buscar-quem-segue.R")
  })
  erro <- !is.list(res)
}

