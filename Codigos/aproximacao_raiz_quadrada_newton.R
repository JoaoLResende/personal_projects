

raiz_quadrada <- function(valor_inicial,numero) {
  valor_inicial^(2)-numero
}



derivada_raiz_quadrada <- function(valor_inicial){
  2*valor_inicial
}

aproximacao_newton <- function(valor_inicial, numero, repeticoes, precisao){
  x <- c()
  y <- c()

  for (i in 1:repeticoes) {


  valor_atual <- valor_inicial - (raiz_quadrada(valor_inicial,numero) / derivada_raiz_quadrada(valor_inicial))


  if(round(valor_atual - valor_inicial,precisao) == 0){


    texto <- paste0("O valor da sua raiz qudrada é aproximadamente de ", valor_atual, "e não pode ser melhorado com essa quantidade de casas decimais")
    # return(texto)



  }

  valor_inicial <- valor_atual


  # print(round(resposta[i] - resposta[i-1],3))


  }

print(paste0("O valor atual de ", valor_atual ," ainda pode se aproximar mais do valor correto"))


}

a <- aproximacao_newton(1,2,5,10)

