#' @name rejeicao
#' @title rejeicao
#' @param fx        - função densidade da distribuição de interesse
#' @param gy        - função densidade da distribuição candidata
#' @param gerador_y - função geradora de valores da candidata
#' @param N         - quantidade de valores, padrão - 10E4
#' @param  ...       - parâmetros do gerador_y
#' @param cor_aceito     - cor dos pontos aceitos, padrão - "blue"
#' @param cor_nao_aceito - cor dos pontos não aceitos, padrão - "red"
#' @param ...            - parâmetros para função plot
#' @return lista x
#' @export
rejeicao <- function(fx, gy, gerador_y, N = 1E4, ...){
  
  # Parâmetros: fx        - função densidade da distribuição de interesse
  #             gy        - função densidade da distribuição candidata
  #             gerador_y - função geradora de valores da candidata
  #             N         - quantidade de valores, padrão - 10E4
  #             ...       - parâmetros do gerador_y
  
  # Retorno:  Lista - $x             - valores aceitos 
  #                   $y             - y correspondente dos valores aceitos 
  #                   $rate          - proporção
  #                   $iteracoes     - número de iterações
  #                   $x_nao_aceitos - valores não aceitos
  #                   $y_nao_aceitos - y correspondente dos valores não aceitos 
  
  # Calculando M
  M <- optimize(f = function(x) fx(x)/gy(x), 
                interval = c(0, 1), maximum = T)$objective
  
  # Iniciando valores faltantes
  faltantes <- N
  # Iniciando vetor x dos aceitos
  x <- c()
  # Iniciando vetor x dos não aceitos
  x_nao_aceito <- c()
  # Iniciando vetor y dos aceitos
  y <- c()
  # Iniciando vetor y dos não aceitos
  y_nao_aceito <- c()
  # Guardando os valores gerados em uma variável
  gerados <- 0
  
  while(faltantes > 0){
    # Calculando o número de gerados
    gerados <- gerados + faltantes
    
    # Gerando Y
    Y <- gerador_y(faltantes, ...)
    # Gerando U
    U <- runif(faltantes)
    
    # Verificando os valores aceitos
    x_i <- ifelse(U <= fx(Y)/(M*gy(Y)), Y, NA)
    # verificando as posições não aceitas
    na_i <- is.na(x_i)
    # salvando os valores aceitos
    x_i_aceito <- x_i[!na_i]
    # calculando y dos valores aceitos
    y_i_aceito <- (U*M*gy(Y))[!na_i]
    # salvando os valores não aceitos
    x_i_nao_aceito <- Y[na_i]
    # calculando y dos valores não aceitos
    y_i_nao_aceito <- (U*M*gy(Y))[na_i]
    
    # Calculando faltantes
    faltantes <- sum(na_i)
    # juntando pares ordenados dos aceitos
    x <- c(x, x_i_aceito)
    y <- c(y, y_i_aceito)
    # juntando pares ordenados dos não aceitos
    x_nao_aceito <- c(x_nao_aceito, x_i_nao_aceito)
    y_nao_aceito <- c(y_nao_aceito, y_i_nao_aceito)
  }
  # Retornando uma lista com os resultados
  return(list(x = x, y = y, rate = N/gerados, iteracoes = gerados,
              x_nao_aceitos = x_nao_aceito, y_nao_aceitos = y_nao_aceito, fx = fx))
}