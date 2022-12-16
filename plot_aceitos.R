#' @name plot_aceitos
#' @title plot_aceitos
#' @param x              - lista gerada com valores aceitos e não aceitos
#' @param cor_aceito     - cor dos pontos aceitos, padrão - "blue"
#' @param cor_nao_aceito - cor dos pontos não aceitos, padrão - "red"
#' @param ...            - parâmetros para função plot
#' @export
plot_aceitos <- function(x, cor_aceito = "blue", cor_nao_aceito = "red", ...){
  
  # Parâmetros: x              - lista gerada com valores aceitos e não aceitos
  #             cor_aceito     - cor dos pontos aceitos, padrão - "blue"
  #             cor_nao_aceito - cor dos pontos não aceitos, padrão - "red"
  #             ...            - parâmetros para função plot
  
  # Retorno:  Gráfico
  
  fx = x$fx
  
  # Plotando aceitos
  plot(x$x, x$y, col = cor_aceito, bty = "L", xlab = "x", ylab = "densidade", ...)
  # Plotando não aceitos
  points(x$x_nao_aceitos, x$y_nao_aceitos, col = cor_nao_aceito)
  # Plotando curva de densidade
  curve(fx, col = cor_aceito, add = T)
  # Plotando proporção
  mtext(sprintf("Proporção de aceitos: %.02f%%", x$rate*100),
        line = 1, adj = 0)
}