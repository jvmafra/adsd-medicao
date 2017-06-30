library(dplyr)
library(ggplot2)

tratamento <- function(df, tipo.conexao, quant.registros, versao.bd){
  return (df %>% 
    filter(conexao == tipo.conexao, quant_registros == quant.registros, versao_bd == versao.bd))
}

agrupamento <- function(df, fator){
  return (df %>% group_by_(fator) %>% summarise(media = mean(tempo), 
                                               mediana = median(tempo),
                                               variancia = var(tempo), 
                                               desvio = sd(tempo)))
}

plotaPontos <- function(df){
  p <- ggplot(df) + geom_point(aes(1:10, df$tempo)) + xlab("Numero da amostra") + 
    ylab("Tempo de resposta (em segundos)") + scale_x_continuous(breaks = seq(0,10,1)) +
    scale_y_continuous(breaks = seq(0,25,2))
  return (p)
}

plotaBoxPlot <- function(df){
  boxplot(df$tempo, ylab = "Tempo de resposta (em s)")
}


mostraEstatisticas <- function(df){
  print(summary(df$tempo))
  print(t.test(df$tempo))
  print(var(df$tempo))
  print(sd(df$tempo))
}

amostras <- read.csv(file = "/Users/Mafra/DOcuments/medicao_adsd.csv")

summary(amostras)

ggplot(amostras) + geom_point(aes(1:80, tempo)) + xlab("Numero da amostra") + ylab("Tempo de resposta (em segundos)")

qplot(y=amostras$tempo, x= 1:1.1, geom = "boxplot") + ylab("Tempo de resposta (em segundos)") + 
  ggtitle("Boxplot para os tempos de respostas (todas as amostras)")


agrupado.tipo.conexao <- agrupamento(amostras, "conexao")
agrupado.versao <- agrupamento(amostras, "versao_bd")
agrupado.registros <- agrupamento(amostras, "quant_registros")

tratamento1.3G.muitos.versao10 <- tratamento(amostras, " 3G", " muitos", " 10.0.0")
tratamento2.3G.poucos.versao10 <- tratamento(amostras, " 3G", " poucos", " 10.0.0")
tratamento3.3G.muitos.versao9 <- tratamento(amostras, " 3G", " muitos", " 9.6.1")
tratamento4.3G.poucos.versao9 <- tratamento(amostras, " 3G", " poucos", " 9.6.1")
tratamento5.WiFi.muitos.versao10 <- tratamento(amostras, " WiFi", " muitos", " 10.0.0")
tratamento6.WiFi.poucos.versao10 <- tratamento(amostras, " WiFi", " poucos", " 10.0.0")
tratamento7.WiFi.muitos.versao9 <- tratamento(amostras, " WiFi", " muitos", " 9.6.1")
tratamento8.WiFi.poucos.versao9 <- tratamento(amostras, " WiFi", " poucos", " 9.6.1")

ggplot(tratamento1.3G.muitos.versao10) + geom_point(aes(1:10, tempo)) + xlab("Numero da amostra") + ylab("Tempo de resposta (em segundos)")




summary(tratamento1.3G.muitos.versao10)
boxplot(tratamento1.3G.muitos.versao10$tempo)
boxplot(amostras$tempo)
plot(amostras$tempo)


t.test(amostras$tempo)

conexao.3g = amostras %>% filter(conexao == " 3G")
conexao.wifi = amostras %>% filter(conexao == " WiFi")
t.test(conexao.3g$tempo)
t.test(conexao.wifi$tempo)


muitos.registros = amostras %>% filter(quant_registros == " muitos")
poucos.registros = amostras %>% filter(quant_registros == " poucos")
t.test(muitos.registros$tempo)
t.test(poucos.registros$tempo)


versao.10 = amostras %>% filter (versao_bd == " 10.0.0")
versao.9 = amostras %>% filter (versao_bd == " 9.6.1")
t.test(versao.10$tempo)
t.test(versao.9$tempo)




  
  
