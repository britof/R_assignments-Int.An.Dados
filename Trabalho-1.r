library(ggplot2)
options(max.print = 3000)
database <- read.csv("C:\\Users\\ferre\\Downloads\\aplicativo_controle_financeiro.csv")
logged_hours = database$horas_logado
bb_logged_hours = database$horas_logado_BB

##########
#Questão 1
print("QUESTÃO 1")
logged_hours_mean = mean(logged_hours)
ordered_logged_hours = sort(logged_hours, decreasing=FALSE)
trimmed_logged_hours_mean = mean(logged_hours[151:2850])
logged_hours_median = median(logged_hours)
print("Média das Horas Logadas no Aplicativo:")
logged_hours_mean
print("Média Aparada (10%) das Horas Logadas no Aplicativo:")
trimmed_logged_hours_mean
print("Mediana das Horas Logadas no Aplicativo:")
logged_hours_median
# a) a mediana é menor do que a média por ser menos afetada por valores extremos
# b) por ser uma medida resistente

##########
#Questão 2
print("QUESTÃO 2")
logged_hours_standard_deviation = sd(logged_hours)
print("Desvio Padrão das Horas Logadas no Aplicativo: ")
logged_hours_standard_deviation
# a) sendo a raiz quadrada da variância, é uma medida de dispersão que se utiliza de um valor positivo na mesma unidade das variáveis observadas

##########
#Questão 3
q1 <- ordered_logged_hours[750]
q2 <- ordered_logged_hours[1500]
q3 <- ordered_logged_hours[1750]
print("1°, 2° e 3° Quartis das Horas Logadas no Aplicativo: ")
q1
q2
q3
# a) o primeiro quartil é o valor que separa 25% dos menores valores dos maiores 75% do conjunto;
#    o segundo quartil é a mediana;
#    o terceiro quartil separa 25% dos maiores valores de 75% dos menores valores de um dado conjunto;

#Questão 4
boxplot(ordered_logged_hours)
# a) há um único outlier
# b) neste caso, o valor outlier difere muito do intervalo normal de valores e pode afetar negativamente uma análise de dados neste conjunto

##########
#Questão 5
i1 <- NULL
j <- 0
for(i in ordered_logged_hours) {
  if(i >= 5) 
    break
  i1[j] <- i
  j <- j + 1
}
i2 <- NULL
j <- 0
for(i in ordered_logged_hours) {
  if(i >= 10) 
    break
  if(i < 5)
    next
  i2[j] <- i
  j <- j + 1
}
i3 <- NULL
j <- 0
for(i in ordered_logged_hours) {
  if(i >= 15) 
    break
  if(i < 10)
    next
  i3[j] <- i
  j <- j + 1
}
i4 <- NULL
j <- 0
for(i in ordered_logged_hours) {
  if(i < 10)
    next
  i4[j] <- i
  j <- j + 1
}

matrix_logged_hours <- matrix(c(length(i1), 100*length(i1)/length(logged_hours),
                                length(i2), 100*length(i2)/length(logged_hours),
                                length(i3), 100*length(i3)/length(logged_hours),
                                length(i4), 100*length(i4)/length(logged_hours),
                                length(logged_hours), 100), 
                              ncol=2, byrow=TRUE)
colnames(matrix_logged_hours) <- c("Freq. Absoluta", "Freq. Relativa")
rownames(matrix_logged_hours) <- c("[0, 5[", "[5, 10[", "[10, 15[", "[15, 41[", "TOTAL")
frequency_table_logged_hours <- as.table(matrix_logged_hours)
# a) 
hist(c(i1,i2,i3,i4))

# b) segundo o histograma, mais da metade dos usuários passa 5 horas ou mais, ao passo que uma parcela mínima passa mais de 15 horas

###########
#Questão 6
plot(logged_hours, bb_logged_hours, main="Relação entre o número de horas no meu App e no app do BB", xlab="Meu App", ylab="App do BB")
# a) segundo o gráfico de dispersão, há uma correlação negativa perfeita entre as duas variáveis, de modo que à medida em que as horas no "Meu App" crescem, as do "App do BB" diminuem


##########
#Questão 7
ggplot(database, (aes=(x=logged_hours, y=bb_logged_hours)))
