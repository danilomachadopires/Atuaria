dados <- data.frame(
  Coluna1 = c(1, 2, 3),
  Coluna2 = c(4, 5, 6),
  Coluna3 = c(7, 8, 9)
)

# Especificar o nome do arquivo de saída
nome_do_arquivo <- "dados.txt"

# Abrir o arquivo para escrita
arquivo <- file(nome_do_arquivo, "w")

# Loop para escrever linha a linha no arquivo
for (i in 1:nrow(dados)) {
  linha <- paste(dados[i,], collapse = "\t") # Separador de tabulação (\t) entre as colunas
  cat(linha, file = arquivo)
  cat("\n", file = arquivo) # Adiciona uma quebra de linha após cada linha
}

# Fechar o arquivo
close(arquivo)


  x <- seq(2,8,len=100)
  y <- seq(15,20,len=100)
  W<-c(15,20,11,18,13)
#  M<-matrix(0,10000,3)
  nome_do_arquivo <- "dados.txt"
# Abrir o arquivo para escrita
  arquivo <- file(nome_do_arquivo, "w")
  aux<-1
  for(i in 1:100){
    for (j in 1:100){
      linha <- paste(x[i],y[j],5*log(x[i])-5*x[i]*log(y[j])+(x[i]-1)*sum(log(W))-(1/(y[j]^x[i]))*sum(W^x[i]), collapse = "\t") # Separador de tabulação (\t) entre as colunas
      cat(linha, file = arquivo)
      cat("\n", file = arquivo)
      aux<-aux+1 
    }
    cat("\n", file = arquivo)
    #write.table( "",row.names = F, col.names = F,file="dadosn.txt",append = TRUE)
  }

  close(arquivo)
  
  
  
  
  
  
  
  write.table(M,row.names = F, col.names = F,file="dados.txt")
  
  z <- outer(x, y, function(x, y) 5*log(x)-5*x*log(y)+(x-1)*sum(log(Y))-(1/(y)^x)*sum(Y^y))
  surface3d(x, y, z, color = "blue")
  plot_ly(x = x, y = y, z = z, type = "surface")
  
  
  plot3d(x, y, z, type = "surface", size = 10, col = "blue")
  title3d("Gráfico 3D de Exemplo")
  xlabel3d("Eixo X")
  ylabel3d("Eixo Y")
  zlabel3d("Eixo Z")
  legend3d("topright", legend = "Pontos", col = "blue", pch = 16, bg = "white")
  
  x <- seq(-5, 5, length.out = 100)
  y <- seq(-5, 5, length.out = 100)
  z <- outer(x, y, function(x, y) x^2 + y^2) 
  contour(x, y, z, main = "Gráfico de Curvas de Nível", xlab = "Eixo X", ylab = "Eixo Y")
  contour(x, y, z, main = "Gráfico de Curvas de Nível", xlab = "Eixo X", ylab = "Eixo Y",
          labels = TRUE, labcex = 0.7, lwd = 2, col = "blue")
  