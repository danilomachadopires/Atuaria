tabuas<- read.table('Tabuas.txt', header=T)
attach(tabuas)

AX<-function( i, idade,b,tabua)  {  
  qx<-tabua
  px<-1-qx
  n<-max(Idade)-idade
  v <- (1/(i+1))^(1:n)                                
  
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) ) 
  if(n==1){
    pxx<-1
  }
  qxx <- c(qx[(idade+1):(idade+n)])
  
  Ax <- b* sum(v*pxx*qxx)
  
  if(n==0){
    Ax<-0
  }
  return (Ax)
}


ax<-function(i,idade,b,tabua){
  px<- 1-tabua
  v <- 1/(1+i)
  n<-max(Idade)-idade
  pxx    <- c(1, cumprod(px[(idade+1):(length(px)-1)]) )
  if(n==1){
    pxx<-1
  }
  t         <- (0:(length(pxx)-1))
  if(n==0){
    ax<-0
  }
  ax      <- b*sum(v^(t)*pxx)
  return(ax)
}



p<-AX(0.03,40,1,tabuas$AT_49_qx)/ax(0.03,40,1,tabuas$AT_49_qx)

Vr<-Vrv<-(1:(max(Idade)-40))*0

Vr[1]<-Vrv[1]<-0

for (i in 1:(max(Idade)-41)){
  
  Vr[i+1]=AX(0.03,40+i,1,tabuas$AT_49_qx)-p*ax(0.03,40+i,1,tabuas$AT_49_qx)
  Vrv[i+1]=(AX(0.03,40+i,1,tabuas$AT_49_qx)+0.005*ax(0.03,40+i,1,tabuas$AT_49_qx))-(p)*ax(0.03,40+i,1,tabuas$AT_49_qx)
}

plot(Vr,type='l',ylim=c(0,1),ylab="Reserva",xlab="tempo",lwd=2)
par(new=T)
plot(Vrv,type='l',col='red',ylim=c(0,1),ylab="Reserva",xlab="tempo",lwd=2)

#----------------------------------------------------------------------------------------
rm(list=ls(all=T))

tabuas<- read.table('Tabuas.txt', header=T)
attach(tabuas)


Axn<-function( i, idade, n,b,tabua)  { 
  
  qx<-tabua
  px<-1-qx
  v <- (1/(i+1))^(1:n)                                
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  if(n==1){
    pxx<-1
  }
  qxx <- c(qx[(idade+1):(idade+n)])
  
  Axn <- b* sum(v*pxx*qxx)
  if(n==0){
    Axn<-0
  }
  return (Axn)
}

anx<-function(i,idade,n,b,tabua){
  v       <- 1/(1+i) 
  px     <- 1-tabua
  pxx   <- c(1, cumprod(px[(idade+1):(idade+n-1)]) )
  if(n==1){
    pxx<-1
  }
  t        <- (0:(length(pxx)-1))
  anx     <- b*sum(v^(t)*pxx)
  if(n==0){
    anx<-0
  }
  return(anx)
}

dotalP<-function(i,idade,n,b,tabua){
  px<-1-tabua
  V <- 1/(i+1)^n
  npx <- prod( px[(idade+1):(idade+n)]) 
  Dt<- V*npx*b
  return(Dt)
}



i<-0.03; x=30; m=10; n=5

p<-(dotalP(i,x,m,1,tabuas$AT_49_qx)*anx(i,x+m,(max(tabuas$Idade)-x-m),1,tabuas$AT_49_qx))/anx(i,x,m,1,tabuas$AT_49_qx)



Vr<-Vrv<-(0:10)*0

Vr[1]=Vrv[1]=0

for (i in 1:10){
  
  Vr[i+1]=Axn(0.03,(10+i),(10-i),1,tabuas$AT_49_qx)-p*anx(0.03,10+i,10-i,1,tabuas$AT_49_qx)
  Vrv[i+1]=(Axn(0.03,20+i,10-i,1,tabuas$AT_49_qx)+0.005*anx(0.03,1+i,10-i,1,tabuas$AT_49_qx))-(p)*anx(0.03,1+i,10-i,1,tabuas$AT_49_qx)
  
}

plot(Vr,type='b',ylab="Reserva",xlab="tempo",lwd=2,ylim=c(0,0.01))
par(new=T)
plot(Vrv,type='l',col='red',ylab="Reserva",xlab="tempo",lwd=2,ylim=c(0,0.15))





# 1) Variar a taxa de juros
# 2) Variar o tempo passado
# 3) para uma idade fixa do segurado, obter a reserva e diferentes taxas de juros em diferentes momentos
# 4) um eixo vai ser a taxa de juros, outro vai ser o tempo passado e outro vai ser a reserva
# 5) normalmente temos somente a reserva por tempo passado. 
# 
# 
# 

  J  <- seq(0.01,0.05,len=100)
  id <- 35:60
  nome_do_arquivo <- "dadosn.txt"
  


arquivo <- file(nome_do_arquivo, "w")
  aux<-1
  for(i in 1:100){
    for (j in 1:26){

Pr<-(dotalP(J,id,m,1,tabuas$AT_49_qx)*anx(J,id+m,(max(tabuas$Idade)-x-m),1,tabuas$AT_49_qx))/anx(i,x,m,1,tabuas$AT_49_qx)     
      
      linha <- paste(J[i],id[j],5*log(x[i])-5*x[i]*log(y[j])+(x[i]-1)*sum(log(W))-(1/(y[j]^x[i]))*sum(W^x[i]), collapse = "\t") # Separador de tabulação (\t) entre as colunas
      cat(linha, file = arquivo)
      cat("\n", file = arquivo)
      aux<-aux+1 
    }
    cat("\n", file = arquivo)
    #write.table( "",row.names = F, col.names = F,file="dadosn.txt",append = TRUE)
  }

  close(arquivo)
  
  
  
  
  
# Colocar no livro do leandro  
  n<-0:3
  p<-dbinom(n,3,0.6)
  fdp<-pbinom(n,3,0.6)
  U<-c(0.7, 0.1, 0.3, 0.1, 0.9, 0.5, 0.5, 0.7, 0.3, 0.10)
  N<-U*0
  for(i in 1:length(U)){
    if(U[i]<fdp[1]){
      N[i]<-0
    }else{ if((fdp[1]<=U[i])|(U[i]<fdp[2])){
      N[i]<-1}else{ 
        if((fdp[2]<=U[i])|(U[i]<fdp[3])){N[i]<-2}else{N[i]<-3}     }
      
    }
    
  }
  
 
  
  