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
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#-----------------------------------------------------GRÁFICOS DE RESERVAS

rm(list=ls(all=T))

tabua<- read.table('Tabuas.txt', header=T)
attach(tabua)

#  O código do R faz arrendondamentos, pois usa a coluna lx e não a q
# motivo da suavição do grafico. 
anx<-function(i,idade,n,b,tabua){
  v       <- 1/(1+i) 
  px     <- 1-tabua
  pxx   <- c(1, cumprod(px[(idade+1):(idade+n-1)]) )
  if(n==1){
    pxx<-1
  }
  t        <- (0:(length(pxx)-1))
  anx     <- b*sum(v^(t)*pxx)
  if(n<=0){
    anx<-0
  }

  return(anx)
}




dotalP<-function(i,idade,n,b,tabua){
  px<-1-tabua
  V <- 1/(i+1)^n
  npx <- prod( px[(idade+1):(idade+n)])
   Dt <- V*npx*b
   if(n<0){Dt=0}
  return(Dt)
}

#  xm é a idade em que vai se aposentar
Pr<-function(i,x,xm,b,tabua){
  q<-tabua
  m<-xm-x
  pr<-(dotalP(i,x,m,1,q)*anx(i,xm, ((length(q)-1)-(xm)),1,q))/anx(i,x,m,1,q)
  if(pr=='NaN'){pr<-0}
  return(pr)
}


#  xm é a idade em que x vai se aposentar
Reserva <-function(i,x,tabua,xm,t,b){
  qq <-tabua
  m  <-(xm-x)
  if(t<m){
       Vr <- dotalP(i,(x+t),(m-t),b,qq)*anx(i,xm,((length(qq)-1)-xm),b,qq)-pr*anx(i,(x+t),(m-t),b,qq)
  } else{
       Vr <- anx(i,(x+t),((length(qq)-1)-(x+t)),b,qq)
  }
  return(Vr)
}


#----------------------------------------------------------------
#PNI = (f_anuidade(idade=65, i=0.2, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=5))/
#       f_anuidade(idade=65, i=0.2, lx=tabua$lx, benef=1, postec = FALSE, temp=5, dif=NA)
#pr<-Pr(0.2,65,70,1,tabua$qx)
#PNI;pr
# m=5, t=4, x=65
#V4 =  (f_anuidade(idade=69, i=0.2, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=1)) - 
#       PNI*f_anuidade(idade=69, i=0.2, lx=tabua$lx, benef=1, postec = FALSE, temp=1, dif=NA)
#V4;Reserva(0.2,65,tabua$qx,70,4,1)

#------------------------------------------------------------------------





#----------------------------------------------------------
id    <- 35:65        # variar a idade inicial
tempo <- 0:45         # Tempo passado   ( ajustar o tempo para que não ultrapasse a tabua)
nome_do_arquivo <- "dados2.txt"
arquivo         <- file(nome_do_arquivo, "w") # Abrir o arquivo para escrita


   aux <- 0
     q <-tabua$AT_49_qx
     q <-c(q[q<1],1)
  
for(Id in 1:length(id)){   # IDADES
  
  pr  <- Pr(0.2,id[Id],70,1,q)
  aux <- aux+1
  
  for (j in 1:length(tempo)){
  
  V <- Reserva(0.2,id[Id],q,70,tempo[j],1)
  
  if(!is.na(V)){
    
  linha <- paste(id[Id],tempo[j],V,collapse = "\t")  # Separador de tabulação (\t) entre as colunas
    cat(linha, file = arquivo)
    cat("\n", file = arquivo)}
  
}
  cat("\n", file = arquivo)
  aux<-aux+1 
  
 }

close(arquivo)

par(mfrow = c(1, 2)) 




Vr<-(1:80)*0

 Reserva(0.03,35,tabua$qx,70,36,1)
 anx(0.03,71,(max(tabua$Idades)-71),1,tabua$qx)


pr<-Pr(0.03,35,70,1,tabua$qx)

for (t in 1:80){
  Vr[t]= Reserva(0.03,35,tabua$qx,70,t,1)
}
plot(Vr,type='l',ylab="Reserva",xlab="tempo",lwd=2)


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
  
 
  
  