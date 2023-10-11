require(lifecontingencies)

data(soaLt)

soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
                         x=x,lx=Ix,name="SOA2008"))
names(soa08Act)
#evaluate and life-long annuity for an aged 65
axn(soa08Act, x=65) 

## carregar tabela atuarial
data("soa08Act")
str(soa08Act)

## Exemplo 1 - reserva para seguro de vida inteira

## prÃªmio
(P = Axn(soa08Act,60)/axn(soa08Act,60))

## reserva t=10
(V = Axn(soa08Act,60+10)-P*axn(soa08Act,60+10))



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

AX(0.03,40,1,tabuas$AT_49_qx)


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


ax(0.03,40,1,tabuas$AT_49_qx)

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


tabuas<- read.table('Tabuas.txt', header=T)
attach(tabuas)

i<-0.03;idade=69;n=1;b=1;tabua<-tabuas$AT_49_qx

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



p<-Axn(0.03,1,10,1,tabuas$AT_49_qx)/anx(0.03,1,10,1,tabuas$AT_49_qx);p

Vr<-Vrv<-(0:10)*0

Vr[1]=Vrv[1]=0

for (i in 1:10){
  
  Vr[i+1]=Axn(0.03,(1+i),(10-i),1,tabuas$AT_49_qx)-p*anx(0.03,1+i,10-i,1,tabuas$AT_49_qx)
  Vrv[i+1]=(Axn(0.03,2+i,10-i,1,tabuas$AT_49_qx)+0.005*anx(0.03,1+i,10-i,1,tabuas$AT_49_qx))-(p)*anx(0.03,1+i,10-i,1,tabuas$AT_49_qx)
  
}

plot(Vr,type='b',ylab="Reserva",xlab="tempo",lwd=2,ylim=c(0,0.01))
par(new=T)
plot(Vrv,type='l',col='red',ylab="Reserva",xlab="tempo",lwd=2,ylim=c(0,0.15))



plot(Vr,type='b',ylab="Reserva",xlab="tempo",lwd=2)









## Exemplo 2 - reserva para seguro de vida temporÃ¡rio

## prÃªmio
## 

## reserva t=10
##









## GrÃ¡fico da reserva com tempo

## 1) Crie uma funÃ§Ã£o V para avaliar a reserva no tempo t

## 2) Avalie a funÃ§Ã£o para t=0,...,30
## Dica: usar a funÃ§Ã£o 'Vectorize'

## 3) FaÃ§a o grÃ¡fico





## Exemplo 3 - reserva para seguro temporario com duraÃ§Ã£o diferente para prÃªmios

## prÃªmio
##

## reserva t=3
##

## reserva t=10
##





## Exemplo 4 - reserva para seguro dotal misto


## prÃªmio
(P = AExn(soa08Act, 60,20)/axn(soa08Act, 60,20))

## reserva t=10
( V = AExn(soa08Act, 60+10,20-10)-P*axn(soa08Act, 60+10,20-10) )


## Exemplo 5 - reserva para anuidade vitalÃ­cia diferida

## prÃªmio
##

## reserva t=10
##

## reserva t=30
##





## FÃ³rmula recursiva


## funÃ§Ã£o para encontrar soluÃ§Ã£o para fÃ³rmula recursiva
recurrent = function(a,b,ufinal){
  s <- rev(cumprod(c(1, b)))
  return( ( rev(cumsum(s[-1]*rev(a))) + s[1]*ufinal )/rev(s[-1]) )
}


plot(c(0.000485, 0.000806, 0.000883, 0.000643),type='l')



## Exemplo - seguro temporÃ¡rio
## encontrando a reserva usando a fÃ³rmula recursiva

## prÃªmio
P = Axn(soa08Act,60,30)/axn(soa08Act,60,30)

## SequÃªncias de a e b
Vecta = Vectorize(function(t) Axn(soa08Act,t,1))(60+0:29) - P
Vectb = Vectorize(function(t) pxt(soa08Act,t,1))(60+0:29)/1.06

## SequÃªncia da reserva com a fÃ³rmula recursiva
Vectv = c(recurrent(a=Vecta,b=Vectb,ufinal=0),0)


## grÃ¡fico das reservas
plot(0:30, Vectv, type="b", pch=20, main="Reserva de seguro temporÃ¡rio por 30 anos")
