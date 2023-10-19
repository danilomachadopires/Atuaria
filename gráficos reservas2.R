# rm(list = ls())
tabua = read.csv2("IBGE_fem_2021.csv",dec=",")


f_seguro = function(idade, i, qx, lx, benef=1, temp=NA, dif=NA)
{
  tamTabua = length(qx)
  vx = 1/(1+i)
  
  if(!is.na(dif))
  {
    idadeInicial = idade
    idade = idade + dif
  }
  
  vetor_vx = vx^(1:(tamTabua - idade ))
  vetor_qx = qx[(idade + 1):tamTabua]
  vetor_px = lx[(idade + 1):tamTabua]/lx[idade + 1]
  
  if(!is.na(temp))
  {
    if((temp + idade) < tamTabua)
      result = sum(benef*vetor_vx[1:temp]*vetor_px[1:temp]*vetor_qx[1:temp])
    else
      result = sum(benef*vetor_vx*vetor_px*vetor_qx)
  }
  else
    result = sum(benef*vetor_vx*vetor_px*vetor_qx)
  
  if(!is.na(dif))
  {
    result = result*(vx^dif)*lx[idadeInicial + dif + 1]/lx[idadeInicial + 1]
  }
  
  return(result)
  
}

f_seguro(idade=30, i=0.06, qx=tabua$qx, lx=tabua$lx, benef=1, temp=NA, dif=10)




rm(list = ls())
tabua = read.csv2("IBGE_fem_2021.csv",dec=",")


f_anuidade = function(idade, i, lx, benef=1, postec = FALSE, temp=NA, dif=NA)
{
  vx = 1/(1 + i)
  tamTabua = length(lx)
  
  if(!is.na(dif))
  {
    idadeInicial = idade
    idade = idade + dif
  }
  
  vetor_vx = vx^(0:(tamTabua - idade - 1))
  vetor_px = lx[(idade + 1):tamTabua]/lx[(idade + 1)]
  
  if(!is.na(temp))
  {
    if((temp + idade) < tamTabua)
    {
      if(!postec)
        result = sum(benef*vetor_vx[1:temp]*vetor_px[1:temp])
      else
        result = sum(benef*vetor_vx[2:(temp+1)]*vetor_px[2:(temp+1)])
    }
    else
    {
      if(!postec)
        result = sum(benef*vetor_vx*vetor_px)
      else
        result = sum(benef*vetor_vx*vetor_px) - 1  
    }
  }
  else
  {
    if(!postec)
      result = sum(benef*vetor_vx*vetor_px)
    else
      result = sum(benef*vetor_vx*vetor_px) - 1
  }
  
  if(!is.na(dif))
    result = result*(vx^dif)*lx[idadeInicial + dif + 1]/lx[idadeInicial + 1]

  return(result)
}


#f_anuidade(idade=30, i=0.06, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=NA)


idadeInicial       = 30
idadeAposentadoria = 70
idadeFinal         = max(tabua$Idades)
vetorReserva       = rep(0, length(idadeFinal - idadeInicial))
txJuros            = 0.03
benef              = 1

 
anuidadDif  = benef*f_anuidade(idade=idadeInicial, i=txJuros, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=(idadeAposentadoria - idadeInicial))
anuidadTemp = f_anuidade(idade=idadeInicial, i=txJuros, lx=tabua$lx, benef=1, postec = FALSE, temp=(idadeAposentadoria - idadeInicial), dif=NA)
PNI         = anuidadDif/anuidadTemp

indice = 1



for(contador in idadeInicial:idadeFinal)
{
  if(contador < idadeAposentadoria)
  {
    VPBF = benef*f_anuidade(idade=contador, i=txJuros, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=(idadeAposentadoria - contador))
    VPCF = PNI*f_anuidade(idade=contador, i=txJuros, lx=tabua$lx, benef=1, postec = FALSE, temp=(idadeAposentadoria - contador), dif=NA)
    vetorReserva[indice] = VPBF - VPCF
  }
  else
  {
    VPBF = benef*f_anuidade(idade=contador, i=txJuros, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=NA)
    vetorReserva[indice] = VPBF
  }
indice = indice + 1
}

vetorReserva
plot.ts(vetorReserva)

