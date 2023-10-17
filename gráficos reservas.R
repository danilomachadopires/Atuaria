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




# rm(list = ls())
#tabua = read.csv2("C:/Users/Leonardo/Documents/Profissional/UNIFAL/Disciplinas/Matemática Atuarial II/2023_01/1ª Prova/Gabarito/IBGE_fem_2021.csv",dec=",")


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

f_anuidade(idade=30, i=0.06, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=NA)


# PROVA DA GESIELI C. R. DA ROCHA
# Questão 1
# a)
PPU = 200000*f_seguro(idade=23, i=0.04, qx=tabua$qx, lx=tabua$lx)
PPU

# b) 
PNI = PPU/f_anuidade(idade=23, i=0.04, lx=tabua$lx)

# c)
V10 = 200000*f_seguro(idade=33, i=0.04, qx=tabua$qx, lx=tabua$lx) - PNI*f_anuidade(idade=33, i=0.04, lx=tabua$lx)
V10

V40 = 200000*f_seguro(idade=63, i=0.04, qx=tabua$qx, lx=tabua$lx) - PNI*f_anuidade(idade=63, i=0.04, lx=tabua$lx)
V40

# d) 
V40 = 200000*f_seguro(idade=63, i=0.02, qx=tabua$qx, lx=tabua$lx) - PNI*f_anuidade(idade=63, i=0.02, lx=tabua$lx)
V40


# Questão 2)
# a)
PNI = 45500*(f_anuidade(idade=23, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=37))/f_anuidade(idade=23, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=37, dif=NA)
PNI

# b)
V10 =  45500*(f_anuidade(idade=33, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=27)) - PNI*f_anuidade(idade=33, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=27, dif=NA)
V10

V50 =  45500*(f_anuidade(idade=73, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=NA))
V50



# Questão 3
# a)
PNI = 45500*(f_anuidade(idade=23, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=37))/f_anuidade(idade=23, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=37, dif=NA)
PNI

# b)
V10 =  45500*(f_anuidade(idade=33, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=27)) - PNI*f_anuidade(idade=33, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=27, dif=NA)
V10

V50 =  45500*(f_anuidade(idade=73, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=NA))
V50


# Questão 4)
# a)
PNI = 45500*(f_anuidade(idade=23, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=34))/f_anuidade(idade=23, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=34, dif=NA)
PNI

# b)
V10 =  45500*(f_anuidade(idade=33, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=24)) - PNI*f_anuidade(idade=33, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=24, dif=NA)
V10

V50 =  45500*(f_anuidade(idade=73, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=NA))
V50





# PROVA DO PEDRO
# Questão 1
# a)
PPU = 200000*f_seguro(idade=22, i=0.04, qx=tabua$qx, lx=tabua$lx)
PPU

# b) 
PNI = PPU/f_anuidade(idade=22, i=0.04, lx=tabua$lx)
PNI

# c)
V10 = 200000*f_seguro(idade=32, i=0.04, qx=tabua$qx, lx=tabua$lx) - PNI*f_anuidade(idade=32, i=0.04, lx=tabua$lx)
V10

V40 = 200000*f_seguro(idade=62, i=0.04, qx=tabua$qx, lx=tabua$lx) - PNI*f_anuidade(idade=62, i=0.04, lx=tabua$lx)
V40

# d) 
V40 = 200000*f_seguro(idade=62, i=0.02, qx=tabua$qx, lx=tabua$lx) - PNI*f_anuidade(idade=62, i=0.02, lx=tabua$lx)
V40


# Questão 2) - NÃO FEZ A QUESTÃO 2
# a)
PNI = 45500*(f_anuidade(idade=22, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=43))/f_anuidade(idade=22, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=43, dif=NA)
PNI

# b)
V10 =  45500*(f_anuidade(idade=32, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=33)) - PNI*f_anuidade(idade=32, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=33, dif=NA)
V10

V50 =  45500*(f_anuidade(idade=72, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=NA))
V50



# Questão 3
# a)
PNI = 45500*(f_anuidade(idade=22, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=43))/f_anuidade(idade=22, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=43, dif=NA)
PNI

# b)
V10 =  45500*(f_anuidade(idade=32, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=33)) - PNI*f_anuidade(idade=32, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=33, dif=NA)
V10

V50 =  45500*(f_anuidade(idade=72, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=NA))
V50


# Questão 4)
# a)
PNI = 45500*(f_anuidade(idade=22, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=40))/f_anuidade(idade=22, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=40, dif=NA)
PNI

# b)
V10 =  45500*(f_anuidade(idade=32, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=30)) - PNI*f_anuidade(idade=32, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=30, dif=NA)
V10

V50 =  45500*(f_anuidade(idade=72, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=NA))
V50






# PROVA DO BERNARDO
# Questão 1
# a)
PPU = 200000*f_seguro(idade=21, i=0.04, qx=tabua$qx, lx=tabua$lx)
PPU

# b) 
PNI = PPU/f_anuidade(idade=21, i=0.04, lx=tabua$lx)
PNI

# c)
V10 = 200000*f_seguro(idade=31, i=0.04, qx=tabua$qx, lx=tabua$lx) - PNI*f_anuidade(idade=31, i=0.04, lx=tabua$lx)
V10

V40 = 200000*f_seguro(idade=61, i=0.04, qx=tabua$qx, lx=tabua$lx) - PNI*f_anuidade(idade=61, i=0.04, lx=tabua$lx)
V40

# d) 
V40 = 200000*f_seguro(idade=61, i=0.02, qx=tabua$qx, lx=tabua$lx) - PNI*f_anuidade(idade=61, i=0.02, lx=tabua$lx)
V40


# Questão 2) - NÃO FEZ A QUESTÃO 2
# a)
PNI = 45500*(f_anuidade(idade=21, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=44))/f_anuidade(idade=21, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=44, dif=NA)
PNI

# b)
V10 =  45500*(f_anuidade(idade=31, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=34)) - PNI*f_anuidade(idade=31, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=34, dif=NA)
V10

V50 =  45500*(f_anuidade(idade=71, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=NA))
V50



# Questão 3 (A QUESTÃO FOI RESOLVIDA PELO DISCENTE COMO SE FOSSE UM SEGURO)
# a)
PNI = 45500*(f_anuidade(idade=21, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=44))/f_anuidade(idade=21, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=44, dif=NA)
PNI

# a) - resolvendo como se fosse um seguro
#PNI = 45500*(f_seguro(idade = 21, i = 0.025, qx=tabua$qx, lx=tabua$lx, benef = 1, temp=NA, dif = 44))/f_anuidade(idade=21, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=44, dif=NA)
#PNI

# b)
V10 =  45500*(f_anuidade(idade=31, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=34)) - PNI*f_anuidade(idade=31, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=34, dif=NA)
V10

V50 =  45500*(f_anuidade(idade=71, i=0.025, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=NA))
V50


# Questão 4)
# a)
PNI = 45500*(f_anuidade(idade=21, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=41))/f_anuidade(idade=21, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=41, dif=NA)
PNI
# a) - resolvendo como se fosse um seguro
#PNI = 45500*(f_seguro(idade = 21, i = 0.04, qx=tabua$qx, lx=tabua$lx, benef = 1, temp=NA, dif = 41))/f_anuidade(idade=21, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=41, dif=NA)
#PNI



# b)
V10 =  45500*(f_anuidade(idade=31, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=31)) - PNI*f_anuidade(idade=31, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=31, dif=NA)
V10

V50 =  45500*(f_anuidade(idade=71, i=0.04, lx=tabua$lx, benef=1, postec = FALSE, temp=NA, dif=NA))
V50



