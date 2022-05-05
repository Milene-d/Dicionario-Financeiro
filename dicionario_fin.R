### Preparação dos dados ###

##### Análise quanti ####
serie<- read.csv("C:/Users/milen/Desktop/serie.csv", sep=";", dec=",")
View(serie)

data<-serie$date
likes<-serie$favorites
reTwitter<-serie$retweets

linhas<-dim(serie)
linhas<-linhas[1]-1

meanL<-c()
meanT<-c()
nobs<-c()
ano<-c()

l<-likes[1]
r<-reTwitter[1]

for(i in 1:linhas)
{
  j=i+1
  
  if(data[i]==data[j])
  {
    l<-cbind(l,likes[j])  
    r<-cbind(r,reTwitter[j])  
  }
  else
  {
    ano<-cbind(ano,data[i])
    meanL<-cbind(meanL,mean(l))
    meanT<-cbind(meanT,mean(r))
    obs<-dim.data.frame(l)
    nobs<-cbind(nobs,obs[2])
    l<-likes[j]
    r<-reTwitter[j]
  }
  
}

ano<-cbind(ano,data[i])
meanL<-cbind(meanL,mean(l))
meanT<-cbind(meanT,mean(r))
obs<-dim.data.frame(l)
nobs<-cbind(nobs,obs[2])

ano<-t(ano)
meanL<-t(meanL)
meanT<-t(meanT)
nobs<-t(nobs)

Resultados<-data.frame(ano,meanL,meanT,nobs)
Resultados
write.table(Resultados, file = "C:\\Users\\milen\\Desktop\\Resultados.csv",sep=";", dec=",")

-------------------------------------------------------------------------------------------------------------------
  
  ##### Análise geral das palavras #####

library(readxl)
library(streamR)
library(RCurl) 
library(RJSONIO)
library(ROAuth)
library(tm)

txt_all<-serie$text
txt_all<-as.character(txt_all)

#Limpar os dados de caracteres estranhos
# txt_all<- gsub("[^0-9A-Za-z///' ]","'" , txt_all,ignore.case = TRUE)
txt_all<- gsub("''","" , txt_all,ignore.case = TRUE)

CORPO<-VectorSource(txt_all)
CORPO<-Corpus(CORPO)
summary(CORPO)
CORPO<-tm_map(CORPO, removeNumbers)
CORPO<-tm_map(CORPO, removePunctuation)
removeURL<-function(x)gsub("http[^[:space:]]*", "", x)
CORPO<-tm_map(CORPO, content_transformer(removeURL))
inspect(CORPO)
CORPO<-tm_map(CORPO, removeWords, stopwords("pt")) # mudar para "pt" se estiver em português
CORPO<-tm_map(CORPO, content_transformer(tolower))
inspect(CORPO)
# CORPO<-tm_map(CORPO, removeWords, c("","","")) #tirar palavras indesejadas

CORPO<-tm_map(CORPO, removeWords, c("de","é", "e","em","o","os","as","-","da","dá","h","d","g","p","do","eu","aqui","tá","vou","a","r","q","que","para","pra","ã","pq","ª","º","no","se","petrobras","petrobrã¡s","©","jã¡","petrobrás"))

dtm<-DocumentTermMatrix(CORPO, control=list(wordLengths=c(1, Inf)))
dim(dtm)
inspect(dtm)

dtm_s<-removeSparseTerms(dtm, 0.995)
dtm_s
inspect(dtm_s)
freq_s<-colSums(as.matrix(dtm_s))
length(freq_s)
ord_ss<-order(freq_s)
ord_sss<-sort(ord_ss)
dtm_s

ord<-freq_s[rev(order(freq_s))]
View(ord) #vendo as palavras que aparecem mais

write.table(ord, file = "C:\\Users\\milen\\Desktop\\ord.csv",sep=";", dec=",")

-----------------------------------------------------------------------------------------------  
#### Análise em Cluster ####

library(cluster)
d<-dist(t(dtm_s),method="euclidean")
fit<-hclust(d=d, method="ward.D")
plot(fit, hang=-1)

-----------------------------------------------------------------------------------------------
### Contando Palavras ###

lista<-read.csv("C:\\Users\\milen\\Desktop\\lista_teste.csv",header=FALSE, sep=";", dec=",")
serie<-read.csv("C:\\Users\\milen\\Desktop\\ano2010 - original.csv",header=TRUE, sep=";", dec=",")

library(stringr)

dias<-dim(serie)[1] #vendo o tamanho da série como um todo
npalavras<-dim(lista)[1]
resultadoFinal<-c()
twdia<-serie[1,]

for(i in 1:dias)
{
  if(i!=dias)
  {
    ii<-i+1
    if (serie$date[i]==serie$date[ii])
    {
      twdia<-rbind(twdia,serie[ii,])
    }
    else
    {
      txt_all<-twdia$text
      txt_all<-as.character(txt_all)
      txt_all<- gsub("''","" , txt_all,ignore.case = TRUE)
      txt_all = gsub("[[:punct:]]", " ", txt_all)  # Remove punctuation
      txt_all = gsub("[ |\t]{2,}", " ", txt_all)  # Remove tabs
      txt_all = gsub("^ ", "", txt_all)  # Leading blanks
      txt_all = gsub(" $", "", txt_all)  # Lagging blanks
      txt_all = gsub(" +", " ", txt_all) # General spaces
      resultado<-cbind(toString(twdia$date[1]),sum(str_count(txt_all,'\\w+')))    
      
      for(p in 1:npalavras)
      {
        palavra<-toString(lista[p,])
        resultado<-cbind(resultado,sum(grepl(palavra,tolower(txt_all),fixed = TRUE )))
      }
      twdia<-serie[ii,]
      resultadoFinal<-rbind(resultadoFinal,resultado)
    }  
  }
  else
  {
    txt_all<-twdia$text
    txt_all<-as.character(txt_all)
    txt_all<- gsub("''","" , txt_all,ignore.case = TRUE)
    txt_all = gsub("[[:punct:]]", " ", txt_all)  # Remove punctuation
    txt_all = gsub("[ |\t]{2,}", " ", txt_all)  # Remove tabs
    txt_all = gsub("^ ", "", txt_all)  # Leading blanks
    txt_all = gsub(" $", "", txt_all)  # Lagging blanks
    txt_all = gsub(" +", " ", txt_all) # General spaces
    resultado<-cbind(toString(twdia$date[1]),sum(str_count(txt_all,'\\w+')))    
    
    for(p in 1:npalavras)
    {
      palavra<-toString(lista[p,])
      resultado<-cbind(resultado,sum(grepl(palavra,tolower(txt_all),fixed = TRUE )))
    }
    resultadoFinal<-rbind(resultadoFinal,resultado)  
  }  
}

colnames(resultadoFinal)<-c("date","Total Palavras",t(lista))
resultadoFinal<- as.data.frame(resultadoFinal)
View(resultadoFinal)

write.table(resultadoFinal, file = "C:\\Users\\milen\\Desktop\\resultadoFinal.csv",sep=";", dec=",")

---------------------------------------------------------------------------------------------------
  # importar excel dos tweets e dos preços
Dados_Milene <- read.csv("C:/Users/milen/Desktop/Dados finais/Dados_Milene.csv", sep=";", dec=",")
View(Dados_Milene)

#Calcular variação diária dos preços da petr e ibov

x <- diff(log(Dados_Milene$`PETR4 BZ Equity`))
length(x)  
retpetr<-c("NA",x)
length(retpetr)

y <- diff(log(Dados_Milene$`IBOV Index`))
length(y)  
retibov<-c("NA",y)
length(retibov)

#Juntar com as datas
retd<-cbind(Dados_Milene, retpetr, retibov)
head(retd)

#unir retornos com as palavras pelos dias
library(dplyr)
total<- left_join(resultadoFinal, retd, by= "date")
write.table(total, file = "C:\\Users\\milen\\Desktop\\total.csv",sep=";", dec=",")

#Preparar a matrix para o mapa#
total<-total[,-2] #tirar o que não interessa para o modelo
total<-total[,-1] #tirar o que não interessa para o modelo
total<-na.omit(total) #tirei as linhas com dados faltantes

positivo<-subset(total, retpetr>0)
A<-positivo[,1:201]
A[]<-lapply(A, function(x) as.numeric(as.character(x)))
A<-as.data.frame(A)
dpos<-lapply(A, sum)
dpos<-as.data.frame(dpos)
dpos<-t(dpos)
dpos<-as.data.frame(dpos)

negativo<-subset(total, retpetr<0)
N<-negativo[,1:201]
N[]<-lapply(N, function(x) as.numeric(as.character(x)))
N<-as.data.frame(N)
dneg<-lapply(N, sum)
dneg<-as.data.frame(dneg)
dneg<-t(dneg)
dneg<-as.data.frame(dneg)

freq<-cbind(dpos,dneg)
freq<-as.data.frame(freq)
colnames(freq)<-c("positivo","negativo")

library(plfm)
library(ggplot2)
pc.cr <- princomp(freq, cor=TRUE)
summary(pc.cr)
biplot(pc.cr, ylim  = c(-0.2,0.2), xlim = c(-0.2,0.6),scale = 0.3)
abline(h=0, lyt = "dotted", col="red")
