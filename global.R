#Instalação de pacotes
#install.packages("rworldmap")
#install.packages("dismo")
#install.packages("RgoogleMaps")
#install.packages("googleVis")
#install.packages("spatstat")
#install.packages("maptools")

#Exemplo com a base Titanic de algumas funções basicas de estatística
### Autora: Cristiane Queiroz ####

#Base do titanic, faz a leitura do arquivo
leitura_arquivo <- function(){
  setwd("~/TitanicDataset/data")
  tabela_train<-read.table(file = "train.csv", header = T, sep = ",", dec = ".", as.is = T)
  return(tabela_train)
}

#Tratamento dos dados antes de manipular
tratamento_dados<-function(df_titanic){

  #Excluir colunas que não serão utilizadas
  nomes_col <- c("PassengerId","SibSp","Parch","Ticket","Cabin")
  df_titanic <- df_titanic[ , -which(names(df_titanic) %in% nomes_col), drop = F]
  #Todos os passageiros que não possuem idade associada será atribuído o valor zero
  df_titanic$Age = ifelse(is.na(df_titanic$Age),0,df_titanic$Age)
  #Sexo - Masculino , feminino
  df_titanic$Sex = ifelse(df_titanic$Sex=="male","Masculino","Feminino")
  #renomear colunas para ficar claro o que cada uma significa
  names(df_titanic) <- c("Sobreviveu","Classe","Nome","Sexo","Idade","Valor_Passagem", "Cidade_Embarque")
  #Descrever o nome da Cidade de embarque
  df_titanic$Cidade_Embarque <- ifelse(df_titanic$Cidade_Embarque=="Q","Queenstown",ifelse(df_titanic$Cidade_Embarque=="S","Southampton","Cherbourg"))
  #Tratar para o Embarque
  return(df_titanic)
}


#total de passageiros Mortos/sobreviventes por classe de embarque no Titanic
passageiros_porsituacao_embarque <-function(df_titanic,filtro = filtro){
  library(dplyr)

  df_agrupamento <- as.data.frame.matrix(table(df_titanic$Classe,df_titanic$Sobreviveu))
  df_agrupamento$classe <-c("Primeira","Segunda","Terceira")
  names(df_agrupamento) <-c("Mortos","Sobreviventes", "Classe")

  return(df_agrupamento)
}

#Criar DataFrame com totais para Dashboard
totalizacao_dashboard<-function(df_titanic){
  total_passageiros <- nrow(df_titanic)
  total_mortos <- nrow(df_titanic[which(df_titanic$Sobreviveu==0),])
  total_sobreviventes <- nrow(df_titanic[which(df_titanic$Sobreviveu==1),])
  prop_mulheres <-round((nrow(df_titanic[which(df_titanic$Sexo=="Feminino"),])/total_passageiros)*100,digits = 0)
  prop_homens <-round((nrow(df_titanic[which(df_titanic$Sexo=="Masculino"),])/total_passageiros)*100,digits = 0)
  df_total <- data.frame(total_passageiros,total_mortos,total_sobreviventes,prop_mulheres,prop_homens)
  return(df_total)
}

#funcao para criar o grafico de mortos e sobreviventes
criar_grafico_pizza<-function(df_total){
  library(plotly)
  library(ggplot2)

  dados_grafico <- data.frame(valores = c(df_total$total_mortos,df_total$total_sobreviventes), info = c("Mortos","Sobreviventes"))
  p<-plot_ly(dados_grafico,
          type="pie",
          labels = ~dados_grafico$info ,
          values= ~valores,
          textinfo="Percent",
          marker=list(colors = c("#ff9999","#80aaff")), lines=c("rgb(0,0,205)","rgb(178,34,34)"),
          showlegend = F)%>%
        layout(showlegend= F)


  p
}

#criar grafico Mortos/Sobreviventes por classe de embarque
gerar_grafico_classes_embarque<-function(df_titanic){
  library(plotly)

  df_titanic_categorizacao <- passageiros_porsituacao_embarque(df_titanic)
  plot_ly(df_titanic_categorizacao, x = ~Classe, y = ~Sobreviventes, type = 'bar', name = 'Sobreviventes',
          marker = list(color = 'rgb(158,202,225)',
                        line = list(color = 'rgb(8,48,107)',
                                    width = 1.5))) %>%
    add_trace(y = ~Mortos, name = 'Mortos',
              marker = list(color = 'rgb(239,137,137)',
                            line = list(color = 'rgb(239,20,20)',
                                        width = 1.5))) %>%

    layout(
      xaxis = list(title = 'Classe'),
      yaxis = list(title = 'Quantidade Passageiros'),
      barmode = 'stack', barmode="group")
}

#criar mapa no r com as cidades do embarque e quantidade de passageiros
gerar_mapa_sobreviventes<-function(df_titanic){
  library(reshape)
  library(ggplot2)
  library(plotly)

  names(df_titanic)
  #montar dataset para as cidades de embarque
  df_cidade<-as.data.frame.matrix(table(df_titanic$Cidade_Embarque,df_titanic$Sobreviveu))
  df_cidade$cidades<- c("Cherbourg","Queenstown","Southampton")
  #Definindo os paises para utilizacao no plot_geo
  df_cidade$Pais <- c("FRA","IRL","GBR")
  names(df_cidade) <- c("Mortos","Sobreviventes","cidades","Pais")
  df_cidade$lat<-c(48.86,53.27,55.37)
  df_cidade$long<-c(2.34,-7.77,-3.43)

  #pivotar o dataframe para paises, latitude e longitude
  df_cidade <- melt(df_cidade,id = c("Pais","lat","long","cidades"))

  #Defino que o mapa vai plotar apenas o escopo do continente Europeu
  g <- list(
    scope = 'europe')

  plot_geo(df_cidade, locationmode="country code", color = "blue", fill="blues") %>%
    add_markers(
      y = ~lat, x = ~long, locations = ~Pais,
      size = ~value,
      color = ~variable,
      text = ~paste(value, variable , "<br> em ", cidades)

    ) %>%
    layout(geo = g
    )
}

proporção_passageiros <-function(df_titanic){
  df_prop <-as.data.frame(prop.table(table(df_titanic$Sexo)))
  return(df_prop)
}

#inicio do programa
df_titanic_bruto <- leitura_arquivo()
df_titanic <- tratamento_dados(df_titanic_bruto)
df_total<-totalizacao_dashboard(df_titanic)
criar_grafico_pizza(df_total)
gerar_grafico_classes_embarque(df_titanic)
gerar_mapa_sobreviventes(df_titanic)
df_prop <- proporção_passageiros(df_titanic)
#df_prop[1,Freq]
