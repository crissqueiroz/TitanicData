#
# Exemplo de Dashboard com o Shiny
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)



#Criar o corpo do dashboard
body<-dashboardBody(

  #Caixa com Total de Passageiros do Titanic
  fluidRow(

          # Total de Passageiros
          valueBoxOutput(outputId = "quantidadeBox",width = 4),
          #percentual de mulheres
          valueBoxOutput(outputId = "mulheresBox", width = 4),
          #percentual de homens
          valueBoxOutput(outputId = "homensBox", width = 4)),
  fluidRow(
      #gráfico de pizza com o percentual de mortos e sobreviventes
      box( title = "Mortos x Sobreviventes", width = "5",solidHeader = F,
           plotlyOutput("pizzaplot",height = "250px", width = "250px")
      ),
      #grafico de pizza com mortos/sobreviventes por classe de embarque
      box( title = "Mortos/Sobreviventes por classe de Embarque",  solidHeader = F,width = "7",
           plotlyOutput("barraplot", height = "250px", width = "450px")
      )

    ),
  fluidRow(
    #mapa de sobreviventes do Titanic
    box(title = "Mortos/Sobreviventes por país de embarque", solidHeader = F, width = 12 ,
            plotlyOutput("mapasobreviventes")
        )
  )




)


ui <- dashboardPage(

    titulo<-dashboardHeader(title = "Titanic´s Dashboard"),
    dashboardSidebar(disable = TRUE),
    body

)

