

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

shinyServer(function(input, output) {
  df_titanic_bruto <- leitura_arquivo()
  df_titanic <- tratamento_dados(df_titanic_bruto)
  df_total<-totalizacao_dashboard(df_titanic)
  #df_perc <-proporção_passageiros(df_titanic)


  output$quantidadeBox <- renderValueBox({
    valueBox(
      df_total$total_passageiros, "Passageiros", icon = icon("glyphicon glyphicon-user", lib = "glyphicon"),
      color = "olive"
    )


  })
?round
  output$mulheresBox <- renderValueBox({
    valueBox(
      paste(df_total$prop_mulheres, "%"), "Mulheres", icon = icon("glyphicon glyphicon-list-alt", lib = "glyphicon"),
      color = "red"
    )
  })

  output$homensBox <- renderValueBox({
    valueBox(
      paste(df_total$prop_homens,"%"), "Homens", icon = icon("glyphicon glyphicon-list-alt", lib = "glyphicon"),
      color = "light-blue"
    )
  })

  output$pizzaplot <- renderPlotly({
    criar_grafico_pizza(df_total)%>%  layout(autosize = FALSE, height = 250, width=250)
  })

  output$barraplot <- renderPlotly({
    gerar_grafico_classes_embarque(df_titanic)%>%  layout(autosize = FALSE, height = 250, width= 450)
  })

  output$mapasobreviventes <-renderPlotly({
    gerar_mapa_sobreviventes(df_titanic)%>%  layout(autosize = FALSE, height = 400)
  })

})
