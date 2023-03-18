funçao_grafico <-function(candidato){
  data<-gtrends(c(candidato), time= "2020-01-01 2020-10-30", geo = "BR")
  time_trend<-data$interest_over_time %>%
    mutate(hits=ifelse(hits=="<1",0.5,hits),
           date=as.Date(date),
           keyword=factor(keyword, levels = candidato))
  plot<-ggplot(data=time_trend, aes(x=date, y=as.numeric(hits), colour=keyword)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(as.Date("2020-09-26"))) +
    theme_bw() +
    scale_y_continuous(breaks = NULL) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text=element_text(size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  return(plot)
}

andrea_plot <- funçao_grafico(candidato = 'andrea matarazzo')
andrea_plot
  


andrea_plot <- grafico(candidato = 'andrea matarazzo')
antonio_plot <- grafico(candidato = 'antonio carlos')
arthur_plot <- grafico(candidato = 'arthur do val')
covas_plot <- grafico(candidato = 'bruno covas')
celso__plot <- grafico(candidato = 'celso russomanno')
boulos__plot <- grafico(candidato = 'guilherme boulos')
tatto__plot <- grafico(candidato = 'jilmar tatto')
joice_plot <- grafico(candidato = 'joice hasselmann')
levy_plot <- grafico(candidato = 'levy fidelix')
marcio_plot <- grafico(candidato = 'marcio frança')
helou_plot <- grafico(candidato = 'marina helou')
orlando_plot <- grafico(candidato = 'orlando silva')
vera_plot <- grafico(candidato = 'vera lucia')

(andrea_plot) +
  (antonio_plot) +
  (arthur_plot) +
  (covas_plot) +
  (celso__plot) +
  (boulos__plot) +
  (tatto__plot)+
  (joice_plot)+
  (levy_plot)+
  (marcio_plot)+
  (helou_plot)+
  (orlando_plot)+
  (vera_plot)+
  plot_annotation(title = 'Frequencia de pesquisas no Google dos candidatos a prefeito em São Paulo', 
                  caption = '')


teste <-function(candidato){
  data<-gtrends(c(candidato), time= "2020-01-01 2020-10-30", geo = "BR-SP-São Paulo - SP")
  time_trend<-data$interest_over_time %>%
    mutate(hits=ifelse(hits=="<1",0.5,hits),
           date=as.Date(date),
           keyword=factor(keyword, levels = candidato))
  plot<-ggplot(data=time_trend, aes(x=date, y=as.numeric(hits), colour=keyword)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(as.Date("2020-09-26"))) +
    theme_bw() +
    scale_y_continuous(breaks = NULL) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text=element_text(size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  return(plot)
}



(andrea_plot) +
  (antonio_plot) +
  (arthur_plot) +
  (covas_plot) +
  plot_annotation(title = 'Frequencia de pesquisas no Google dos candidatos a prefeito em São Paulo')

(andrea_plot)+ (antonio_plot)+
  plot_annotation(title = 'Frequencia de pesquisas no Google dos candidatos a prefeito em São Paulo',
                  caption = 'Dados obtidos atrvés do pacote gtrendsR, entre os dias 01-01-2020 e 30-10-2020',
                  tag_levels = 1,
                  tag_prefix = 'a')
    



pTeste <- teste(candidato = 'andrea matarazzo')




View(data(countries))
getOption('max.print', data("countries"))
data("countries")
getOption('max.print')

View(countries)

###########


testefla <- gtrends('flamengo', geo = 'BR-SP-São Paulo', time = '2020-10-01 2020-11-01')
plot(testefla)

testefla2 <- gtrends('flamengo', geo = 'BR-SP-SP', time = '2020-10-01 2020-11-01')


rm(testplot)

testplot <-function(candidato){
  data<-gtrends(c(candidato), time= "'2020-01-01 2020-10-30", geo = "BR-SP")
  time_trend<- data$interest_over_time %>%
    mutate(hits=ifelse(hits=="<1",0.5,hits),
           date=as.Date(date),
           keyword=factor(keyword, levels = candidato))
  plot<-ggplot(data=time_trend, aes(x=date, y=as.numeric(hits), colour=keyword)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(as.Date("2020-08-11"))) +
    theme_bw() +
    scale_y_continuous(breaks = NULL) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text=element_text(size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  return(plot)
}

p1 <- testplot(candidato = 'prefeito')

p2 <- testplot(candidato = 'filipe sabará')
p2

library(dplyr)


ggplot(as.data.frame(search_prefeito))+
  geom_line()
