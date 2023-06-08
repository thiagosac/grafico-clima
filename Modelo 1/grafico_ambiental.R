################################################################################
#                                                                              #
# Desenvolvedor: THIAGO ARA?JO DOS SANTOS                                      #
#                                                                              #
# Contato: thiagosantosac96@outlook.com                                        #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Reiniciando sessão R                                                                       
# -----------------------------------------------------------------------------#
  rm(list=ls())
  graphics.off()
  options(warn=0)
  .rs.restartR()

#------------------------------------------------------------------------------#
# Checando pacotes necessários                                
# -----------------------------------------------------------------------------#
  .packages = c("xlsx", "ggplot2", "pipeR", "lubridate",
                                                  "ggThemeAssist")
  .inst <- .packages %in% installed.packages()
  lapply(.packages, require, character.only=TRUE)
  
#------------------------------------------------------------------------------#
# Informando o diretório de trabalho                                                                       
# -----------------------------------------------------------------------------#
  setwd("~/GitHub/grafico-clima")
  getwd()
  dir()

#------------------------------------------------------------------------------#
# Carregando dados                                                            
# -----------------------------------------------------------------------------#
  Data <- read.xlsx(file="dados_A108_D_2019-12-01_2020-03-31.xlsx", 
                sheetName="dados_A108_D_2019-12-01_2020-03", 
                h=T, 
                row.names=)

#------------------------------------------------------------------------------#
# Plotando em escalas determinadas cada variável                                         
# -----------------------------------------------------------------------------#
  # Precipitação
  Data %>>% ggplot() + 
  geom_bar(mapping = aes(x = Data, y = PREC), stat = "identity") + 
                              scale_x_date(date_breaks = "15 days", 
                                           date_labels = "%D") +
  ylab("Precipitação (mm)") 

  # Temperatura Mínima
  Data %>>% ggplot() + 
  geom_line(mapping = aes(x = Data, y = TEMP.MI, group=2)) + 
  scale_x_date(date_breaks = "14 days",date_labels = "%D") +
  ylab(expression("Temperatura mínima ("~degree~"C)"))

  # Temperatura Média
  Data %>>% ggplot() + 
  geom_line(mapping = aes(x = Data, y = TEMP.ME, group=3)) + 
  scale_x_date(date_breaks = "14 days",date_labels = "%D") +
  ylab(expression("Temperatura média ("~degree~"C)"))
   
  # Temperatura Máxima
  Data %>>% ggplot() + 
  geom_line(mapping = aes(x = Data, y = TEMP.MA, group=4)) + 
  scale_x_date(date_breaks = "14 days",date_labels = "%D") +
  ylab(expression("Temperatura máxima ("~degree~"C)"))

#------------------------------------------------------------------------------#
# Plotanto todas as vaiáveis em um gráfico
# -----------------------------------------------------------------------------#
  gp1 <- Data %>>% ggplot() + 
  geom_bar(mapping = aes(x = Data, y = PREC * 40/ 200, fill="Precipitação"),
                                                          stat = "identity") +
    
  geom_line(mapping = aes(x = Data, y = TEMP.MI, group=2, 
                                                  linetype ="Temp. Mínima")) +
    
  geom_line(mapping = aes(x = Data, y = TEMP.ME, group=3, 
                                                  linetype ="Temp. Média")) + 
    
  geom_line(mapping = aes(x = Data, y = TEMP.MA, group=4, 
                                                  linetype="Temp. Máxima")) +
    
  scale_x_date(date_breaks = "14 days",date_labels ="%d/%m" ) +
    
  scale_y_continuous(name = expression("Temperatura ("~degree~"C)"), 
                                              limits = c(0, 40)) + labs(x = "")
  
  ggsave ("Gráfico1.tiff", width = 15, height = 10, units = "cm")
  gp1
  dev.off()

#------------------------------------------------------------------------------#
# Aplicando algumas melhorias na viasualização
# -----------------------------------------------------------------------------#
  gp2 = gp1 + scale_y_continuous(name = expression("Temperatura ("~degree~"C)"),
                                            sec.axis = sec_axis(~ . * 200 / 40 ,
                                                name = "Precipitação (mm)"), 
                                                            limits = c(0, 40))+
    
  scale_fill_manual("", values = "darkgray") +
  scale_linetype_manual("", values = c("solid", "dashed","twodash")) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))

  ggsave ("Gráfico2.tiff", width = 15, height = 10, units = "cm")
  gp2
  dev.off()
  
#------------------------------------------------------------------------------#
# Determinando anotações de objetos ao gráfico
# -----------------------------------------------------------------------------#
  anno <- data.frame(x1 = as.Date(c("2019-12-01", "2020-01-01" )), 
                     x2 = as.Date(c("2019-12-31", "2020-03-31" )), 
                     y1 = c(38, 38), 
                     y2 = c(40, 40), 
                     xstar = c(3, 2),
                     ystar = c(38, 38),
                     lab = c("2019", "2020"),
                     region = c("2019", "2020"))
  
  anno2 <- data.frame(x1 = as.Date(c("2019-12-07", "2019-12-20", "2020-01-04", 
                                                  "2020-01-20", "2020-03-25")),
                      
                      x2 = as.Date(c("2019-12-07", "2019-12-19", "2020-01-04", 
                                                  "2020-01-20","2020-03-25" )),
                      
                      y1 = c(7,7, 7,7,7 ), 
                      y2 = c(15,18,15,15,15), 
                      xstar = c(1,2,3,4,5),
                      ystar = c(15,18,15,15,15))
  
  gp3 = gp2 + geom_segment(data = anno, aes(x = x1, xend = x1, y = y1, 
                                                             yend = y2)) +
    
    geom_segment(data = anno, aes(x = x2, xend = x2, y = y1, yend = y2)) +
    geom_segment(data = anno, aes(x = x1, xend = x2, y = y2, yend = y2))
  gp3
  
  gp4= gp3 + geom_segment(data = anno2, aes(x = x1, xend = x1, 
                                            y = y2, yend = y1), 
                                        arrow = arrow(length = unit(0.1, "cm")))
  
  gp4
  
#------------------------------------------------------------------------------#
# Determinando anotações de texto ao gráfico
# -----------------------------------------------------------------------------#
  annotation <- data.frame(
    x = as.Date(c("2019-12-15", "2020-02-15")),
    y = c(40,40),
    label = c("2019", "2020"))
  
  gp5= gp4 + geom_label(data=annotation, aes(x=x, y=y, label=label),      
                        color="black", 
                        size=3.5 , angle=0, fontface="bold" )
  gp5
  
  annotation2 <- data.frame(
    x = as.Date(c("2019-12-07", "2019-12-20", "2020-01-04", "2020-01-20", 
                                                                "2020-03-25")),
    
    y = c(16,19,16,16,16),
    label = c("Semeadura","Aplicação dos tratamentos","1° Cobertura", 
                                                    "2° Cobertura","Colheita"))
  
  gp6= gp5 + geom_label(data=annotation2, aes(x=x, y=y, label=label),      
                        color="black", 
                        size=3.5 , angle=0, fontface="bold" )
  
  gp6
  
#------------------------------------------------------------------------------#
# Aplicando algumas melhorias na viasualização
# -----------------------------------------------------------------------------#
  gp7= gp6 + theme(axis.line = element_line(size = 0.5, 
                                            linetype = "solid"), 
                   axis.ticks = element_line(size = 1), 
                   axis.title = element_text(size = 11), 
                   axis.text = element_text(size = 11, colour = "black"), 
                   axis.text.x = element_text(size = 11), 
                   axis.text.y = element_text(size = 11), 
                   legend.text = element_text(size = 13), 
                   panel.background = element_rect(fill = NA), 
                   legend.key = element_rect(fill = NA), 
                   legend.background = element_rect(fill = NA), 
                   legend.position = "top", legend.direction = "horizontal")
  
  ggsave ("Gráfico3.tiff", width = 20, height = 10, units = "cm")
  gp7
  dev.off()
  
  
  
