cor_graph <- function(data, title=NULL){
  require(highcharter)
  hchart(cor(data))%>%
    hc_subtitle(text="Correlaciones")%>%
    hc_title(text=title)%>%
    hc_legend(layout = 'vertical',
              align = 'right',
              verticalAlign= 'middle')
}
