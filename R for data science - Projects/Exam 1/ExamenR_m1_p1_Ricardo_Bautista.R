
# 1. Importar WB-DATA.csv y WB-METADATA.csv ----
library(readr)

df1 <- read_csv("WB-DATA.csv")

df2 = read_csv("WB-METADATA.csv")

# 2. Crear una tabla, que sea la union (inner_join) de las dos tablas anteriores  ----

# nota: la variable llave en ambas tablas es series_id

library(dplyr)

df12 = inner_join(df1, df2, by="series_id")

# 3. ¿Cuantos indicadores (series_id) unicos tiene df12? ----

length(unique(df12$series_id))

# 7 valores unicos

# 4. ¿Cuantos registros tiene cada indicador? ----

table(df12$series_id)
'TX.VAL.TRVL.ZS.WT  1998              
VC.BTL.DETH         1111
VC.IDP.NWCV         400
VC.IDP.NWDS         1535
VC.IDP.TOCV         668 
VC.IHR.PSRC.P5      3686
VC.PKP.TOTL.UN      140
'
# 5. Crear una data, que considere la siguiente seleccion de registros: ----

# series_id: VC.IHR.PSRC.P5
# country_name: Peru y Brazil
# year: mayor o igual a 2008

df3 = df12[df12$series_id=="VC.IHR.PSRC.P5" & 
             (df12$country_name=="Peru" | df12$country_name=="Brazil") &
             df12$year >= 2008, ]
  
# 6. ¿Qué significa el indicador VC.IHR.PSRC.P5? ----

#Homicidios intencionales (por cada 100.000 habitantes)

# 7. En la data, ordenar los registros por country_name y year (ascendente) ----

df3 = arrange(df3,country_name, year)


# 8. En la data, redondear value a dos decimales ----

df3$value = round(df3$value, digits = 2)

# 9. Hacer un grafico de lineas (tendencia) de value por año y pais ----

# tip: revisar el script: Caso_TendenciaMuertes.R , en la ultima parte
# se desarrollan diversos graficos de lineas (tendencia)

library(ggplot2)
#Peru
m_pe <- subset(df3, country_code=="PER")

ggplot(m_pe, aes(x=year, y=value, group=1))+ geom_point() + geom_line()+
  labs(title="Homicidios intencionales (por cada 100.000 habitantes) en Peru") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(breaks = seq(min(m_pe$value), 
                                  max(m_pe$value), by = 1))

#Brazil
m_br <- subset(df3, country_code=="BRA")

ggplot(m_br, aes(x=year, y=value, group=1))+ geom_point() + geom_line()+
  labs(title="Homicidios intencionales (por cada 100.000 habitantes) en Brasil") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_continuous(labels = scales::number_format(accuracy = 1))

