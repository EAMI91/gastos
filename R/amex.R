library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(glue)
# list.files("data",full.names = T) %>% map(~read_lines(.x))

# Amex --------------------------------------------------------------------
Sys.setlocale(locale="es_ES.UTF-8")
amex <- read_excel("data/summary.xls",skip = 11)
names(amex) <- c("fecha","fecha_compra","descripcion","tarjetahabiente","monto","monto_extranjera","tipo_cambio","negocios_como","direccion","informacion")


amex <- amex %>% mutate(fecha = dmy(fecha), fecha_compra = dmy(fecha_compra),
                        monto = parse_number(str_replace(monto, "-\\$", "$-")),
                        monto_extranjera = parse_number(str_replace(monto_extranjera, "-\\$", "$-")),
                        tipo_cambio = parse_number(str_replace(tipo_cambio, "-\\$", "$-")))
acum <- amex
pagos <- amex %>% filter(grepl(x = descripcion,pattern = "GRACIAS POR SU PAGO EN"))
amex <- amex %>% filter(!grepl(x = descripcion,pattern = "GRACIAS POR SU PAGO EN"))


# transacciones -----------------------------------------------------------

amex %>% count(descripcion, wt = monto, name = "monto") %>% 
  ggplot(aes(y = fct_reorder(descripcion,monto), x = monto))+ geom_col()


# mensualidades -----------------------------------------------------------

mens <- amex %>% filter(grepl("MONTO",descripcion)) %>% 
  select(monto, fecha) %>% filter(monto > 0) %>% 
  mutate(mensualidad = monto / 6,
         fecha_final = fecha + months(5)) %>% 
  group_by(fecha)

mens_aux <- mens %>% complete(fecha =  seq.Date(min(fecha), min(fecha_final), by = "month"), 
                              mensualidad = mensualidad) %>% ungroup %>% 
  mutate(corte = cut(fecha-days(1),
                     seq(min(fecha),max(fecha_final,na.rm = T), by = "month")
                     )) %>% 
  count(corte, wt = mensualidad, name = "mensualidad") %>% mutate(fecha = ymd(corte))

mens_aux %>% 
  ggplot(aes(x = fecha, y = mensualidad)) + 
  geom_col(aes(fill = fecha < floor_date(today(),unit = "month")),show.legend = F) +
  geom_label(aes(label = dollar(mensualidad,accuracy = 1)), hjust = "center", vjust = 0, nudge_y = 100) +
  geom_label(aes(label = month(fecha,abbr = T,label = T)), hjust = "center", vjust = 0, nudge_y = 1000) + 
  scale_y_continuous(label = dollar) + labs(x = NULL, y = NULL) + ggtitle("Mensualidades sin intereses")
# mes a mes ---------------------------------------------------------------

gasto_aux <- amex %>% filter(!grepl("MESES EN AUTOMÁTICO",descripcion),
                             is.na(monto_extranjera)) 


gasto_aux <-
  gasto_aux %>% anti_join(mens, by = c("monto")) %>%
  mutate(corte = cut(fecha,c(seq(ymd(20200211),today(),by = "month"),today()))) %>% 
  count(fecha = corte, wt = monto, name = "mensualidad") %>% mutate(fecha = ymd(fecha))

gasto_aux %>% 
  ggplot(aes(x = fecha, y = mensualidad)) + 
  geom_col(aes(fill = fecha < floor_date(today(),unit = "month")),show.legend = F) +
  geom_label(aes(label = dollar(mensualidad,accuracy = 1)), hjust = "center", vjust = 0, nudge_y = 100) +
  geom_label(aes(label = month(fecha,abbr = T,label = T)), hjust = "center", vjust = 0, nudge_y = 3200) + 
  scale_y_continuous(label = dollar) + labs(x = NULL, y = NULL) + ggtitle("Gasto mensual")

bind_rows(gasto_aux %>% mutate(tipo = "Gasto mensual"),
          mens_aux %>% mutate(tipo = "Mensualidades sin intereses")
) %>% 
  ggplot(aes(x = fecha, y = mensualidad)) + 
  geom_col(aes(fill = fecha < floor_date(today(),unit = "month")),show.legend = F) +
  geom_label_repel(aes(label = glue("{month(fecha,abbr = T,label = T)} {dollar(mensualidad,accuracy = 1)}")), hjust = "center", vjust = 0, nudge_y = 1200) + 
  scale_y_continuous(label = dollar) + labs(x = NULL, y = NULL) + facet_wrap(~tipo)


dia <- amex %>% group_by(fecha = floor_date(fecha,unit = "day")) %>% summarise(monto = sum(monto))
mes <- dia %>% group_by(fecha = floor_date(fecha,unit = "month")) %>% summarise(monto = sum(monto)) %>% 
  mutate(fecha_end = lead(fecha)) %>% replace_na(list(fecha_end = ceiling_date(today(),unit =  "month")))

amex %>% filter(floor_date(fecha) == ymd("2021-04-01"))
dia %>% ggplot(aes(x = fecha, y = monto)) + geom_col() + 
  geom_segment(data = mes, aes(x = fecha, xend = fecha_end, yend = monto), color = "red", alpha = .5) +
  geom_label(data = mes, aes(x = fecha + days(14), label = month(fecha,label = T,abbr = F)), 
             vjust = 0, nudge_y = 1000)



pagos_acum <- acum %>% 
  complete(fecha = seq(floor_date(today(),"year"), today(), by = "day"), fill = list(monto = 0)) %>% 
  arrange(fecha) %>% 
  count(fecha, wt = monto,name = "monto") %>% 
  mutate(acum = cumsum(monto))

pagos_acum %>% 
  ggplot(aes(x = fecha, y = acum)) + geom_col()

