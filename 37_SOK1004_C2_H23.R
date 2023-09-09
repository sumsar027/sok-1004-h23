Sys.setlocale(locale="no_NO")

# kode fra oppgavesettet
library(tidyverse)

url <- 
  "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"

df <- url %>%
  read_csv()

# I. Karbonintensitet i produksjon

list <- c("China", "United States", "India", "Japan", "Germany")

df %>%
  
  mutate(gdp = gdp/10^12) %>%
  
  mutate(co2 = co2/10^3) %>%
  
  filter(country %in% list) %>% 
  
  ggplot(aes(x=gdp, y=co2, color = country)) %>%
  
  + geom_point() %>%
  
  + geom_smooth(se = FALSE) %>%
  
  + labs(y = "CO2", x = "GDP", title = "GDP vs CO2", subtitle = "Production-based C02 emissions, billions of tons. 
GDP in trillions of USD, constant 2011 prices at purchasing power parity. 1850-2021.", color = "Country") %>%
  
  + theme_bw()

# Oppgave Ia, dr??ft figuren 

# Figuren viser en sammenheng mellom CO2 utslipp og GDP/BNP i landene Kina, Tyskland, India, Japan og USA. Grafen utvikler seg sammenhengene i en positiv kurvet vei, dette kan vi
# se ekstra godt i Kina og USA, grafen viser da en sammenheng med jo sterkere BNP landet har jo h??yere CO2 utslipp er det, n??r BNP har store verdier begynner grafen ?? gjevne seg 
# ut horisontalt som betyr at sammenhengen virker bare i en viss grad. Landene Japan, Tyskland og India er vanskeligere ?? lese siden sammenliknet med USA og Kina er de mye mindre
# ulike endringer og m??nstre, men vi kan fremdeles se en lik utvikling og sammenheng mellom CO2 utslipp og BNP. Linjene viser ogs?? bare den gjennomsnittlige gangen, dette gj??r slik
# at ulike ekstremverdier blir tydelige, Kina har en del slike verdier som g??r langt unna gjennomsnittet. Alt i alt viser grafen en god sammenheng mellom CO2 utslipp og BNP. 

# Oppgave Ib, lag figur og sammenlikn 

oppgave1b <- df %>% 
  filter(country %in% list) %>% 
  mutate(ci=co2/gdp) %>%
  ggplot(aes(y=ci, x=year, color = country)) +
  geom_line() +
  labs(x='??r', y ='co2 intensitet', title='Karbonintensiteten i ulike land over tid', color='Country') +
  theme_bw()
oppgave1b  

# Her kan vi se en graf som viser karbonintensiteten til ulike land over tid, vi jan tydelig se en ??kning fra midten av 1800 tallet, dette kommer av den industrielle revolusjonen,
# senere kan vi se at karbonintensititeten g??r sammenhengene i negativ gang, vi kan se en sammenheng med grafen fra forrige oppgave der Kina har en veldig h??y karbonintensitet p?? 
# slutten av grafen i forhold til de andre landene. Det samme gjelder i grafen fra forrige oppgave, mens USA p?? den andre siden som i graf 1 er rett under Kina er i dette tilfellet
# mye lenger ned, nesten likt med India, dette viser at karbonintenisiteten hos Kina er sv??rt sterk i forhold til de andre landene. Resten av landene ligger langt nede i co2 intensiteten 
# dette viser en lik sammenheng med graf 1.

# Oppgave Ic

oppgave1c <- df %>%
  filter(country %in% list) %>% 
  mutate(ci=co2/gdp) %>% 
  ggplot(aes(x=coal_co2, y=year, color = country)) +
  geom_point() +
  labs(x='Kullbruk', y ='??r', title='Kullforbruk i ulike land over tid', color='Country') +
  geom_smooth(se = FALSE) +
  theme_bw()
oppgave1c

oppgave1c2 <- df %>% 
  filter(country %in% list) %>% 
  ggplot(aes(y=co2, x=coal_co2, color=country)) +
  geom_point() +
  labs(x='Kullbruk', y ='co2', title='Karbonutslipp sammenliknet med kullbruk', color='Country') +
  geom_smooth(se = FALSE) +
  theme_bw()
oppgave1c2

# Her kan vi se to grafer som viser faktorene co2utslipp og kullbruk, hvis vi ser p?? graf oppgave1c kan vi se kullbruk i land over tid, starten av grafen viser lite bruk men fra rundt
# 1850 begynner endringer i grafenes gang. Her kan man tydelig se at Kina har st??rst kullforbruk av alle landene, sammenliknet med de tideligere grafene kan dette v??re grunnen til 
# at Kina sk??rer s?? h??yt p?? de forskjellige grafene om klimautslipp. Man kan se at USA har et b??lgete m??nster over tid, dette viser at de har v??rt av og p?? med bruk av kull men de
# har synket i bruken mot nyere tider, dette gjelder ogs?? Japan og Tyskland. P?? den andre siden har vi Kina og India som har relativt hatt en eksplosiv oppgang i bruk av kull, man
# kan tenke seg at dette kommer av at land som er bedre utviklet og har en bedre ??konomisk fordeling innad i landene ikke tar i bruk ressurser som Kull p?? grunn av at de er d??rligere
# generelt sett i forhold til andre ressurser som olje og str??m. 
# Hvis vi ser p?? graf oppgave1c2 kan vi se en sammenheng mellom karbonutslipp og kullbruk, igjen kan vi se at Kina skiller seg tydelig ut fra de andre med en nesten rett linje, denne
# linjen viser en tydelig sammenheng med jo mer kullbruk jo mer co2 utslipp, dette sier sier seg selv. De andre landene er langt ifra Kina b??de p?? y aksen og p?? x aksen, dette viser
# at Kina er et land som har generelt sett et veldig stor utslipp for klimagasser spesielt co2. USA har nesten like mye kullbruk som India men sk????rer fremdeles mye h??yere p?? co2 utslipp
# dette kommer av at USA slipper ut co2 gasser p?? andre m??ter i tilleg til kullbruk, dette kan v??re bilbruk, transportbruk og maskinbruk. Samtlige grafer vi har sett p?? viser en sammengheng
# Det er at Kina skiller seg godt ut p?? ?? slippe ut farlige klimagasser som co2, dette viser til at de har en del ?? jobbe med hvis de skal i fremtiden bli et mer milj??vennelig land. 


# Oppgave 2a

# De konsumbaserte co2utslipp verdiene viser til co2utslipp som kommer av konsumerte faktorer, vanligvis baserer man seg p?? produksjonsbaserte utslipp, man kan tenke seg at konsumbaserte
# verdier kommer fra befolkningen innad i landene, verdiene sier hvordan ressursene som slipper ut co2 blir brukt, deretter kommer dette til utslippsverdier av bruken deres. 

# Oppgave 2b

oppgave2a <- df %>% 
  filter(country %in% list) %>% 
  filter(year > 1990) %>% 
  ggplot(aes(y=year, x=consumption_co2, size=consumption_co2_per_capita, color=country)) +
  geom_point() +
  labs(x='Konsumbasert co2 utslipp', y ='??r', title='Konsumbasert co2 utslipp over tid', color='Country') +
  theme_bw()
oppgave2a

# Her kan vi se en graf som jeg har eksperimentert litt med, men den viser konsumbasert co2 utslipp over tid der st??rrelsen p?? boblene viser til konsumbasert co2 utslipp per person,
# vi kan se at Kina her som i alle de andre sk??rer h??yt i den totale konsumbaserte co2utslippet, men USA er ikke langt unna og hvis vi sammenligner st??rrelsen kan vi se at USA har
# tykkere bobler enn boblene til Kina, dette betyr at befolkningen i USA har h??yere konsum som f??rer til co2 utslipp enn det Kina har, men USA har en befolkning p?? 331,9 millioner
# mens Kina har en befolkning p?? 1,412 milliarder. Dette viser til at selvom Kina har en mindre konsumbasert co2 utslipp per person enn USA s?? betyr det ikke at de har mindre 
# konsumbasert co2 utslipp enn USA. N??r det gjelder per person kan det vise veldig gode sammenlikninger mellom personer i forskjellige land, men det sier ingenting om det totale.

# Oppgave 3

df %>%
  
  filter(country %in% c(list)) %>% 
  
  ggplot(aes(x=year, y=trade_co2_share, color = country)) %>%
  
  + xlim(1990,2021) %>%
  
  + geom_line(size = 1.5) %>%
  
  + geom_hline(yintercept=0) %>%
  
  + labs(x = "Year", y = "Traded CO2 share", title = "Traded CO2 as share of production-based emissions", subtitle = "CO2 embedded in trade. Share of production based emissions. Percent. 1950-2021.
Positive (negative) value indicates net imports (export) of CO2 emissions.", color = "Country") %>%
  
  + theme_bw()

# Her kan vi se en figur som viser kvoter som er handlet mellom landene, her kan vi se en utvikling der Kina og India er store forhandler n??r det gjelder eksport, mens USA, Japan og India
# er store n??r det gjelder import. 