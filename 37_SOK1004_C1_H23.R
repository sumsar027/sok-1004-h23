Sys.setlocale(locale="no_NO")
# Kode fra oppgaveteksten
rm(list=ls())
library(tidyverse)
library(PxWebApiData)
lenke <- "http://data.ssb.no/api/v0/no/table/09842"
df <- lenke %>%
  ApiData()
df %>%
  print()
metadata <- lenke %>%
  ApiData(returnMetaData = TRUE)
df <- lenke %>% 
  ApiData(Tid = paste(1970:2022), ContentsCode = c("BNP","MEMOBNP"))

# Oppgave 1a, rydd i data:

df = df[-1] 
df1a <- as.data.frame(df)
df1a

df1a <- df1a %>% 
  rename("var" = "dataset.ContentsCode",
         "tid" = "dataset.Tid",
         "verdi" = "dataset.value")

df1a$tid <- as.integer(df1a$tid)
  


# Oppgave 1b, lag en figur:

df1a %>%
  filter(var == "BNP") %>%
  ggplot(aes(x=tid,y=verdi)) +
  geom_line()

df1b <- df1a %>% 
  filter(tid > 1999) %>% 
  filter(var == "BNP") %>% 
  ggplot(aes(x=tid,y=verdi)) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "BNP per person", x = "Årstall", title = "BNP per person over tid") +
  geom_line()

df1b

# Oppgave 2a, beregn vekst:

df_wide <- df1a %>%
  pivot_wider(names_from = var, values_from = verdi) # var verdiene ble satt opp som kolonner, dette gjør at man enkelt kan hente ut BNP og MEMOBNP hver for seg

df_wide <- df_wide %>%
  mutate(LBNP = lag(BNP,n=1L)) %>%
  mutate(LMEMOBNP = lag(MEMOBNP,n=1L))

# legger variablene i rekkefC8lge

df_wide <- df_wide %>%
  relocate("LBNP", .before = "MEMOBNP")

# Oppgave 2b

# Bruk funksjonen `mutate()` til C% lage en ny variabel med relativ endring i `BNP` og `MEMOBNP` i `df_wide` og lagre de som `DBNP` og `DMEMOBNP`.

df_wide <- mutate(df_wide, DBNP = ((BNP - lag(BNP)) / lag(BNP)) * 100, DMEMOBNP = ((MEMOBNP-lag(MEMOBNP)) / lag(MEMOBNP)) * 100)
df_wide

# Oppgave 2c

# Bruk nC% funksjonen `pivot_longer()` til C% transformere `df_wide` til det opprinnelige formatet, altsC% med variablene `var` og `verdi`. Kall den transformerte tabellen for `df_long`.

df_long <- df_wide %>% 
  pivot_longer(cols = BNP:DMEMOBNP, names_to = 'var', values_to = 'verdi')
df_long

# Oppgave 2d

# Lag en pen figur med prosentvis vekst i nominelt og reelt BNP per person fra 1970 til 2022. Finnes det observasjoner med negativ vekst i reell BNP? Hva skyldes dette?

