# ---------------------------------------------------------------------------------------------
# PROJEKT: Program dokonuje analizy częstości słów i analizy sentymentu w zbiorze 10000 wierszy
# Autorzy: Jakub Rutkowski, Mikołaj Wojtkowski, Marek Grajewski
# ---------------------------------------------------------------------------------------------


#Wczytujemy potrzebne pakiety

library(tm)
library(tidytext)
library(tidyverse)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(SnowballC)
library(ggthemes)
library(readr)



# Funkcja która przetworzy tekst z apostrofami i stemmingiem

process_text_vector <- function(text_column) {
  
  # Łączymy wszystkie wierszy w jeden wielki ciąg tekstowy i małe litery
  full_text <- tolower(paste(text_column, collapse = " "))
  
  # Zamieniamy wszystkie rodzaje apostrofów na klasyczny '
  full_text <- gsub("[\u2019\u2018\u0060\u00B4]", "'", full_text)
  
  # Usuwamy cyfry
  full_text <- removeNumbers(full_text)
  
  # Dzielimy cały tekst na pojedyncze słowa
  words <- unlist(strsplit(full_text, "\\s+"))
  
  # Usuwamy puste elementy
  words <- words[words != ""]
  
  # Usuwamy słowa zawierających apostrof
  words <- words[!str_detect(words, "'")]
  
  # Usuwamy interpunkcję
  words <- str_replace_all(words, "[[:punct:]]", "")
  words <- words[words != ""]
  
  # Usuwamy spację z początka i końca słów
  words <- str_trim(words)
  
  # Stopwords z pakietu tidytext
  tidy_stopwords <- tolower(stop_words$word)
  tidy_stopwords <- gsub("[\u2019\u2018\u0060\u00B4]", "'", tidy_stopwords)
  words <- words[!(words %in% tidy_stopwords)]
  
  # Stopwords z pakietu tm
  tm_stopwords <- tolower(stopwords("en"))
  tm_stopwords <- gsub("[\u2019\u2018\u0060\u00B4]", "'", tm_stopwords)
  words <- words[!(words %in% tm_stopwords)]
  
  # UTWORZENIE SŁOWNIKA DO ODZYSKIWANIA SŁÓW (Przed stemmingiem!)
  # Zapisujemy oryginalne, czyste słowa jako słownik do późniejszego completion
  global_dictionary <<- words 
  
  # Robimy SAM stemming i od razu zwracamy rdzenie
  stemmed_doc <- stemDocument(words)
  stemmed_doc <- stemmed_doc[stemmed_doc != "" & !is.na(stemmed_doc)]
  
  # Zwracamy jeden wektor ze wszystkimi słowami
  return(as.character(stemmed_doc))
}


# Funkcja którą użyjemy do zliczania ilości słów
word_frequency <- function(words) {
  freq <- table(words)
  freq_df <- data.frame(word = names(freq), freq = as.numeric(freq))
  freq_df <- freq_df[order(-freq_df$freq), ]
  return(freq_df)
}



# Funkcja, którą użyjemy do chmury słów
plot_wordcloud <- function(freq_df, title, color_palette = "Dark2") {
  wordcloud(words = freq_df$word, freq = freq_df$freq, min.freq = 10,max.words = 50,
            colors = brewer.pal(8, color_palette))
  title(title)
}


# Wczytujemy nasze dane
df <- read.csv("PoemDataset.csv", stringsAsFactors = FALSE)

# Pobieramy unikalne gatunki
gatunki <- unique(df$Genre)

# Dzielimy w zależności od Genre
for (g in gatunki){
  nazwa_zmiennej <- make.names(paste0("Poem_", g))
  wektor_tymczasowy <- unique(df[df$Genre == g, "Poem"])
  assign(nazwa_zmiennej, wektor_tymczasowy)
}

# Usuwamy zmienne pomocnicze
rm(nazwa_zmiennej, wektor_tymczasowy)


for (g in gatunki){
  # Pobieramy same rdzenie (stemmy)
  stemmed_words <- process_text_vector(get(paste("Poem_", g, sep = "")))
  
  # Obliczamy częstość występowania dla wszystkich rdzeni
  freq_df <- word_frequency(stemmed_words)
  
  # Wycinamy tylko TOP 70 najczęstszych rdzeni
  freq_df_top50 <- head(freq_df, 50)
  
  # Robimy stem completion
  completed_words <- stemCompletion(freq_df_top50$word, dictionary = global_dictionary, type = "prevalent")
  
  # Czasami stemCompletion zwraca puste wartości, jeśli nie znajdzie dopasowania, jeśli tak się stanie, zostawiamy oryginalny rdzeń.
  completed_words[completed_words == "" | is.na(completed_words)] <- freq_df_top50$word[completed_words == "" | is.na(completed_words)]
  
  # Podmieniamy ucięte słowa na odzyskane pełne słowa
  freq_df_top50$word <- as.character(completed_words)
  
  # Tworzymy chmurę słów
  plot_wordcloud(freq_df_top50, paste("Poem_", g, sep = ""), "Dark2")
  
  # Wyświetlamy 10 najczęściej występujących słów
  cat("\n Najczęściej występujące słowa w wierszach z gatunku", tolower(g), ":\n\n")
  print(knitr::kable(head(freq_df_top50, 10)))
  
  # Dodatkowy odstęp po tabeli
  cat("\n\n")
}

#Analiza sentymentu 


# Wczytujemy słowniki z plików csv
afinn <- read.csv("afinn.csv", stringsAsFactors = FALSE)
bing <- read.csv("bing.csv", stringsAsFactors = FALSE)
loughran <- read.csv("loughran.csv", stringsAsFactors = FALSE)
nrc <- read.csv("nrc.csv", stringsAsFactors = FALSE)


# Przygotowanie danych tekstowych i tokenizacja 

# Wyciągamy kolumnę "Poem"
tokeny_data <- df %>% 
  filter(!is.na(Poem)) %>%            
  select(Review = Poem)

# Tokenizacja i usuwanie stopwords
tidy_tokeny <- tokeny_data %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words) 

# Podgląd wyczyszczonych słów
cat("\n### Podgląd oczyszczonych tokenów (Top 10):\n\n")
print(knitr::kable(head(tidy_tokeny, 10)))
cat("\n\n")


# Dodatkowe czyszczenie na potrzeby analiz szczegółowych
tidy_tokeny2 <- tokeny_data %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words)





# 1. Analiza sentymentu przy użyciu słownika Loughran 

# Zliczanie ogólne sentymentu
sentiment_review <- tidy_tokeny %>%
  inner_join(loughran, relationship = "many-to-many")

loughran_summary <- sentiment_review %>%
  count(sentiment)

cat("\n Podsumowanie sentymentu wg słownika Loughran:\n\n")
print(knitr::kable(loughran_summary, col.names = c("Sentyment", "Liczba słów")))
cat("\n\n")

# Zliczanie najczęstszych słów
loughran_top_words <- sentiment_review %>%
  count(word, sentiment) %>%
  arrange(desc(n))

cat("\n Najczęstsze słowa w wierszach ze słownika Loughran (Top 10):\n\n")
print(knitr::kable(head(loughran_top_words, 10), col.names = c("Słowo", "Sentyment", "Liczba")))
cat("\n\n")

# Filtrowanie i wizualizacja
sentiment_review2 <- sentiment_review %>%
  filter(sentiment %in% c("positive", "negative"))

word_counts <- sentiment_review2 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )

# Wykres Loughran
ggplot(word_counts, aes(x=word2, y=n, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów w wierszach wg sentymentu (Loughran)") +
  scale_fill_manual(values = c("firebrick", "darkolivegreen4"))





# 2. Analiza sentymentu przy użyciu słownika NRC 

# Zliczanie ogólne sentymentu
sentiment_review_nrc <- tidy_tokeny %>%
  inner_join(nrc, relationship = "many-to-many")

nrc_summary <- sentiment_review_nrc %>%
  count(sentiment)

cat("\n Podsumowanie sentymentu wg słownika NRC:\n\n")
print(knitr::kable(nrc_summary, col.names = c("Sentyment", "Liczba słów")))
cat("\n\n")

# Agregacja powtarzających się słów i łączenie ich emocji w jeden wiersz
nrc_top_words_unique <- sentiment_review_nrc %>%
  group_by(word) %>%
  summarise(
    Liczba = n(),
    Sentymenty = stringr::str_flatten_comma(unique(sentiment))
  ) %>%
  arrange(desc(Liczba))

cat("\n Najczęstsze słowa w wierszach ze słownika NRC (TOP 10):\n\n")
print(knitr::kable(head(nrc_top_words_unique, 10), col.names = c("Słowo", "Liczba wystąpień", "Przypisane sentymenty")))
cat("\n\n")

# Filtrowanie i wizualizacja dla NRC
sentiment_review_nrc2 <- sentiment_review_nrc %>%
  filter(sentiment %in% c("positive", "negative"))

word_counts_nrc2 <- sentiment_review_nrc2 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )

# Wykres NRC
ggplot(word_counts_nrc2, aes(x=word2, y=n, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów w wierszach wg sentymentu (NRC)")





# 3. Analiza sentymentu przy użyciu słownika Bing

# Zliczanie sentymentu
sentiment_review_bing <- tidy_tokeny %>%
  inner_join(bing)

bing_summary <- sentiment_review_bing %>%
  count(sentiment)

cat("\n Podsumowanie sentymentu wg słownika Bing:\n\n")
print(knitr::kable(bing_summary, col.names = c("Sentyment", "Liczba słów")))
cat("\n\n")

# Zliczanie najczęstszych słów
bing_top_words <- sentiment_review_bing %>%
  count(word, sentiment) %>%
  arrange(desc(n))

cat("\n Najczęstsze słowa w wierszach ze słownika Bing (TOP 10):\n\n")
print(knitr::kable(head(bing_top_words, 10), col.names = c("Słowo", "Sentyment", "Liczba")))
cat("\n\n")

# Filtrowanie i wizualizacja
sentiment_review_bing2 <- sentiment_review_bing %>%
  filter(sentiment %in% c("positive", "negative"))

word_counts_bing2 <- sentiment_review_bing2 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )

# Wykres Bing
ggplot(word_counts_bing2, aes(x=word2, y=n, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów w wierszach wg sentymentu (Bing)") +
  scale_fill_manual(values = c("dodgerblue4", "goldenrod1"))





# 4. Analiza sentymentu przy użyciu słownika AFINN

# Zliczanie sentymentu
sentiment_review_afinn <- tidy_tokeny %>%
  inner_join(afinn)

afinn_summary <- sentiment_review_afinn %>%
  count(value)

cat("\n Podsumowanie sentymentu wg słownika AFINN (wartości punktowe):\n\n")
print(knitr::kable(afinn_summary, col.names = c("Wartość emocjonalna", "Liczba słów")))
cat("\n\n")

# Zliczanie najczęstszych słów
afinn_top_words <- sentiment_review_afinn %>%
  count(word, value) %>%
  arrange(desc(n))

cat("\n Najczęstsze słowa w wierszach ze słownika AFINN (TOP 10):\n\n")
print(knitr::kable(head(afinn_top_words, 10), col.names = c("Słowo", "Wartość punktowa", "Liczba")))
cat("\n\n")

# Filtrowanie silnych emocji i wizualizacja
sentiment_review_afinn3 <- sentiment_review_afinn %>%
  filter(value %in% c("3", "-3" , "4", "-4"))

word_counts_afinn3 <- sentiment_review_afinn3 %>%
  count(word, value) %>%
  group_by(value) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )

# Wykres AFINN
ggplot(word_counts_afinn3, aes(x=word2, y=n, fill=value)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~value, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów w wierszach wg sentymentu (AFINN)")


# 5. Analiza sentymentu za pomocą słownika AFINN w podziale na gatunki

# Przygotowanie tokenów z zachowaniem informacji o gatunku 
tokeny_genre <- df %>%
  filter(!is.na(Poem) & !is.na(Genre)) %>%
  select(Genre, Review = Poem) %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words)

# Połączenie tokenów ze słownikiem AFINN
sentiment_genre_afinn <- tokeny_genre %>%
  inner_join(afinn)

# Obliczenie średniego sentymentu oraz statystyk dla każdego z 6 gatunków
genre_sentiment_summary <- sentiment_genre_afinn %>%
  group_by(Genre) %>%
  summarise(
    `Liczba słów emocjonalnych` = n(),
    `Suma punktów` = sum(value),
    `Średni sentyment` = round(mean(value), 3)
  ) %>%
  arrange(desc(`Średni sentyment`))

# Wyświetlenie wyników
cat("\n Porównanie sentymentu (AFINN) dla 6 gatunków literackich:\n\n")
print(knitr::kable(genre_sentiment_summary, col.names = c("Gatunek", "Liczba słów emocjonalnych", "Suma punktów", "Średni sentyment")))
cat("\n\n")

# Wizualizacja: Średni sentyment dla każdego gatunku
ggplot(genre_sentiment_summary, aes(x = fct_reorder(Genre, `Średni sentyment`), y = `Średni sentyment`, fill = `Średni sentyment` > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    x = "Gatunek", 
    y = "Średni wskaźnik sentymentu (AFINN)",
    title = "Średni sentyment dla różnych gatunków",
    subtitle = "Wartości dodatnie oznaczają wydźwięk pozytywny, ujemne - negatywny"
  ) +
  scale_fill_manual(values = c("firebrick", "darkolivegreen4")) +
  theme_gdocs()