tw_tokenizer <- function(base){
  
  #reg_words <- c('rt', 'https', 't.co')
  terminos <- deparse(substitute(base))
  
  base$text <- gsub("#\\w+","",base$text)
  base$text <- gsub("@\\w+","",base$text)
  base$text <- gsub("http.*","",base$text)
  base$text <- gsub("https.*","",base$text)
  base$text <- gsub("[[:punct:]]","",base$text)
  base$text <- gsub("\\w*[0-9]+\\w*\\s*", "",base$text)
  
  tweet_token <- base %>% 
    unnest_tokens(Palabra, text) %>%
    count(Palabra, sort=TRUE) %>%
    filter(!Palabra%in%stopwords('es')) %>%
    #filter(!Palabra%in%reg_words) %>%
    filter(Palabra!=terminos) %>% 
    filter(str_detect(Palabra, "^[a-zA-z]|^#|^@")) %>%
    ungroup() %>%
    arrange(desc(n)) %>% 
    mutate(word=Palabra,
           freq=n) %>% 
    select(word, freq)
  
}

tw_tokenizer_n <- function(base, num=1){
  
  reg_words <- c('rt', 'https', 't.co')
  #terminos <- deparse(substitute(base))
  
  base$text <- gsub("#\\w+","",base$text)
  base$text <- gsub("@\\w+","",base$text)
  base$text <- gsub("http.*","",base$text)
  base$text <- gsub("https.*","",base$text)
  base$text <- gsub("[[:punct:]]","",base$text)
  base$text <- gsub("\\w*[0-9]+\\w*\\s*", "",base$text)
  
  tweet_token <- base %>% 
    unnest_tokens(Palabra, text, token='ngrams', n = num) %>%
    separate(Palabra, c('word1', 'word2'), sep=' ') %>% 
    filter(!word1%in%stopwords('es')) %>% 
    filter(!word2%in%stopwords('es')) %>% 
    filter(!word1%in%reg_words) %>% 
    filter(!word2%in%reg_words) %>% 
    filter(str_detect(word1, "^[a-zA-z]|^#|^@")) %>%
    filter(str_detect(word2, "^[a-zA-z]|^#|^@")) %>%
    count(word1, word2, sort=TRUE) %>%
    unite(Palabra, word1, word2, sep=' ') %>% 
    ungroup() %>%
    arrange(desc(n)) %>% 
    mutate(word=Palabra,
           freq=n) %>% 
    select(word, freq)
  
}

tw_sentiment <- function(base){
  
  base_text <- gsub("http.*","",base$text)
  base_text <- gsub("https.*","",base_text)
  
  base_text <- gsub("#\\w+","",base_text)
  base_text <- gsub("@\\w+","",base_text)
  
  
  base_text <- gsub("[[:punct:]]","",base_text)
  base_text <- gsub("\\w*[0-9]+\\w*\\s*", "",base_text)
  
  
  base_vector <- as.vector(base_text)
  
  base_emocion <- get_nrc_sentiment(char_v = base_vector, language = "spanish") %>% 
    rename('anticipación'=anticipation,
           'ira'=anger,
           'disgusto'=disgust,
           'miedo'=fear,
           'alegría'=joy,
           'tristeza'=sadness,
           'sorpresa'=surprise,
           'confianza'=trust,
           'negativa'=negative,
           'positiva'=positive)
  
  base_emocion <- data.frame(t(base_emocion))
  base_emocion <- data.frame(rowSums(base_emocion))
  names(base_emocion)[1] <- "cuenta"
  
  base_emocion <- cbind('sentimiento'=rownames(base_emocion), base_emocion)
  rownames(base_emocion) <- NULL
  
  return(base_emocion)
  
}

clean.text = function(x)
{
  # tolower
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}

tw_hashtag <- function(base){
  
  base %>% 
    unnest_tokens(Palabra, text, token = 'tweets') %>% 
    count(Palabra, sort = TRUE) %>% 
    filter(str_detect(Palabra, "^#"))  %>%
    ungroup() %>%
    arrange(desc(n)) %>% 
    mutate(word=Palabra,
           freq=n) %>% 
    select(word, freq)
  
}

tw_user <- function(base){
  
  base %>% 
    unnest_tokens(Palabra, text, token = 'tweets') %>% 
    count(Palabra, sort = TRUE) %>% 
    filter(str_detect(Palabra, "^@"))  %>%
    ungroup() %>%
    arrange(desc(n)) %>% 
    mutate(word=Palabra,
           freq=n) %>% 
    select(word, freq)
  
}
