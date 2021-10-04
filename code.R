# get data from Dagestanian db we collected with Viola --------------------
library(tidyverse)
df <- read_tsv("https://raw.githubusercontent.com/agricolamz/Daghestanian_Sound_Database/master/database.csv")
bib <- read_tsv("https://raw.githubusercontent.com/agricolamz/Daghestanian_Sound_Database/master/bibliography.csv")

bib %>% 
  select(language, contributer, idiom)  ->
  lang_cont
  
df %>% 
  distinct(language, segments_IPA, segments_source, source, comments, glottocode) %>% 
  left_join(lang_cont) %>% 
  rename(segment = segments_IPA, 
         lang = language,
         segment_source = segments_source,
         comment = comments) %>% 
  mutate(language = lang,
         type = "village",
         segment_type = ifelse(str_detect(segment, "[iouaeæəøyɑɨ]"), "vowel", "consonant"),
         page = str_extract(source, ": [\\d–]{1,}"),
         page = str_remove_all(page, ": "),
         date = "2017.05.06",
         source = "kibrikkodzasov1990") %>% 
  select(glottocode, lang, language, idiom, type, segment, segment_source, segment_type, source, page, comment, contributer, date) %>% 
  # distinct(segment) %>%   pull(segment) %>% sort()
  filter( lang != "Mehweb") %>% 
  write_csv("KiKo_db.csv", na = "")

# create_id ---------------------------------------------------------------
df <- read_csv("database.csv")

library(lingtypology)
df %>% 
  mutate(aff = aff.lang(lang))
  

df %>% 
  dis