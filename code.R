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
  #distinct(segment) %>%   pull(segment) %>% sort()
  filter( lang != "Mehweb") %>% 
  write_csv("KiKo_db.csv", na = "")

# create_id ---------------------------------------------------------------
df <- read_csv("database.csv")

library(lingtypology)
df %>% 
  distinct(lang) %>% 
  mutate(lang2 = lang,
         lang2 = str_replace(lang2, "Inkhoqwari", "Inxokvari"),
         lang2 = str_replace(lang2, "Khwarshi", "Khwarshi-Inkhoqwari"),
         lang2 = str_replace(lang2, "Khinalugh", "Khinalug")) %>% 
  mutate(aff = aff.lang(lang2)) %>% 
  select(-lang2) %>% 
  full_join(df) %>% 
  arrange(aff, language, source, segment_type, segment) %>% 
  mutate(id = 1:n()) %>% 
  select(id, glottocode, lang, language, idiom, type, segment, segment_source, segment_type, source, page, comment, contributer, date) %>% 
  write_csv("database.csv", na = "")

# add features ------------------------------------------------------------
df <- read_csv("database.csv")

df %>% 
  mutate(segment = str_replace_all(segment, "'", "ʼ"),
         segment = str_replace_all(segment, "’", "ʼ"),
         segment = str_replace_all(segment, ":", "ː"),
         segment = str_replace_all(segment, "ːⁿ", "ⁿː"),
         segment = str_replace_all(segment, "ːʲ", "ʲː"),
         segment = str_replace_all(segment, "ʷː", "ːʷ"),
         segment = str_replace_all(segment, "ʼʲ", "ʲʼ"),
         segment = str_replace_all(segment, "g", "ɡ"),
         segment = str_replace_all(segment, "c", "kʲ"),
         segment = str_replace_all(segment, "ɟ", "ɡʲ"),
         segment = str_replace_all(segment, "ç", "xʲ")) %>% 
  #distinct(segment) %>% pull(segment) %>% sort()
  write_csv("database.csv", na = "")

# create tables -----------------------------------------------------------

# geminates
df <- read_csv("database.csv")
df %>% 
  filter(segment_type == "consonant") %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(geminates = ifelse(str_detect(segment, "ː"),
                               "attested",
                               "not attested")) %>% 
  count(language, idiom, type, source, page, contributer, date, geminates) %>% 
  pivot_wider(names_from = geminates, values_from = n, values_fill = 0) %>% 
  mutate(geminates = ifelse(attested > 0, "attested", "not attested"),
         map = "") %>% 
  select(-`not attested`, -attested) %>% 
  select(language, idiom, type, map, geminates, source, page, contributer, date) %>% 
  mutate(map = ifelse(source == "kibrikkodzasov1990" & language == "Khinalug", "no", map),
         map = ifelse(source == "talibov2007" & language == "Budukh", "no", map)) %>% 
  write_csv("for_dagatlas/geminates.csv", na = "")

# labialization
df %>% 
  filter(segment_type == "consonant") %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(labialization = ifelse(str_detect(segment, "ʷ"),
                            "attested",
                            "not attested")) %>% 
  count(language, idiom, type, source, page, contributer, date, labialization) %>% 
  pivot_wider(names_from = labialization, values_from = n, values_fill = 0) %>% 
  mutate(labialization = ifelse(attested > 0, "attested", "not attested"),
         map = "") %>% 
  select(-`not attested`, -attested) %>% 
  select(language, idiom, type, map, labialization, source, page, contributer, date) %>% 
  mutate(map = ifelse(source == "kibrikkodzasov1990" & language == "Khinalug", "no", map),
         map = ifelse(source == "talibov2007" & language == "Budukh", "no", map)) %>% 
  write_csv("for_dagatlas/labialization.csv", na = "")

# labials
df %>% 
  filter(str_detect(segment, "[wvβ]")) %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", "),
         labials = str_c(sort(unique(segment)), collapse = ", "),
         map = "") %>% 
  distinct(language, idiom, type, map, labials, source, page, contributer, date) %>% 
  select(language, idiom, type, map, labials, source, page, contributer, date) %>%
  mutate(map = ifelse(source == "kibrikkodzasov1990" & language == "Khinalug", "no", map),
         map = ifelse(source == "talibov2007" & language == "Budukh", "no", map)) %>% 
  write_csv("for_dagatlas/labials.csv", na = "")

# laterals  
df %>% 
  filter(segment_type == "consonant") %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(laterals = ifelse(str_detect(segment, "ɬ"),
                            "attested",
                            "not attested")) %>% 
  count(language, idiom, type, source, page, contributer, date, laterals) %>% 
  pivot_wider(names_from = laterals, values_from = n, values_fill = 0) %>% 
  mutate(laterals = ifelse(attested > 0, "attested", "not attested"),
         map = "") %>% 
  select(-`not attested`, -attested) %>% 
  select(language, idiom, type, map, laterals, source, page, contributer, date) %>% 
  mutate(map = ifelse(source == "kibrikkodzasov1990" & language == "Khinalug", "no", map),
         map = ifelse(source == "talibov2007" & language == "Budukh", "no", map)) %>% 
  write_csv("for_dagatlas/laterals.csv", na = "")

# long vowels
df %>% 
  filter(segment_type == "vowel") %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(long_vowels = ifelse(str_detect(segment, "ː"),
                            "attested",
                            "not attested")) %>% 
  count(language, idiom, type, source, page, contributer, date, long_vowels) %>% 
  pivot_wider(names_from = long_vowels, values_from = n, values_fill = 0) %>% 
  mutate(long_vowels = ifelse(attested > 0, "attested", "not attested"),
         map = "") %>% 
  select(-`not attested`, -attested) %>% 
  select(language, idiom, type, map, long_vowels, source, page, contributer, date) %>% 
  mutate(map = ifelse(source == "kibrikkodzasov1990" & language == "Khinalug", "no", map),
         map = ifelse(source == "talibov2007" & language == "Budukh", "no", map)) %>% 
  write_csv("for_dagatlas/long_vowels.csv", na = "")

# nasalization
df %>% 
  filter(segment_type == "vowel") %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(nasalization = ifelse(str_detect(segment, "ⁿ"),
                              "attested",
                              "not attested")) %>% 
  count(language, idiom, type, source, page, contributer, date, nasalization) %>% 
  pivot_wider(names_from = nasalization, values_from = n, values_fill = 0) %>% 
  mutate(nasalization = ifelse(attested > 0, "attested", "not attested"),
         map = "") %>% 
  select(-`not attested`, -attested) %>% 
  select(language, idiom, type, map, nasalization, source, page, contributer, date) %>% 
  mutate(map = ifelse(source == "kibrikkodzasov1990" & language == "Khinalug", "no", map),
         map = ifelse(source == "talibov2007" & language == "Budukh", "no", map)) %>% 
  write_csv("for_dagatlas/nasalization.csv", na = "")

# pharyngealization
df %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(pharyngealization = ifelse(str_detect(segment, "ˤ"),
                               "attested",
                               "not attested")) %>% 
  count(language, idiom, type, source, page, contributer, date, pharyngealization) %>% 
  pivot_wider(names_from = pharyngealization, values_from = n, values_fill = 0) %>% 
  mutate(pharyngealization = ifelse(attested > 0, "attested", "not attested"),
         map = "") %>% 
  select(-`not attested`, -attested) %>% 
  select(language, idiom, type, map, pharyngealization, source, page, contributer, date) %>% 
  mutate(map = ifelse(source == "kibrikkodzasov1990" & language == "Khinalug", "no", map),
         map = ifelse(source == "talibov2007" & language == "Budukh", "no", map)) %>% 
  write_csv("for_dagatlas/pharyngealization.csv", na = "")


# umlaut vowels
df %>% 
  filter(segment_type == "vowel") %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(umlaut_vowels = ifelse(str_detect(segment, "[yøæ]"),
                                    "attested",
                                    "not attested")) %>% 
  count(language, idiom, type, source, page, contributer, date, umlaut_vowels) %>% 
  pivot_wider(names_from = umlaut_vowels, values_from = n, values_fill = 0) %>% 
  mutate(umlaut_vowels = ifelse(attested > 0, "attested", "not attested"),
         map = "") %>% 
  select(-`not attested`, -attested) %>% 
  select(language, idiom, type, map, umlaut_vowels, source, page, contributer, date) %>% 
  mutate(map = ifelse(source == "kibrikkodzasov1990" & language == "Khinalug", "no", map),
         map = ifelse(source == "talibov2007" & language == "Budukh", "no", map)) %>% 
  write_csv("for_dagatlas/umlaut_vowels.csv", na = "")

# palatalized consonants
df %>% 
  filter(segment_type == "consonant") %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(palatalized_consonants = ifelse(str_detect(segment, "ʲ"),
                                "attested",
                                "not attested")) %>% 
  count(language, idiom, type, source, page, contributer, date, palatalized_consonants) %>% 
  pivot_wider(names_from = palatalized_consonants, values_from = n, values_fill = 0) %>% 
  mutate(palatalized_consonants = ifelse(attested > 0, "attested", "not attested"),
         map = "") %>% 
  select(-`not attested`, -attested) %>% 
  select(language, idiom, type, map, palatalized_consonants, source, page, contributer, date) %>% 
  mutate(map = ifelse(source == "kibrikkodzasov1990" & language == "Khinalug", "no", map),
         map = ifelse(source == "talibov2007" & language == "Budukh", "no", map)) %>% 
  write_csv("for_dagatlas/palatalized_consonants.csv", na = "")

# aspirated
df %>% 
  filter(segment_type == "consonant") %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(aspirated = ifelse(str_detect(segment, "ʰ"),
                                         "attested",
                                         "not attested")) %>% 
  count(language, idiom, type, source, page, contributer, date, aspirated) %>% 
  pivot_wider(names_from = aspirated, values_from = n, values_fill = 0) %>% 
  mutate(aspirated = ifelse(attested > 0, "attested", "not attested"),
         map = "") %>% 
  select(-`not attested`, -attested) %>% 
  select(language, idiom, type, map, aspirated, source, page, contributer, date) %>% 
  mutate(map = ifelse(source == "kibrikkodzasov1990" & language == "Khinalug", "no", map),
         map = ifelse(source == "talibov2007" & language == "Budukh", "no", map)) %>% 
  write_csv("for_dagatlas/aspirated.csv", na = "")

# pharyngeals
df %>% 
  filter(segment_type == "consonant") %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", "),
         pharyngeals = case_when(
           str_detect(segment, "ʕ")~"ʕ",
           str_detect(segment, "ħ")~"ħ",
           str_detect(segment, "ʜ")~"ʜ",
           str_detect(segment, "ʡ")~"ʡ",
           str_detect(segment, "ʢ")~"ʢ",
           TRUE ~ "none"),
         pharyngeals = str_c(unique(pharyngeals), collapse = ", "),
         pharyngeals = str_remove_all(pharyngeals, "none, "),
         map = "") %>% 
  distinct(language, idiom, type, map, pharyngeals, source, page, contributer, date) %>% 
  select(language, idiom, type, map, pharyngeals, source, page, contributer, date) %>% 
  mutate(map = ifelse(source == "kibrikkodzasov1990" & language == "Khinalug", "no", map),
         map = ifelse(source == "talibov2007" & language == "Budukh", "no", map)) %>%  
  write_csv("for_dagatlas/pharyngeals.csv", na = "")

# velar voiced fricatives
df %>% 
  filter(segment_type == "consonant") %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(velar_voiced_fricatives = ifelse(str_detect(segment, "ɣ"),
                            "attested",
                            "not attested")) %>% 
  count(language, idiom, type, source, page, contributer, date, velar_voiced_fricatives) %>% 
  pivot_wider(names_from = velar_voiced_fricatives, values_from = n, values_fill = 0) %>% 
  mutate(velar_voiced_fricatives = ifelse(attested > 0, "attested", "not attested"),
         map = "") %>% 
  select(-`not attested`, -attested) %>% 
  select(language, idiom, type, map, velar_voiced_fricatives, source, page, contributer, date) %>% 
  mutate(map = ifelse(source == "kibrikkodzasov1990" & language == "Khinalug", "no", map),
         map = ifelse(source == "talibov2007" & language == "Budukh", "no", map)) %>% 
  write_csv("for_dagatlas/velar_voiced_fricatives.csv", na = "")

df %>% 
  distinct(segment) %>%   pull(segment) %>% sort()
  