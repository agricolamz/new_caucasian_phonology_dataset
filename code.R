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
  distinct(language, idiom, type, source) %>% 
  mutate(map = "yes",
         genlang_point = "yes",
         map = ifelse(source == "kibrikkodzasov1990" & language == "Khinalug", 
                      "no", map),
         map = ifelse(source == "talibov2007" & language == "Budukh", 
                      "no", map),
         genlang_point = ifelse(source == "kibrikkodzasov1990" & language %in% c("Agul", "Akhvakh", "Andi", "Archi", "Avar", "Bezhta", "Hinuq", "Hunzib", "Khinalug", "Kryz", "Lezgian", "Rutul", "Tabasaran", "Tindi", "Tsakhur", "Tsez"), 
                                "no", genlang_point),
         genlang_point = ifelse(source == "talibov2007" & language == "Budukh", 
                                "no", genlang_point),
         genlang_point = ifelse(source == "bokarev1949a" & language == "Chamalal", 
                                "no", genlang_point),
         genlang_point = ifelse(idiom %in% c("Chirag", "Kaitag", "Tanty"), "no", genlang_point)) ->
  tomerge
  
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
  mutate(value1 = ifelse(attested > 0, "attested", "not attested"),
         feature = "Presence of geminates",
         value1_name = "Presence of geminates") %>% 
  left_join(tomerge) %>% 
  mutate(genlang_point = ifelse(language == "Tokita", "no", genlang_point),
         language = ifelse(language == "Tokita", "Karata", language)) %>% 
  arrange(language) %>% 
  select(language, idiom, type, genlang_point, map, feature, value1_name, value1, source, page, contributer, date) %>% 
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
  mutate(value1 = ifelse(attested > 0, "attested", "not attested"),
         feature = "Presence of labialized consonants",
         value1_name = "Presence of labialized consonants") %>% 
  left_join(tomerge) %>% 
  mutate(genlang_point = ifelse(language == "Tokita", "no", genlang_point),
         language = ifelse(language == "Tokita", "Karata", language)) %>% 
  arrange(language) %>% 
  select(language, idiom, type, genlang_point, map, feature, value1_name, value1, source, page, contributer, date) %>% 
  write_csv("for_dagatlas/labialization.csv", na = "")

# labials
df %>% 
  filter(str_detect(segment, "[wvβ]")) %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", "),
         value1 = str_c(sort(unique(segment)), collapse = ", "),
         feature = "Inventory of v-like consonants",
         value1_name = "Inventory of v-like consonants") %>% 
  distinct(language, idiom, type, feature, value1, value1_name, source, page, contributer, date) %>% 
  left_join(tomerge) %>% 
  select(language, idiom, type, genlang_point, map, feature, value1, value1_name, source, page, contributer, date) %>% 
  mutate(genlang_point = ifelse(language == "Tokita", "no", genlang_point),
         language = ifelse(language == "Tokita", "Karata", language)) %>% 
  arrange(language) %>% 
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
  mutate(value1 = ifelse(attested > 0, "attested", "not attested"),
         feature = "Presence of laterals",
         value1_name = "Presence of laterals") %>% 
  left_join(tomerge) %>% 
  mutate(genlang_point = ifelse(language == "Tokita", "no", genlang_point),
         language = ifelse(language == "Tokita", "Karata", language)) %>% 
  select(language, idiom, type, genlang_point, map, feature, value1_name, value1, source, page, contributer, date) %>% 
  arrange(language) %>% 
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
  mutate(value1 = ifelse(attested > 0, "attested", "not attested"),
         feature = "Presence of long vowels",
         value1_name = "Presence of long vowels") %>% 
  left_join(tomerge) %>% 
  mutate(genlang_point = ifelse(language == "Tokita", "no", genlang_point),
         language = ifelse(language == "Tokita", "Karata", language)) %>% 
  select(language, idiom, type, genlang_point, map, feature, value1_name, value1, source, page, contributer, date) %>% 
  arrange(language) %>% 
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
  mutate(value1 = ifelse(attested > 0, "attested", "not attested"),
         feature = "Presence of nasal vowels",
         value1_name = "Presence of nasal vowels") %>% 
  left_join(tomerge) %>% 
  mutate(genlang_point = ifelse(language == "Tokita", "no", genlang_point),
         language = ifelse(language == "Tokita", "Karata", language)) %>% 
  arrange(language) %>% 
  select(language, idiom, type, genlang_point, map, feature, value1_name, value1, source, page, contributer, date) %>% 
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
  mutate(value1 = ifelse(attested > 0, "attested", "not attested"),
         feature = "Presence of pharyngealized segments",
         value1_name = "Presence of pharyngealized segments") %>% 
  left_join(tomerge) %>% 
  mutate(genlang_point = ifelse(language == "Tokita", "no", genlang_point),
         language = ifelse(language == "Tokita", "Karata", language)) %>% 
  select(language, idiom, type, genlang_point, map, feature, value1_name, value1, source, page, contributer, date) %>% 
  arrange(language) %>% 
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
  mutate(value1 = ifelse(attested > 0, "attested", "not attested"),
         feature = "Presence of umlaut vowels",
         value1_name = "Presence of umlaut vowels") %>% 
  left_join(tomerge) %>% 
  mutate(genlang_point = ifelse(language == "Tokita", "no", genlang_point),
         language = ifelse(language == "Tokita", "Karata", language)) %>% 
  select(language, idiom, type, genlang_point, map, feature, value1_name, value1, source, page, contributer, date) %>% 
  arrange(language) %>% 
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
  mutate(value1 = ifelse(attested > 0, "attested", "not attested"),
         feature = "Presence of palatalized consonants",
         value1_name = "Presence of palatalized consonants") %>% 
  left_join(tomerge) %>% 
  mutate(genlang_point = ifelse(language == "Tokita", "no", genlang_point),
         language = ifelse(language == "Tokita", "Karata", language)) %>% 
  select(language, idiom, type, genlang_point, map, feature, value1_name, value1, source, page, contributer, date) %>% 
  arrange(language) %>% 
  write_csv("for_dagatlas/palatalized_consonants.csv", na = "")

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
         pharyngeals = str_remove_all(pharyngeals, "none, ")) %>% 
  distinct(language, idiom, type, pharyngeals, source, page, contributer, date) %>% 
  mutate(value1 = pharyngeals,
         feature = "Pharyngeal inventory",
         value1_name = "Pharyngeal inventory") %>% 
  left_join(tomerge) %>% 
  mutate(genlang_point = ifelse(language == "Tokita", "no", genlang_point),
         language = ifelse(language == "Tokita", "Karata", language)) %>% 
  select(language, idiom, type, genlang_point, map, feature, value1_name, value1, source, page, contributer, date) %>% 
  arrange(language) %>% 
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
  mutate(value1 = ifelse(attested > 0, "attested", "not attested"),
         feature = "Presence of velar voiced fricatives",
         value1_name = "Presence of velar voiced fricatives") %>% 
  left_join(tomerge) %>% 
  mutate(genlang_point = ifelse(language == "Tokita", "no", genlang_point),
         language = ifelse(language == "Tokita", "Karata", language)) %>% 
  select(language, idiom, type, genlang_point, map, feature, value1_name, value1, source, page, contributer, date) %>% 
  arrange(language) %>% 
  write_csv("for_dagatlas/velar_voiced_fricatives.csv", na = "")

# uvular voiced fricatives
df %>% 
  filter(segment_type == "consonant") %>% 
  group_by(language, idiom, type, source, contributer) %>% 
  mutate(page = str_c(unique(page), collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(velar_voiced_fricatives = ifelse(str_detect(segment, "ɢ"),
                                          "attested",
                                          "not attested")) %>% 
  count(language, idiom, type, source, page, contributer, date, velar_voiced_fricatives) %>% 
  pivot_wider(names_from = velar_voiced_fricatives, values_from = n, values_fill = 0) %>% 
  mutate(value1 = ifelse(attested > 0, "attested", "not attested"),
         feature = "Presence of uvular voiced fricatives",
         value1_name = "Presence of uvular voiced fricatives") %>% 
  left_join(tomerge) %>% 
  mutate(genlang_point = ifelse(language == "Tokita", "no", genlang_point),
         language = ifelse(language == "Tokita", "Karata", language)) %>% 
  select(language, idiom, type, genlang_point, map, feature, value1_name, value1, source, page, contributer, date) %>% 
  arrange(language) %>% 
  write_csv("for_dagatlas/uvular_voiced_fricatives.csv", na = "")

# initiation/phonation
df %>% 
  filter(segment_type == "consonant",
         str_detect(segment, "k"), 
         !str_detect(segment, "ʷ"),
         !str_detect(segment, "ˤ"),
         !str_detect(segment, "ʲ")) %>% 
  group_by(language, idiom, type, source, contributer, date) %>% 
  mutate(page2 = str_c(unique(page), collapse = ", ")) %>% 
  ungroup() %>% 
  distinct(language, idiom, segment, source, contributer, page, date) %>% 
  group_by(language, idiom, source, contributer, date) %>% 
  arrange(language, idiom, source, contributer, segment) %>% 
  mutate(feature = "Number of initiation/phonation contrasts in voiceless consonants",
         value1_name = "Type of the contrast",
         value1 = n(),
         value1 = case_when(value1 == 1 ~ "one-way",
                            value1 == 2 ~ "two-way",
                            value1 == 3 ~ "three-way",
                            value1 == 4 ~ "four-way"),
         segment = str_replace(segment, "k", "C"),
         value2_name = "Contents of the contrast",
         value2 = str_c(segment, collapse = "-")) %>% 
  select(-segment) %>% 
  distinct() %>% 
  left_join(tomerge) %>% 
  mutate(genlang_point = ifelse(language == "Tokita", "no", genlang_point),
         language = ifelse(language == "Tokita", "Karata", language)) %>% 
  select(language, idiom, type, genlang_point, map, feature, value1_name, value1, value2_name, value2, source, page, contributer, date) %>% 
  arrange(language) %>% 
  write_csv("for_dagatlas/init_phon_contrasts.csv", na = "")

# all_features_merger -----------------------------------------------------

df <- map_dfr(str_c("for_dagatlas/", list.files("for_dagatlas/")), read_csv)
