library(dplyr)
library(stringr)
# read a file with text data
text <- read.csv('text_data/eniki.csv')

head(text)

# compute number of syllables and word lengh in phonemes
text <- text %>%
  mutate(syl_number = str_count(syllables, '-') + 1, 
         word_len = nchar(phonetic))
  
# Menserath-Altmann law
text %>%
  group_by(syl_number) %>%
  summarise(avg_len =  mean(word_len / syl_number)) -> ASL # compute avarage syllable length

syl_number  = ASL$syl_number
avg_len_phon = ASL$avg_len

fit <-nls(avg_len ~ a * syl_number**b * exp(-c*syl_number), data = ASL,
          start = list(a = 3, b = 1, c = 3), control = list(maxiter = 500),    algorithm = "port")
summary(fit) -> sum_fit
sum_fit

# Calculate the R-squared value - a measure of fit
RSS_p  <- sum(residuals(fit)^2) # Residual sum of squares

TSS <- sum((avg_len_phon - mean(avg_len_phon))^2) # Total sum of squares
R_squ = 1 - (RSS_p/TSS) # R-squared measure

R_squ 

fitted(fit) # get fitted values

# count average syllable length depending on word length and its position in a word.
text %>%
  mutate(syl_split = str_split(syllables, pattern = '-')) %>%
  mutate(syl_number = str_count(syllables, '-') + 1) -> text

text$syl_seq <- sapply(text$syl_number, seq_len)

text %>%
  select(phonetic, syl_number, syl_split, syl_seq) %>%
  tidyr::unnest(cols = c(syl_split, syl_seq)) -> text_unnested

text_unnested %>%
  mutate(length_of_syl = str_length(syl_split)) %>%
  group_by(syl_number, syl_seq) %>%
  summarise(mean_syl_len = mean(length_of_syl)) %>%
  tidyr::spread(syl_seq , value = mean_syl_len, fill = 0)


