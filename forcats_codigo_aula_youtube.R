#1. pacotes            ----
library(tidyverse)

#2. dados              ----
titanic_train <-  read_csv("train.csv") %>% janitor::clean_names() 

titanic_train %>% slice_sample(n=10)

#3. objetivos          ----
# Transformar variavel resposta (survived) para categorica com sim ou nao  
# Transformar classe, sexo, local de embarque para categorica
# Criar variaveis mulher e crian?a como categoricas
# Criar variavel conves categorica
# Criar variavel tamanho da familia
# Criar varival com titulo
# Reduzir as variaveis de titulos
# Alterar grupo de idades com faixa etaria

#4. mao na massa       ----

# Transformar var resposta para categorica  
titanic_train <-
  titanic_train %>% 
  mutate(survived = fct_recode(as.factor(survived), "nao"="0","sim"="1"))

# Transformar classe, sexo, local de embarque para categorica
titanic_train <-
  titanic_train %>% 
  mutate(across(c("pclass","sex","embarked"), as.factor))

# Criar variaveis mulher e crian?a como categoricas
titanic_train <- 
  titanic_train %>% 
  mutate(woman = as.factor(ifelse(sex == "female" & age >14, "sim","nao"))) %>% 
  mutate(child = as.factor(ifelse(age <=14, "sim","nao")))

# criar variavel convez extraindo a primeira letra e se for na transforma pra U
titanic_train <-
  titanic_train %>% 
  mutate(conves  = as.factor(ifelse(is.na(cabin),"U",str_extract(cabin, pattern = "^."))))

# criando uma feature com titulo do nome
titanic_train <- 
  titanic_train %>% 
  mutate(title = str_replace_all(name, pattern = "(.*, )|(\\..*)", replacement = ""))

# Criar variavel tamanho da familia
titanic_train <- 
  titanic_train %>% 
  mutate(family_size = 1 + sib_sp, parch)

# Reduzir as variaveis de titulos
table(titanic_train$title)

titanic_train %>% 
  head()

titanic_train <-
  titanic_train %>% 
  mutate(title = fct_collapse(title,
                              "Miss" = c("Miss","Ms","Lady"),
                              "Mr"  = c("Mr","Sir"),
                              "Mrs" = "Mrs")) %>% 
  mutate(other = fct_other(title,
                           keep = c("Miss","Mr","Mrs"),
                           other_level = "Outro"))

# Reduzir as variaveis de titulos com lump
titanic_train <-
  titanic_train %>% 
  mutate(title = fct_lump(f = title, n = 3, other_level = "outros"))

#Alterar grupo de idades com faixa etaria
titanic_train <-
  titanic_train %>% 
  mutate(age_class = case_when(age <13 ~ "child",
                               between(age, 13,18) ~ "teen",
                               between(age, 19,50)~ "adult",
                               age> 50 ~ "old"))

# Selecionaod dadoS categoricos
titanic_train %>% 
  filter(str_detect(name, "Mrs.")) %>% 
  summarise(Mr = n())
