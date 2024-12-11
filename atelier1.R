library(jsonlite)
library(mongolite)
library(rvest)
library(tidyverse)

url='https://scrapeme.live/product-category/pokemon/page/2/'
response=read_html(url)


# WEB SCRAPING

table1=tibble(
  'nom'=NA,
  'price'=NA,
  'link'=NA)


for(k in 1:5){
  print(k)
  url=paste0('https://scrapeme.live/product-category/pokemon/page/',k,'/')
  
  response=read_html(url)
  
  s_name = 'h2.woocommerce-loop-product__title'
  s_price= 'span.woocommerce-Price-amount'
  s_link = 'a.woocommerce-LoopProduct-link'
  pokename=response %>% 
    html_nodes(s_name) %>% 
    html_text()
  pokeprice=response %>% 
    html_nodes(s_price) %>% 
    html_text()
  pokelink=response %>% 
    html_nodes(s_link) %>% 
    html_attr('href')
  
  local = tibble(
    'nom'=pokename,
    'price'=pokeprice,
    'link'=pokelink)
  local
  table1=rbind(table1,local)

}

table1=table1[-1,]
table1$wght=NA
table1

for(k in 1:nrow(table1)){
  print(k)
  url = table1$link[k]
  response=read_html(url) 
  local=response%>% 
    html_nodes('td.product_weight') %>% 
    html_text()
  table1$wght[k]=local
}

table1=table1 %>% 
  mutate(price=str_replace_all(price,pattern="£",replacement='') %>% as.numeric(),
         wght =str_replace_all(wght,pattern=' kg',replacement='') %>% as.numeric(),
         nom=tolower(nom))

table1  
  
# API 

obj=fromJSON('https://pokeapi.co/api/v2/pokemon/')
nbr=obj['count'][[1]]

fullpoke=fromJSON(paste0('https://pokeapi.co/api/v2/pokemon/?offset=20&limit=',nbr))
table2=fullpoke['results'] %>% as.data.frame()
table2 %>% head()

table1=table1 %>% 
  inner_join(table2,by=c('nom'='results.name'))
table1

abilities=tibble('name'=NA,'abilities'=NA,'hp_n'=NA,'atk'=NA,'def'=NA,'spd'=NA,'type'=NA)
table1=table1 %>% mutate(nom=tolower(nom))

for(k in 1:nrow(table1)){
  print(k)
  pokename=table1[k,'nom']

  json=table2[k,'results.url']
  
  ab=fromJSON(json)['abilities'][[1]]
  ab2=ab$ability$name
  ab2
  x=fromJSON(json)['stats'][[1]] %>% select(base_stat) %>% pull(1) # nb hp 
  hp=x[1]
  atk=x[2]
  def=x[3]
  spd=x[6]
  hp;atk;def;spd
  y=fromJSON(json)['types'][[1]] %>% pull(type) %>% pull(name)
  y
  local=tibble('name'=pokename,'type'=list(y),'abilities'=list(ab2),'hp_n'=hp,'atk'=atk,'def'=def,'spd'=spd)
  abilities=rbind(abilities,local)
}
abilities

abilities=abilities[-1,]

table1 %>% str()
abilities %>% str()

abilities
table1
# Tidyverse

table1 %>% 
  select(nom,price,wght) %>% 
  mutate(nom=tolower(nom)) %>% 
  left_join(abilities,by=c('nom'='name'))

aaa
# MONGO DB



m=mongo('pokemon')
m$count()

if(m$count()>0) {m$drop()}

m$insert(aaa)

m$find()
