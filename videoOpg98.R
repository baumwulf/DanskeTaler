library(tidyverse)
library(Sentida)

alleT=readRDS("alletaler.rds")
allPM=readRDS("pm.rds")

alleT = alleT %>% rowwise() %>% mutate(year=str_extract(title,"[0-9]{4}$"))
alleTSub = alleT %>% filter(str_detect(title,"aabningstale-[0-9]{4}"))
alleT = alleT %>% mutate(folketing=str_detect(title,"aabningstale-[0-9]{4}"))
alleT = alleT %>% unique()

alleTFolk = alleT %>% filter(folketing==T)
alleTFolk = alleTFolk %>% rowwise() %>% mutate(sscore=sentida(content, output="mean"))
alleTFolk$idx=1:nrow(alleTFolk)

hist(alleTFolk$sscore, breaks = 12)
alleTFolk$year = as.numeric(alleTFolk$year)
alleTFolk = alleTFolk %>% filter(!is.na(year))

table(alleTFolk$year)

alleTFolk2 = left_join(alleTFolk,allPM,by="year")


alleTFolk2Sub=alleTFolk2 %>% filter(year %in% c(1986,1996,2009,2011))

# now tidy
library(tidytext)
alleTidy=alleTFolk2Sub %>% unnest_tokens(word,content) %>% count(word)
alleTidyT=alleTFolk2Sub %>% group_by(idx) %>% unnest_tokens(word,content) %>% count(word)
alleTidyT=alleTFolk2Sub %>% group_by(idx) %>% unnest_tokens(word,content) %>% count(word)
alleTidyTFIDF=alleTidyT %>% bind_tf_idf(word,idx,n)

# sentiment
sfile=read_csv("2_headword_headword_polarity.csv",col_names = F)
colnames(sfile)=c("word","x2","pos","x4","score","stemm")

alleTidySc=inner_join(alleTidyTFIDF,sfile,by="word")
alleTidySc2=alleTidySc %>% group_by(idx) %>% mutate(slscore=n*score) %>% summarise(totscore=sum(slscore))



# LDA
library(tm)
library(topicmodels)
library(stm)
# clean before cast
alleTidyTcl=alleTidyT %>% filter(str_detect(word,"\\D"))
stdk=stopwords("da")
stopdk=c(stdk,"må","nye","får","kan","så","ved","vores")
alleTidyTcl=alleTidyTcl %>% filter(!word %in% stopdk)
lleTidyTcl=alleTidyTcl %>% filter(str_detect(word,"\\D"))
alleTidyTcl=alleTidyTcl %>% filter(!str_detect(word,"[0-9,\\.]+"))
dtm=cast_dtm(alleTidyTcl,idx,word,n)
# now LDA
top_m2=LDA(dtm,k=4)
top_m <- LDA( dtm, k = 4, method = "Gibbs", 
            control = control)
            control = list(alpha=0.1, delta= 2.01, burnin = 500, thin = 100, iter = 4000) )

# back to tidy
top_gamma=tidy(top_m,matrix="gamma")
top_beta=tidy(top_m,matrix="beta")
top_beta
top_gamma

top_top_beta = top_beta %>% group_by(topic) %>% top_n(5,beta)
ggplot(top_gamma,aes(x=reorder(topic,gamma),y=gamma, fill=as.factor(document)))+geom_bar(stat = "identity",position = "dodge")
ggplot(top_top_beta,aes(x=reorder(term,beta),y=beta, fill=as.factor(topic)))+geom_bar(stat = "identity",position = "dodge")+
  coord_flip()+facet_wrap(~topic, scales = "free")

top_gamma2 = left_join(top_gamma,top_top_beta,by="topic")
top_gamma2 = top_gamma2 %>% mutate(lb=as.integer(log2(400*beta)))


ggplot(top_gamma2,aes(x=document,y=topic, fill=gamma))+geom_tile(show.legend = F)+
  scale_fill_gradient2(low="red",high = "blue", midpoint = 0.5)+
  geom_text(aes(label=term,size=lb),show.legend=F,position = position_jitter(width=0.3,height = 0.3))


#
library(wordcloud2)
top_top_beta=top_top_beta %>% ungroup()
top_top_beta %>% filter(topic==3) %>% select(term,beta) %>% rename(word=term,freq=beta) %>% 
  wordcloud2()

