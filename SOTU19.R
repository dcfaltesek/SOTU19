#trump state of the union 19
#step one import data - using the VOX transcript
#processed in google sheets as a CSV - i'll put that on github
#some of my code needs some refactoring, I reflexively produce reference vectors and use destructive methos
#some of this may be out of order, this is for the most part my actual code scratch.

#for my own sake, I am assigning the base dataset a name
SPEECH<-SOTU19...Sheet1

#Check it
View(SPEECH)
#rename 
colnames(SPEECH)[1]<-"Text"

#the following code owes much to Slige and Robinsion - READ THEM https://www.tidytextmining.com/sentiment.html
library(tidytext)
library(dplyr)
library(ggplot2)
#because sometimes I just have to use a T pipe.
library(magrittr)

#we need to make a little metadata first, such as paragraph number
#there are 62, 
N<-1:62

#add that sequence to the dataset
SPEECH<-mutate(SPEECH, N=N)

#we need a version of this dataset that includes just the words
tidySPEECH<-SPEECH%>%
  unnest_tokens(word, Text)

#looks good, I apprecieate that the row number is a word count, could be a handy accident, LETS SAVE THAT
View(tidySPEECH)
M<-1:4446
SPEECH_withWordCount<-mutate(tidySPEECH, M=M)%T>%
  View()

#for those of you playing along at home N = paragraph, M = word

#here is a controversial choice - I am pulling stop words
tidySPEECHclean<-tidySPEECH%>%
  anti_join(stop_words)

#quick data - these words make sense - we will also want to use that APPLUSE metadata later
tScA<-tidySPEECHclean%>%
  count(word, sort=TRUE)

#that mess I have assigned it is my normal sort of variable tag

#graphic 1: a quick word count - I am adapting Slige and Robinson here
tidySPEECHclean %>%
  #redoes that which I have already sored as tScA
  count(word, sort = TRUE) %>%
  #gets rid of the long tail of low usage words
  filter(n>5)%>%
  #remove the METADATA applause
  filter(word != "applause")%>%
  #moves stuff around slightly
  mutate(word = reorder(word, n)) %>%
  #runs ggplot piping in all that work with aesthetics "word" and "n" which is count, we are using "N" for paragraph number
  ggplot(aes(word, n)) +
  #makes a nice histogramish thing
  geom_col() +
  #labels it
  xlab(NULL) +
  #makes it vertical because that is cool.
  coord_flip()

#the frequency data here isn't going to be too revealing - trump uses a whole ton of words about 5 times

#for our purposes here we want 
View(SPEECH_withWordCount)
#M is word no, N is paragraph, word is the word

#why do it this way? all sentiment methods are inner_join, the are really pretty straight forward, first example
#get some general purpose SCORED sentiments
A<-get_sentiments("afinn")
#join those to our data
SP1<-inner_join(SPEECH_withWordCount, A, by = "word")
#get some DESCRIPTIVE terms
B<-get_sentiments("nrc")
#Join THOSE to our data
SP2<-inner_join(SP1, B, by = "word")
#and lets get rid of "applause"
SP3<-SP2%>%
  filter(word != "applause")
#have a look
View(SP3)

#GRAPHIC 2 - sentiment score as Y, COLOR as descriptor with a JITTER
ggplot(SP3, aes(M, score, colour=sentiment))+geom_jitter()+xlab("word number")
#this is why pschoanalysis continues today
ggplot(SP3, aes(M, score, colour=sentiment))+geom_density2d()+xlab("word number")

#LETS GO FURTHER AND FILTER FOR A FEW SPECIFIC EMOTIONS, on the POSITIVE SIDE TRUST and JOY; on the negative side, FEAR ANGER, DISGUST
SP3%>%
  filter(sentiment != "anticipation")%>%
  filter(sentiment != "negative")%>%
  filter(sentiment != "positive")%>%
  filter(sentiment != "sadness")%>%
  filter(sentiment != "surprise")%>%
  ggplot(aes(M, score, colour=sentiment))+geom_cols()

SP3%>%
  filter(sentiment != "anticipation")%>%
  filter(sentiment != "negative")%>%
  filter(sentiment != "positive")%>%
  filter(sentiment != "sadness")%>%
  filter(sentiment != "surprise")%>%
  ggplot(aes(M, score, colour=sentiment))+geom_jitter()

#a FACET MODEL - all that are not positive or negative
ggplot(SP3, aes(M, score, colour=sentiment))+geom_density_2d()+facet_grid(~sentiment)

#find the positive with a negative value
SP3%>%
  filter(sentiment=="positive")%>%
  filter(score<1)

#clustering starts to be sort of visible here, really INTENSE bursts of positive and negative.

#less aesthetic
SP3%>%
  filter(sentiment != "anticipation")%>%
  filter(sentiment != "sadness")%>%
  filter(sentiment != "surprise")%>%
  filter(sentiment != "disgust")%>%
  filter(sentiment != "fear")%>%
  filter(sentiment != "joy")%>%
  filter(sentiment != "trust")%>%
  filter(sentiment != "")%>%
  ggplot(aes(M, score))+geom_density2d()+facet_grid(~sentiment)


#LETS TRY THIS AGAIN using ALL of SP3 but now GROUPED by paragrpah number
SP4<-SP3%>%
  group_by(N)%>%
  summarize(mean(score))
View(SP4)
colnames(SP4)[2]<-"S"

#plot of general positive or negative - we can see paragraph 30 is the most negative
ggplot(SP4, aes(N, S))+geom_jitter()+xlab("Paragraph Number")+ylab("Score")
View(SPEECH)

#what if we want to know if there was applause in this paragraph and a few other things like the use of the word BORDER
library(stringr)
#counts the applause lines in a part of the speech
E<-str_count(SP2$word, "applause")
E<-data.frame(E)

#binds the applauses back into the data set
SP5<-bind_cols(SP2, E)

#now we take the next step and get the scores
U<-SP5%>%
  group_by(N)%>%
  summarize(mean(score))

#and the applauses
V<-SP5%>%
  group_by(N)%>%
  summarize(sum(E))

#a summary dataset with applause and sentiment
SP6<-data.frame(U,V)
ggplot(SP6, aes(N, mean.score.))+geom_jitter(aes(colour=sum.E.))+
  scale_colour_gradient(low = "white", high = "red")+
  ylab("Sentiment")+
  xlab("Paragraph Number")+
  ggtitle("Applause")

View(SPEECH)

#on the sentence level
SPSentences<-SPEECH%>%
  unnest_tokens(sentence, Text, token = "sentences")

#add a sentence count
View(SPSentences)
I<-1:255
I<-data.frame(I)
SPs2<-bind_cols(SPSentences, I)

#perhaps a redundant unnesting
SPs3<-SPs2%>%
  unnest_tokens(word, sentence)%>%
  inner_join(A, by = "word")

SPs5<-inner_join(SPs3, B, by = "word")

SPs5%>%
  group_by(I)%>%
  summarize(mean(score))


SPs4<-SPs3%>%
  group_by(I)%>%
  summarize(mean(score))

colnames(SPs4)[2]<-"score"

#sentence level plot
ggplot(SPs4, aes(I, score))+geom_jitter()

#restricted sentiment version of SP5
SPs5%>%
  filter(sentiment != "anticipation")%>%
  filter(sentiment != "sadness")%>%
  filter(sentiment != "surprise")%>%
  filter(sentiment != "disgust")%>%
  filter(sentiment != "fear")%>%
  filter(sentiment != "joy")%>%
  filter(sentiment != "positive")%>%
  filter(sentiment != "negative")%>%
  ggplot(aes(I, score))+geom_jitter(aes(colour=sentiment))

SPs5%>%
  ggplot(aes(I, score, colour=sentiment))+geom_jitter()

SPs5%>%
  ggplot(aes(I, score, colour=sentiment))+geom_smooth(model=lm)

#produce standard deviation on the sentence level
L<-SPs5%>%
  group_by(I)%>%
  summarize(sd(score))
colnames(L)[2]<-"deviation"

P<-SPs5%>%
  group_by(I)%>%
  summarize(mean(score))
colnames(P)[2]<-"balance" 

#join these as a dataframe
Q<-data.frame(L,P)

#Q now contains the feeling balance and the deviation
ggplot(Q, aes(I, deviation, colour=balance))+geom_jitter()
                                                         