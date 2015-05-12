from nltk.stem.wordnet import WordNetLemmatizer
import re
import os

path_ = os.getcwd()
#name of the file that contains the data.
fileName = "SMSSpamCollection"
#This is the path to the file that contains the original data.
filePath = path_ + os.sep + ".." + os.sep + "data" + os.sep + fileName
lemmatizer = WordNetLemmatizer()

# Returns all words from a textLine without any special characters
def getWordsFromString(textLine):
	wordList = re.sub('[^a-zA-Z+]', " ", textLine).split()
	return wordList
	
# Takes a line of Text and applies the Lemmatizer on each word in it. Returns a dictionary with original words mapped to the lemmatized words. Only returns lower case characters.
def getLemmatizedTextKeyset(textLine):
	listOfWords = getWordsFromString(textLine)
	dictionaryMap = {}
	i = 0
	for word in listOfWords:
		dictionaryMap[word] = lemmatizer.lemmatize(word.lower())
		if word.lower() == dictionaryMap[word]:
			dictionaryMap[word] = lemmatizer.lemmatize(word.lower(), 'v')
		i += 1
	return dictionaryMap

def buildDictionaryFromTextBlock(text):
	lines = text.splitlines()
	dic =  {}
	for line in lines:
		dic.update(getLemmatizedTextKeyset(line))
	return dic

def getDictionary_(textFile):
	text=""
	for line in textFile:
		text += textFile.readline()
	return buildDictionaryFromTextBlock(text)
	

#Returns a dictionary for the spamCollection with word:lemma as mapping.
def getDictionary():
	spamCollection = open(filePath, "r")
	temp = getDictionary_(spamCollection)
	spamCollection.close()
	return temp
	
def getLemmatizedText_(file):
	dic = getDictionary()
	lemmatizedText = ""
	for temp in file:
		line = file.readline()
		words = re.sub('[^a-zA-Z+]', " ", line).split()
		for word in words:
			line = line.replace(word, dic[word])
		#Can in theory throw an exception for not finding the key.
		lemmatizedText += line
	return lemmatizedText
	
#returns a version of the target file text with lemmatization appled to it as a string.
def getLemmatizedText():
	spamCollection = open(filePath, "r")
	spamCollection.seek(0)
	lemmatizedText = getLemmatizedText_(spamCollection)
	spamCollection.close()
	return lemmatizedText