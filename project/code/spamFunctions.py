import re, os

##Start of help functions for the SMSSpamCollection dataset
def isSpamSMS(textLine):
	"""SMS spam dataset spam classifier. Returns true if a word is spam, false otherwise."""
	return re.sub("[\^w]", " ", textLine).split()[0].lower() == "spam"
	
def preprocessingSMS(textLine):
	"""Removes the first spam/ham of the textline."""
	return textLine.split(None, 1)[1]
	
def getDefultSMSpath():
	"""Returns the filepath to the SMSSpamCollection assuming we have the default folder structure."""
	return getDataPath() + "SMSSpamCollection"
	
##End of help functions for the SMSSpamCollection dataset

##Dummy functions
def isSpam(textLine):
	"""Classes everything as spam"""
	return True
	
def preprocessing(textLine):
	"""Does nothing"""
	return textLine
	
def getDataPath():
	"""Returns ../data/ with os dependent separators."""
	return path_ + os.sep + ".." + os.sep + "data" + os.sep
	
##Option selection
def getPreprocessingText():
	return "0 - Dummy Preprocessing,\n 1 - SMSSpamCollection preprocessing."
	
def getIsSpamText():
	return "0 - Dummy Spam Classifier,\n 1 - SMSSpamCollection spam classifier"
	
def getIsSpam(i, textLine):
	if(i == 0):
		isSpam(textLine)
	elif(i == 1):
		isSpamSMS(textLine)
		
def getPreprocessing(i, textLine):
	if(i == 0):
		preprocessing(textLine)
	elif(i == 1):
		preprocessingSMS