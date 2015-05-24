from nltk.stem.wordnet import WordNetLemmatizer
import re, os, io, glob, argparse
import spamFunctions

lemmatizer = WordNetLemmatizer()

# Returns all words from a textLine without any special characters
def __getWordsFromString(textLine):
	wordList = re.sub('[^a-zA-Z+]', " ", textLine).split()
	return wordList

def __getLemmatizedTextKeyset(textLine):
	listOfWords = __getWordsFromString(textLine)
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
		dic.update(__getLemmatizedTextKeyset(line))
	return dic

def __getDictionary(textFile):
	textFile.seek(0)
	text=""
	textList=[]
	for line in textFile:
		text += line
	return buildDictionaryFromTextBlock(text)

def getLemmatizedText_(file):
	""" #returns a String version of the file with lemmatization applied to it. Does not work on empty files!"""
	file.seek(0)
	dic = __getDictionary(file)
	file.seek(0)
	lemmatizedText = ""
	for temp in file:
		line = temp
		words = re.sub('[^a-zA-Z+]', " ", line).split()
		for word in words:
			line = line.replace(word, dic[word])
		#Can in theory throw an exception for not finding the key.
		lemmatizedText += line
	return lemmatizedText

def createOutputSMS(outputPath):
	"""If the folders don't exist it creates the outputPath folder containing one folder named spam and one folder named ham."""
	if not os.path.exists(outputPath):
		os.mkdir(outputPath)
	spamFolder = outputPath + os.sep + "spam" + os.sep
	hamFolder = outputPath + os.sep + "ham" + os.sep
	if not os.path.exists(spamFolder):
		os.mkdir(spamFolder)
	if not os.path.exists(hamFolder):
		os.mkdir(hamFolder)	

def createOutputFolder(outputPath):
	"""Creates a folder named as the argument in the output path where all the lemmatized files will end up."""
	if not os.path.exists(outputPath):
		os.mkdir(outputPath)

def getDefaultDataPath():
	return spamFunctions.getDataPath()
	
def getDefaultSMSPath():
	return spamFunctions.getDefultSMSpath()

def lemmatizeTextInFile(inputFilePath, outputPath, lineMode = False, debug = False, fileSeperator = ""):
	messageCounter = 0
	createOutputFolder(outputPath)
	input_ = open(inputFilePath, "r")
	lemmatizedText = getLemmatizedText_(input_)
	input_.close()
	messageBuffer = io.StringIO(lemmatizedText)
	print("lemmatizeTextInFile: Starting Lemmatization") if debug else ()
	if(lineMode):
		for line in (messageBuffer.readlines()):
			outputFile = open(outputPath + os.sep + fileSeperator + "message" + str(messageCounter), "w+")
			messageCounter += 1
			outputFile.write(line)
			outputFile.close()
			messageBuffer.close()
	else:
		outputFile = open(outputPath + os.sep + fileSeperator + "message" + str(messageCounter), "w+")
		messageBuffer.seek(0)
		for text in messageBuffer.read():
			outputFile.write(text)
		messageCounter += 1
		outputFile.close()
		messageBuffer.close()
	print("lemmatizeTextInFile: Lemmatized " + str(messageCounter) + " messages") if debug else ()
	
def lemmatizeTextInDataFolder(inputFolder, output, matchString = ".txt", lineMode = False, debug = False):
	fileCounter = 0
	filesToParse = []
	for file in os.listdir(inputFolder):
		if file.endswith(matchString):
			filesToParse.append(file)
	print("lemmatizeTextInDataFolder: Found " + str(len(filesToParse)) + " files matching in the folder: " + inputFolder + " that matches: " + matchString) if debug else()
	for filePaths in filesToParse:
		filePaths = inputFolder + os.sep + filePaths
		print("lemmatizeTextInDataFolder: Applying lemmatization on the file: " + filePaths) if debug else()
		lemmatizeTextInFile(filePaths, output, lineMode, debug, str(fileCounter))
		fileCounter += 1
	print("lemmatizeTextInDataFolder: Done applying lemmatization on " + str(fileCounter) + " files.") if debug else()

##Special method for the SMS dataset
def lemmatizeSMSset(input, output, debug = False):
	createOutputSMS(output)
	outputPathSpam = output + os.sep + "spam" + os.sep
	outputPathHam = output + os.sep + "ham" + os.sep
	msgCounterSpam = 0
	msgCounterHam = 0
	inputFile = open(input, "r")
	messageBuffer = io.StringIO(getLemmatizedText_(inputFile))
	print("lemmatizeSMSset: Starting Lemmatization") if debug else ()
	for line in messageBuffer.readlines():
		if spamFunctions.isSpamSMS(line):
			outputFile = open(outputPathSpam + 'message' + str(msgCounterSpam), "w+")
			msgCounterSpam += 1
		else:
			outputFile = open(outputPathHam + 'message' + str(msgCounterHam), "w+")
			msgCounterHam += 1
		line = spamFunctions.preprocessingSMS(line)
		outputFile.write(line)
		outputFile.close()
	messageBuffer.close()
	print("lemmatizeSMSset: Finished Lemmatization") if debug else ()

##Assuming everything is named and placed right this will create the preprocessed spam dataset
def createDefaultSMSDataset():
	lemmatizeSMSset(getDefaultSMSPath(), (getDefaultDataPath() + os.sep + "SMSpreprocessed"), False)

##Fast script to get the enron dataset.
def createEnron():
	dataPathHam = spamFunctions.getDefaultEnronPre() + os.sep + "ham" + os.sep
	dataPathSpam = spamFunctions.getDefaultEnronPre() + os.sep + "spam" + os.sep
	outPathHam = dataPathHam + "lem" + os.sep
	outPathSpam = dataPathSpam + "lem" + os.sep
	lemmatizeTextInDataFolder(dataPathHam, outPathHam, ".txt", False, False)
	lemmatizeTextInDataFolder(dataPathHam, outPathSpam, ".txt", False, False)

def test():
	print("dataLemmatizer got called")