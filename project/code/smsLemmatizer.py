from nltk.stem.wordnet import WordNetLemmatizer
import re, os, io, glob, argparse
import spamFunctions

#path_ = os.getcwd()
#name of the file that contains the data.
#fileName = "SMSSpamCollection"
#This is the path to the file that contains the original data.
#filePath = path_ + os.sep + ".." + os.sep + "data" + os.sep + fileName
lemmatizer = WordNetLemmatizer()

# Returns all words from a textLine without any special characters
def __getWordsFromString(textLine):
	wordList = re.sub('[^a-zA-Z+]', " ", textLine).split()
	return wordList
	
# Takes a line of Text and applies the Lemmatizer on each word in it. Returns a
# dictionary with original words mapped to the lemmatized words. Only returns
# lower case characters.
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

def getDictionary(textFile):
	text=""
	for line in textFile:
		text += textFile.readline()
	return buildDictionaryFromTextBlock(text)

#def getDictionary():
#	"""Returns a dictionary for the spamCollection with word:lemma as mapping. DEPRECATED"""
#	spamCollection = open(filePath, "r")
#	temp = getDictionary_(spamCollection)
#	spamCollection.close()
#	return temp
	
def getLemmatizedText_(file):
        """ #returns a version of the file with lemmatization applied to it as
        a string.	"""
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
	
def getLemmatizedText():
        """ #returns a version of the SMSSpamCollection text with lemmatization
        applied to it as a string.	"""
	spamCollection = open(filePath, "r")
	spamCollection.seek(0)
	lemmatizedText = getLemmatizedText_(spamCollection)
	spamCollection.close()
	return lemmatizedText

#NOT TESTED
def __lemmatizeTextInFile(input, outputPath, spamClassificationFunction, preprocessingFunction, lineMode = True):
	"""Lemmatizes the text in a file line by line or the whole file.
	
	Keyword Arguments:
        input -- The file that contains all the data. This should be a path
        including the file name.
        outputPath -- This is the path to the folder where the output should
        be. It should contain a folder named "spam" and one named "ham" where
        the output will be generated.
        spamClassificationFunction -- A function String -> Boolean that returns
        true if a line or a file is a spam.
        preprocessingFunction -- A String -> String function that returns the
        text with all desired preprocessing applied to it.
        lineMode -- A boolean that tells if input should be parsed line by line
        or in a chunk until EOF.
	"""
	messageCounterHam = 0
	messageCounterSpam = 0
	messagebuffer = io.StringIO(getLemmatizedText_(input))
	for line in (messageBuffer.readlines() if lineMode else messageBuffer.read()):
		if isSpam(line):
			outputFile = open(outputPath + "message" + str(messageCounterSpam), "w+")
			messageCounterSpam += 1
		elif not spam(line):
			outputFile = open(outputPath + "message" + str(messageCounterHam), "w+")
			messageCounterHam += 1
		line = preprocessingFunction
		outputFile.write(line)
		outputFile.close()
		
def createOutputFolders(outputPath):
        """If the folders don't exist it creates the outputPath folder
        containing one folder named spam and one folder named ham."""
	if not os.path.exists(outputPath):
		os.mkdir(outputPath)
	spamFolder = outputPath + os.sep + "spam" + os.sep
	hamFolder = outputPath + os.sep + "ham" + os.sep
	if not os.path.exists(spamFolder):
		os.mkdir(spamFolder)
	if not os.path.exists(hamFolder):
		os.mkdir(hamFolder)

def lemmatizeFilesInFolder_(inputPath, outputPath, spamClassificationFunction, preprocessingFunction, dataFileMatching, lineMode = True, silentMode = True):
	"""Lemmatizes all files in a folder and output them at a desired place.
	
	Keyword Arguments:
        inputPath -- The folder that contains all files with the data. This
        should be a path including the file name.
        outputPath -- This is the path to the folder where the output should
        be. It should contain a folder named "spam" and one named "ham" where
        the output will be generated.
        spamClassificationFunction -- A function String -> Boolean that returns
        true if a line or a file is a spam.
        preprocessingFunction -- A String -> String function that returns the
        text with all desired preprocessing applied to it.
        lineMode -- A boolean that tells if input should be parsed line by line
        or in a chunk until EOF.
        dataFileMatching -- A string describing the glob pattern of the data
        files. For example "*.data" would only pick up all files in inputPath
        that ends with the suffix "data". Unknown behaviour if matching
        folders.
        silentMode -- If set to false the function returns text feedback about
        what it's doing.
	"""
	createOutputFolders(outputPath)
	fileCounter = 0
	for file in glob.glob(dataFileMatching):		
		print("Applying lemmatization on the file: " + file) if not silentMode else()
		__lemmatizeTextInFile(file, outputPath, spamClassificationFunction, preprocessingFunction, lineMode)
		fileCounter += 1
	print("Done applying lemmatization on " + str(fileCounter) + " files.") if (not silentMode) else()
	
parser = argparse.ArgumentParser(prog="Spam Lemmatizer")
parser.add_argument("dataFolder", help="Path to the folder containing the dataset")
parser.add_argument("outputPath", help="Path to the folder where the script will output the spam and ham folder containing the lemmatized dataset. ")
parser.add_argument("spamClassificationFunction", help="Function that classify each message file or line as spam or ham. The options are: \n" + spamFunctions.getIsSpamText(), type=int, choices=[0, 1])
parser.add_argument("preprocessingFunction", help="Function to perform the preprocessing.", type=int, choices=[0, 1])
parser.add_argument("-p","--pattern", help="A pattern matching the datafiles suffix in the folder.")
parser.add_argument("-l", "--lineMode", help="Treats every text line in the files as a message instead of reading it to EOF.", action="store_true")
parser.add_argument("-d", "--debug", help="Prints some feedback about what files are being parsed during execution.", action="store_true")

args = parser.parse_args()
parser.parse_args()
print("Datafolder choosen is: " + args.dataFolder)
print("outputPath choosen is: " + args.outputPath)
print("Spam Classification choosen is: " + str(args.spamClassificationFunction))
print("Preprocessing function choosen is: " + str(args.preprocessingFunction))
print("Pattern chosen: " + args.pattern)
print("lineMode: " + str(args.lineMode))
print("debug: " + str(args.debug))

