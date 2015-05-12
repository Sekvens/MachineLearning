from nltk.stem.wordnet import WordNetLemmatizer
import re
import io
import os
import sys
import smsLemmatizer

path_ = os.getcwd()

#name of the file that contains the data.
fileName = "SMSSpamCollection"
#This is the path to the file that contains the original data.
filePath = path_ + os.sep + ".." + os.sep + "data" + os.sep + fileName
outputPathSpam = path_ + os.sep + ".." + os.sep + "data" + os.sep + "SMS" + os.sep + "spam"+ os.sep
outputPathHam = path_ + os.sep + ".." + os.sep + "data" + os.sep + "SMS" + os.sep + "ham" + os.sep
spamCollection = open(filePath, "r")

# Return the length of the longest line in a file.
def getLongestLineInFile(filePath_):
	print(len(max(open(filePath_, 'r'), key=len)))

# Returns true the first word is spam, false otherwise.
def isSpam(textLine):
	return re.sub("[\^w]", " ", textLine).split()[0].lower() == "spam"
	
#Removes the first spam/ham of the textline.
def preprocessing(textLine):
	return textLine.split(None, 1)[1]

#Create the processed messages as seperate files.
def writeFiles():
	global outputPathHam
	global outputPathSpam
	if len(sys.argv) >= 2:
		try:
			spamCollection = open(sys.argv[1], "r")
		except:
			print("Could not open the data file! Your path is written as bad as a spam SMS.")
	if len(sys.argv) == 3:
		outputPathSpam = sys.argv[2] + path_ + os.sep + ".." + os.sep + "data" + os.sep + "SMS" + os.sep + "spam" + os.sep
		outputPathHam = sys.argv[2] + path_ + os.sep + ".." + os.sep + "data" + os.sep + "SMS" + os.sep + "ham"+ os.sep
		print("Setted the new output path to: " + sys.argv[2])
	if not os.path.exists(outputPathSpam):
		os.mkdir(outputPathSpam)
	if not os.path.exists(outputPathHam):
		os.mkdir(outputPathHam)
		
	messageCounterSpam = 0
	messageCounterHam = 0
	messageBuffer = io.StringIO(smsLemmatizer.getLemmatizedText())
	for line in messageBuffer.readlines():
		if isSpam(line):
			outputFile = open(outputPathSpam + 'message' + str(messageCounterSpam), "w+")
			messageCounterSpam += 1
		elif not isSpam(line):
			outputFile = open(outputPathHam + 'message' + str(messageCounterHam), "w+")
			messageCounterHam += 1
		line = preprocessing(line)
		outputFile.write(line)
		outputFile.close()
