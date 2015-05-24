import dataLemmatizer
import argparse

parser = argparse.ArgumentParser(description="This is a CLI for the dataLemmatizer")

parser.add_argument("input", help="The input dataset, either a folder or a file", )
parser.add_argument("output", help="The folder location where the data should be generated.")
parser.add_argument("-d", "--debug", help="Sets the debug mode to true if it's supported. Gives additional prints.", action="store_true")
parser.add_argument("-lm", "--linemode", help="If set it will parse the datafiles line by line, creating a new file for each line.", action="store_true")
parser.add_argument("-fs", "--filesuffix", help="Sets the file suffix of the data files that will be lemmatized in the input folder. Default is '.txt'", default=".txt")
parser.add_argument("-sms", "--sms", help="Set this flag for the SMS corpus dataset. Enbales preprocessing and categorization of the messages in this dataset.", action="store_true")
parser.add_argument("-sf", "--singleFile", help="Explicit telling that the input is a single datafile. This will ignore the filesuffix. Useful for testing.", action="store_true")
parser.add_argument("-defaultSMS", "--getDefaultSMS", help="Calls the function to create the preprocessed SMS corpus. Overruns all other arguments and assumes the folder structure and file locations are default.", action="store_true")
parser.add_argument("-defaultEnron", "--getDefaultEnron", help="Calls the function to create the preprocessed Enron. Overruns all other arguments and assumes the folder structure and file locations are default.", action="store_true")

args = parser.parse_args()

print("Input file %s" % args.input)
print("Output file %s" % args.output)
print("FileSuffix file %s" % args.filesuffix)
print("Debug mode is set to %s" % args.debug)
print("LineMode is set to %s" % args.linemode)
print("SMS is set to %s" % args.sms)
print("dSMS is set to %s" % args.getDefaultSMS)
print("dEnron is set to %s" % args.getDefaultEnron)

if(args.getDefaultSMS):
	print("Calling default SMS method.") if args.debug else ()
	dataLemmatizer.createDefaultSMSDataset()
	print("SMS spam corpus dataset created.") if args.debug else ()
elif(args.getDefaultEnron):
	print("Calling default Enron method.")if args.debug else ()
	dataLemmatizer.createEnron
	print("Enron preprocessed datasets created.")if args.debug else ()
elif(args.sms):
	print("SMS enabled, calling lemmatizeSMSset method.")if args.debug else ()
	dataLemmatizer.lemmatizeSMSset(args.input, args.output, args.debug)
	print("SMS spam corpus dataset created.")if args.debug else ()
elif(args.singleFile):
	print("Single file mode enabled, calling lemmatization on file.")if args.debug else ()
	dataLemmatizer.lemmtizeTextInFile(args.input, args.output, args.linemode, args.debug)
	print("Single file lemmatized.")if args.debug else ()
else:
	print("Calling lemmatization on target.")if args.debug else ()
	dataLemmatizer.lemmatizeTextInDataFolder(args.input, args.output, args.filesuffix, args.linemode, args.debug)
	print("Lemmatization of target file(s) done.")if args.debug else ()
