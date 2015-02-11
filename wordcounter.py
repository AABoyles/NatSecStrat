import re
from os import listdir
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords

print "Year,Word,Count"

stpwrds = stopwords.words('english')

for f in listdir('txts'):
	f2words = word_tokenize(
		re.sub(r'[^a-z ]',r'',
			open('txts/'+f,'r')
				.read()
				.lower()
		)
	)
	words = {}
	for word in f2words:
		word = word.strip()
		if word in stpwrds: continue
		if word in words: words[word] += 1
		else: words[word] = 1
	for word, count in words.iteritems():
		print ",".join([str(f),word,str(count)])

