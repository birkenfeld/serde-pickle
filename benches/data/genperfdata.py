import random
import string
import pickle

l = [i for i in range(10000)]
pickle.dump(l, open("biglist.pickle", "wb"))

obj = [1, 2, 3, 4, 5]
l = [obj for _ in range(10000)]
pickle.dump(l, open("manyrefs.pickle", "wb"))

l = [''.join(random.sample(string.ascii_letters, 32)) for _ in range(10000)]
pickle.dump(l, open("manystrings.pickle", "wb"))
