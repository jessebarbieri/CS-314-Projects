import os 
import os.path 
import string 
import sys 
from string import digits
#Imports the library needed to create histogram and scatter plots 
# ** REMEMBER TO SOURCE ACTIVATE PYTHON27 **
import matplotlib 
matplotlib.use('Agg')

import matplotlib.pyplot as plt 
import numpy as np 

#python examine.py arg1 arg2 arg3 ... (subdir's chosen)
#    find texts in directory
#    open and read texts 

cur_dir = os.getcwd()
sub_dir = sys.argv[1:]

for i in sub_dir:
    for files in os.listdir(i):
        cur_file = os.path.join(cur_dir,i,files)
        if os.path.isfile(cur_file):  
            f = open(cur_file, 'r')
            #split original string into seperate strings for each word and put everything in lowercase
            word_org = f.read().lower().split()
            
            #The intent of this section is to remove special characters and numbers from our lists
            word_numremov = [x for x in word_org if not any(y.isdigit() for y in x)] #filter out numbers
            filter_more = ['"', '', '@','#','%', '[]'] 
            word = [x for x in word_numremov if x not in filter_more]
            
            post_punc = []
            word_dict = {}
            for the_strings in word:
                post_punc.append(the_strings.strip(string.punctuation))
                if post_punc[-1] in word_dict: 
                    word_dict[post_punc[-1]] += 1
                else: 
                    word_dict[post_punc[-1]] = 1
        total_words = len(word) #total number of words
        float(total_words) #typecasting the number to float

        # y_words = word_dict.keys()
        # freq = word_dict.values()
        y_pos = np.arange(len(word_dict.keys()))
        y_words, freq = zip(*word_dict.items())
        
        y_size = len(y_words)
        if len(y_words) > 600: 
            y_size = 600
        
        fig, ax = plt.subplots(figsize = (20, y_size))
        
        plt.barh(y_pos, [float(x)/total_words for x in freq], height = 1, align = 'center')
        plt.title('Discrete Distribute of Latin Literature')
        plt.yticks(y_pos, y_words, fontsize = 10)
        plt.ylabel('Words')
        plt.xlabel('Frequency')
        plt.xlim(0,1)
        
        plt.savefig(cur_file + ".png")
      

        
        

            



            
            

            


            
            

