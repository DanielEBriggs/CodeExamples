import os 
import sys
import argparse
import re
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

#create the parsers 
parser = argparse.ArgumentParser(description = 'This script takes a generic .tsv file and plots a scatter plot of a series of x and y values',
epilog = 'Created to automatically visualize data with a scatter plot')

#positional argument for the file to give the script
parser.add_argument('file', help = 'Pass a .tsv file to the script')

#xvariable flag
parser.add_argument('-x',
					'--xvar',
					type = int,
					help = '1-indexed X variable for plotting')

#y variable flag
parser.add_argument('-y',
					'--yvar',
					type = int,
					help = '1-indexed y variable for plotting')

#stratifying variable flag
parser.add_argument('-c',
					'--category',
					type = int,
					required = False,
					help = '1-indexed variable to stratify by')
					
#directory to save the plot					
parser.add_argument('-o',
					'--output',
					help = 'Directory in which to save the resulting plot',
					required = False)

#optional file name			
parser.add_argument('-s',
					'--savedname',
					type = str,
					default = 'scatterplot.png',
					help = 'Desired name for the plots you are saving. This requires a .png extension.',
					required = False)

#parse the args
clargs = parser.parse_args()



#init lists for 
x = []
y = []
cat = []

#try catch for reading the file
try:
	#opens the file
	with open(clargs.file, 'r') as f:
		lines = f.readlines()
		#flag to parse the first line
		first = True
		#iterate over all lines
		for line in lines:
			
			#take the line
			lineread = line.strip().split('\t')
			#if the first line we remove that dumb #
			if clargs.xvar == 1 & first:
				lineread[0] = re.sub('#', '', lineread[0])
			
			#done with the first line keep going
			first = False
			
			#if there is a categorical flag
			if clargs.category is not None:
				x.append(lineread[clargs.xvar - 1])
				y.append(lineread[clargs.yvar - 1])
				cat.append(lineread[clargs.category - 1])
			
			#if there isnt a categorical flag
			if clargs.category is None:
				x.append(lineread[clargs.yvar - 1])
				y.append(lineread[clargs.xvar - 1])					
				
#exception and warning if the file can't be read
except IOError:
    print("Could not read file:", clargs.file)
	
#extract name of the variable
x_name = x[0]

#temporary container
x_inter = x[1:]

#convert strings to floats with loop
x_series = [float(x) for x in x_inter]

#extract name of the variable
y_name = y[0]

#temporary container
y_inter = y[1:]

#convert strings to floats with loop
y_series = [float(y) for y in y_inter]

#If the directory was given 
if clargs.output is not None:
	#set the directory
	directory = os.path.dirname(clargs.output)
	#try to go to the directory otherwise make the directory
	if not os.path.exists(directory):
		os.mkdir(directory)   
	else:
		os.chdir(directory)


#process the categorical variable and give it a color encoding
if clargs.category is not None:
	c_name = cat[0] #create the category name
	c_series = cat[1:] #extract the category 
	cmap = plt.get_cmap('viridis') #I like the viridis theme 
	colors = cmap(pd.Series(c_series).astype('category').cat.codes.values / len(set(c_series))) #get the color map


#easy case when the category is not given
if clargs.category is None:
	plt.scatter(x_series, y_series) #create a scatter plot
	plt.ylabel(y_name) #plot the y-label
	plt.xlabel(x_name) #plot the x-label
	
	if clargs.output is None:
		plt.savefig(os.path.join(os.getcwd(),clargs.savedname)) #save to the current working directory
	else:
		plt.savefig(os.path.join(directory,clargs.savedname)) #save to the specified directory
	
	
if clargs.category is not None:
	plt.scatter(x_series, y_series, c = colors) #colored scatter plot :)
	plt.ylabel(y_name) # ylabel 
	plt.xlabel(x_name) #xlabel 
	patches = [] #patches to create the legend 
	list(set(tuple(row) for row in colors)) #turn the RGB color encoding into a list

	for label in range(len(set(c_series))):
		col = list(set(tuple(row) for row in colors))[label] #create the color encoding
		label = list(set(c_series))[label] #create the label
		patches.append(mpatches.Patch(color = col, label = label)) #create the patch
	
	plt.legend(handles = patches) #put the patches as the legend
	
	if clargs.output is None:
		plt.savefig(os.path.join(os.getcwd(),clargs.savedname)) #save the plot
	else:
		plt.savefig(os.path.join(directory,clargs.savedname)) #save the plto
	
