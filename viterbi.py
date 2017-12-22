
import sys
from math import log
from compsci260lib import *

def viterbi_decoding():
    hmm_file = raw_input("Enter the name of the HMM file:").strip()
    sys.stdout.flush()
    input_file = raw_input("Enter the name of the input file:").strip()
    
    f_in_file = open(input_file)
    f_hmm_file = open(hmm_file)
    
    if f_in_file is None:
        sys.exit("Can't open HMM file: " + hmm_file)
    if f_hmm_file is None:
        sys.exit("Can't open file: " + input_file)
    
    # read the state names and number
    states = f_hmm_file.readline().split()
    K = len(states)
    #print("Our states are: " + str(states))
    #print("The number K of states is " + str(K))
    
    # read the initial probabilities
    probs = f_hmm_file.readline().split()
    initial_probs = [float(prob) for prob in probs]
    #print("The initial probability of each of our states is " + str(initial_probs))
    
    # read the transition matrix
    transitions = [None for _ in range(K)]
    for i in range(K):
        matrix_row_arry = f_hmm_file.readline().split()
        matrix_row = [float(trans_prob) for trans_prob in matrix_row_arry]
        transitions[i] = matrix_row
    #print("Our transition matrix between states is " + str(transitions))
    
    # read the emitted symbols
    emitted_symbols = f_hmm_file.readline().split()
    #print("Our options for emitted symbols are " + str(emitted_symbols))
    
    # read the emission probability matrix
    emit_probs = [None for _ in range(K)]
    for i in range(K):
        matrix_row_arry = f_hmm_file.readline().split()
        matrix_row = [float(emit_prob) for emit_prob in matrix_row_arry]
        emit_probs[i] = matrix_row
    #print("Our emission probabilities are " + str(emit_probs))
    
    f_hmm_file.close()
    
    seq_dict = get_fasta_dict(input_file)
    emit_str = seq_dict.values()[0]  #there's only 1
    
    print "Done reading sequence of length ", len(emit_str)
    
    # Create Viterbi table and traceback table
    viterbi = [[0 for _ in range(len(emit_str))] for _ in range(K)]   
    pointers = [[0 for _ in range(len(emit_str))] for _ in range(K)] 

    # Initialize the first column of the matrix
    for i in range(K):
        in_index = get_emit_index(emit_str[0].upper(), emitted_symbols)
        viterbi[i][0] = log(emit_probs[i][in_index]) + log(initial_probs[i])
              
    # Build the matrix column by column
    for j in range(1, len(emit_str)):
        in_index = get_emit_index(emit_str[j].upper(), emitted_symbols)
        
        for i in range(K):
            # Compute the entries viterbi[i][j] and pointers[i][j]
            
            #Get the log of the emission probability for our next character in our sequence from each of our K states
            emission = log(emit_probs[i][in_index])
                        
            #Store the current maximum transition value for our given character
            maxValue = float('-inf')
            #Store the state that we just came from
            prevStateIndex = 0
            
            #Iterate through the rows of our DP table to find the maximum transition value
            for k in range(0, K):
                #Determine the transition probability to our kth state and sum it with the viterbi value from the previous character
                value = viterbi[k][j-1] + log(transitions[k][i])
                
                #If this value is greater than our max value, store the value and the previous state value
                if(value > maxValue):
                    maxValue = value
                    prevStateIndex = k
            
            #Set our viterbi value and our pointer value
            viterbi[i][j] = emission + maxValue
            pointers[i][j] = prevStateIndex+1
    
    #print("Final Viterbi matrix is " + str(viterbi))
    #print("Final pointer matrix is " + str(pointers))    
    
    #Initialize an output path
    path = ""     
    
    # Traceback code goes here:
    
    #Iterate in reverse order through the characters of our string
    for j in range(len(emit_str)-1, 0, -1):
        
        #Initialize a maximum probability and a state index
        maxProb = float('-inf')
        stateIndex = 0
        
        #Iterate through the potential states for this character
        for i in range(K):
            #Find the maximum viterbi value, and based off of this value, get the corresponding state
            if(viterbi[i][j] > maxProb):
                maxProb = viterbi[i][j]
                stateIndex = pointers[i][j]
                goodRow = i
        
        #For the first value in our output path, save out an additional state correspodning the row of the viterbi value we chose
        if(j == len(emit_str)-1):
            path = path + str(goodRow+1)
                   
        path = path + str(stateIndex)
    
    #print(path)
    print(emit_str)
    print(path[::-1]) 
    
    #Return and print our output in terms of the format requested (see formatPath method)
    output = formatPath(path[::-1], states)
    print("Output is " + str(output))
    return output 
    
    
#A helper method to return our Viterbi decoded path in the proper format
#Takes as input our Viterbi decoded path and our states
def formatPath(seq, states):
    #Initialize our output
    output = []
    
    #Initialize a counter for the number of values in state 2 (for part b)
    numState2 = 0
    
    #Get the current state of the first letter of our sequence and initialize start and end counters
    currentState = seq[0]
    currentStart = 0
    currentEnd = 0
    
    #Iterate through the rest of the sequence
    for i in range(1, len(seq)):
        #If the state of this character is the same as our currently saved state, add 1 to our end pointer
        if(seq[i] == currentState):
            currentEnd +=1
        
        #Otherwise, we reset our pointers and add an entry to our output with the state and it start and end pointer
        else:
            if(int(currentState) == 2):
                numState2 += 1
            output.append([states[int(currentState)-1], currentStart+1, currentEnd+1])
            currentState = seq[i]
            currentStart = i
            currentEnd = i

    #Append the last region of our sequence to our output
    output.append([states[int(currentState)-1], currentStart+1, currentEnd+1])
    if(int(currentState) == 2):
        numState2 += 1
    
    #Print out the number of regions of state 2
    print("The number of RNA regions is expected to be " + str(numState2))
    
    #Return our properly formatted output
    return output
            
         
            
def get_emit_index(input_val, alphabet):
    for i in range(len(alphabet)):
        if alphabet[i] == input_val:
            return i
    
    sys.stderr("Could not find character " + input_val)

if __name__ == '__main__':
    viterbi_decoding()
