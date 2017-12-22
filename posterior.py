
import sys
from math import log, exp
from compsci260lib import get_fasta_dict

def posterior_decoding():
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

    # read the emission symbols
    emission_symbols = f_hmm_file.readline().split()
    #print("Our options for emitted symbols are " + str(emission_symbols))

    # read the emission probability matrix
    emit_probs = [None for _ in range(K)]
    for i in range(K):
        matrix_row_arry = f_hmm_file.readline().split()
        matrix_row = [float(emit_prob) for emit_prob in matrix_row_arry]
        emit_probs[i] = matrix_row
    #print("Our emission probabilities are " + str(emit_probs))

    f_hmm_file.close()
    
    seq_dict = get_fasta_dict(input_file)
    emit_str = seq_dict.values()[0]  # there's only 1
    
    print "Done reading sequence of length ", len(emit_str)
    
    # Run the forward algorithm
    forward = run_forward(states, initial_probs, transitions, emission_symbols, emit_probs, emit_str)
    
    # Run the backward algorithm
    backward = run_backward(states, initial_probs, transitions, emission_symbols, emit_probs, emit_str)
    
    # Calculate the posterior probabilities
    # Initializing the posterior 2D matrices
    posterior = [[float(0) for _ in range(K)] for _ in range(len(emit_str))]
    for i in range(len(emit_str)):
        # Did not normalize the probabilities (i.e., did not divide by P(X)),
        # because we will only use these probabilities to compare
        # posterior[i][0] versus posterior[i][1]
        for k in range(K):
            posterior[i][k] = forward[i][k] + backward[i][k]
        
    # Print out the decoded results
    #print("Final posterior matrix is " + str(posterior))
    
    #Initialize an output path
    path = ""     
        
    #Iterate in reverse order through the characters of our string
    for i in range(len(emit_str)-1, -1, -1):        
        #Initialize a maximum probability and a state index
        maxProb = float('-inf')
        stateIndex = 0
        #Iterate through the potential states for this character
        for k in range(K):
            #Find the maximum viterbi value, and based off of this value, get the corresponding state
            #print("Checking row " + str(k))
            if(posterior[i][k] > maxProb):
                maxProb = posterior[i][k]
                stateIndex = k
                           
        path = path + str(stateIndex+1)
        #print("Adding " + str(stateIndex+1))
        
        #For the first value in our output path, save out an additional state correspodning the row of the viterbi value we chose
    
    #print(path)
    #print(emit_str)
    #print(path[::-1]) 
    
    #Return and print our output in terms of the format requested (see formatPath method)
    output = formatPath(path[::-1], states)
    print("Output is " + str(output))
    return output  

def run_forward(states, initial_probs, transitions, 
    emission_symbols, emit_probs, emit_str):
    """Calculates the forward probability matrix"""

    K = len(states)
    L = len(emit_str)

    forward = [[float(0) for _ in range(K)] for _ in range(L)]

    # Initialize
    emit_index = get_emit_index(emit_str[0], emission_symbols)
    for k in range(K):
        forward[0][k] = log(initial_probs[k]) + log(emit_probs[k][emit_index])
        
    # Iterate
    for i in range(1, L):
        emit_index = get_emit_index(emit_str[i].upper(), emission_symbols)
        
        #Compute the forward probabilities for the states
        
        #Iterate through each row in our forward matrix
        for currentState in range(0, K):
            #Initialize a value for each cell in our current column
            sum = 0
            
            #Iterate through all the rows in our previous column
            for k in range(0, K):                
                #We calculate the value of the forward matrix in the last column, the transition probability btwn
                #the last state and the current state, and the emission probability for our new value in our current state
                #This happens for EVERY possible state in our previous row
                #value = forward[i-1][k]*transitions[k][currentState]*emit_probs[currentState][emit_index]
                value = forward[i-1][k] + log(transitions[k][currentState]) + log(emit_probs[currentState][emit_index])
                
                #Add this value to our overall sum
                if(sum == 0):
                    sum = value
                else:
                    sum = sumLogProbabilities(sum, value)
            
            #Our new value for our cell is the sum we just calculated
            #print("Calculating entry for: " + str(i) + ", " + str(currentState))
            forward[i][currentState] = sum
            #print(forward)   

    #print("Final forward matrix is " + str(forward))
    return forward        
        

def run_backward(states, initial_probs, transitions, 
    emission_symbols, emit_probs, emit_str):
    """Calculates the backward probability matrix"""

    K = len(states)
    L = len(emit_str)

    backward = [[float(0) for _ in range(K)] for _ in range(L)]

    # Initialize
    for k in range(K):
        backward[L-1][k] = log(1)  # which is zero, but just to be explicit...

    # Iterate
    for i in range(L-2, -1, -1):
        emit_index = get_emit_index(emit_str[i+1].upper(), emission_symbols)
        
        # Compute the backward probabilities for the states
        
        #Iterate through each row in our forward matrix
        for currentState in range(0, K):
            #Initialize a value for each row in our current column
            sum = 0.0
            
            #Iterate through all the rows in our previous column
            for k in range(0, K):                
                #We calculate the value of the forward matrix in the last column, the transition probability btwn
                #the last state and the current state, and the emission probability for our new value in our current state
                #This happens for EVERY possible state in our previous row
                #value = backward[i+1][k]*transitions[k][currentState]*emit_probs[currentState][emit_index]
                value = backward[i+1][k] + log(transitions[k][currentState]) + log(emit_probs[currentState][emit_index])
                
                #Add this value to our overall sum
                if(sum == 0):
                    sum = value
                else:
                    sum = sumLogProbabilities(sum, value)
            
            #Our new value for our cell is the sum we just calculated
            #print("Calculating entry for: " + str(i) + ", " + str(currentState))
            backward[i][currentState] = sum
            #print(backward)   
    
    #print("Final backward matrix is " + str(backward))
    return backward        
 
 
        
#A helper method to return our decoded path in the proper format
#Takes as input our decoded path and our states
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

#Given two probabilities that have ALREADY been logged, calculate the sum of the original probabilities
def sumLogProbabilities(a, b):
    return a + log(1+exp(b-a))

def get_emit_index(input_val, alphabet):
    """does a linear search to find the index of a character"""
    for i in range(len(alphabet)):
        if alphabet[i] == input_val:
            return i
    sys.exit("Could not find character " + input_val)


if __name__ == '__main__':
    posterior_decoding()
