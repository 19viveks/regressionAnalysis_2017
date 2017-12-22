from mesa import Agent, Model
from mesa.time import RandomActivation
from mesa.space import SingleGrid
from mesa.datacollection import DataCollector

import random
import matplotlib.pyplot as plt
import numpy as np

import math
import time

#%matplotlib inline

#-----------------------------------------------------------------------------

#Universal constants

#Decay rate of inhibitor
MU_H = 5
#MU_H = 50

#Decay rate of activator
MU_A = 1

#Diffusion rate of activator
#D_A = 0.005
D_A = 0.05

#Diffusion rate of inhibitor
#D_H = 0.05
D_H = 0.5

#Production rate of both chemicals
RO = 1

#Steady-state concentrations
STEADY_CC_ACT = MU_H/MU_A
STEADY_CC_INH = RO*MU_H/(math.pow(MU_A,2))


#Number of time points to be considered
TIME = 10

#Time fraction
#DT = 0.0001
DT = 1

#Size of grid (not including boundaries)
N = 5


#-----------------------------------------------------------------------------


class Cell(Agent):
    """ An agent/cell with fixed initial activator and inhibitor concentrations."""
    def __init__(self, unique_id, model):
        super().__init__(unique_id, model)
        self.act = STEADY_CC_ACT
        self.inh = STEADY_CC_INH
        self.x = float("-inf")
        self.y = float("-inf")

    #Calculate the net production of activator in a cell, using the Meinhardt equation for activator production
    def calculateActivatorReaction (self, mu):
        return DT*(RO*math.pow(self.act,2)/self.inh - mu*self.act)

    
    #Calculate the net production of inhibitor in a cell, using the Meinhardt equation for inhibitor production
    def calculateInhibitorReaction (self, mu):
        return DT*(RO*math.pow(self.act,2) - mu*self.inh)


    #Calculate the net diffusion of a cell
    #If the cell is at a boundary, treat the diffusion of the cell as 0
    def calculateDiffusion(self, y, x, data, D):
        n = self.model.currentActivatorGrid.width #length of grid, assuming that it is nxn    
        
        possible_steps = self.model.oldActivatorGrid.get_neighborhood(
        self.pos,
        moore=True,
        include_center=False)
    
        if(x > 0 and x < n-1):
            if(y > 0 and y < n-1):
                return (0.25*D*(data[y-1][x] + data[y+1][x] + data[y][x-1] + data[y][x+1] - 4*data[y][x]))
        
        else:
            return 0
    
    def step(self):
        actGrid = self.model.oldActivatorGrid
        activatorDiffusion = self.calculateDiffusion(self.y, self.x, actGrid, D_A)
        #print("Activator Diffusion is " + str(activatorDiffusion))
        activatorProduction = self.calculateActivatorReaction(MU_A)
        #print("Activator Production is " + str(activatorProduction))
        
        inhGrid = self.model.oldInhibitorGrid
        inhibitorDiffusion = self.calculateDiffusion(self.y, self.x, inhGrid, D_H)
        #print("Inhibitor Diffusion is " + str(inhibitorDiffusion))
        inhibitorProduction = self.calculateInhibitorReaction(MU_H)
        #print("Mu_H is " + str(MU_H))
        #print("Inhibitor Production is " + str(inhibitorProduction))
        
        self.act = self.act + activatorProduction - activatorDiffusion
        self.inh = self.inh + inhibitorProduction - inhibitorDiffusion

#-----------------------------------------------------------------------------

class ReactionDiffusionModel(Model):
    """A model with some number of agents."""
    #Initialize a model that includes a grid of side-length N and one agent for each grid
    def __init__(self, N):
        #Number of agents
        self.num_agents = N*N
        
        #The two grids can have just one agent per cell, it is dimensions NxN, and it is toroidal
        self.oldActivatorGrid = SingleGrid(N, N, True)
        self.oldInhibitorGrid = SingleGrid(N, N, True)
        self.currentActivatorGrid = SingleGrid(N, N, True)
        self.currentInhibitorGrid = SingleGrid(N, N, True)
        
        #Determine how our model will pick agent to interact with
        self.schedule = RandomActivation(self)

        # Create agents
        for i in range(self.num_agents):
            #Initialize a cell with uniqueID = i
            a = Cell(i, self)
            
            #Add our agent to our scheduler
            self.schedule.add(a)
            
            #Choose a random, unoccupied cell in our grid and add our agent to it
            #position_agent stores the x and y value for each of our agents
            locationTuple = self.oldActivatorGrid.find_empty()
            if(locationTuple) == (N/2, N/2):
                a.act = 2*a.act
            self.oldActivatorGrid.place_agent(a, locationTuple)
            self.oldInhibitorGrid.place_agent(a, locationTuple)
            self.currentActivatorGrid.place_agent(a, locationTuple)
            self.currentInhibitorGrid.place_agent(a, locationTuple)
    
    #Method to get activator values in our current activator grid
    def getActivatorGrid(self):
        activator_Grid = np.zeros((self.currentActivatorGrid.width, self.currentActivatorGrid.height))
        for cell in model.currentActivatorGrid.coord_iter():
            cell_content, x, y = cell
            activator_Grid[x][y] = cell_content.act
        return activator_Grid
    
    
    #Method to get inhibitor values in our current inhibitor grid
    def getInhibitorGrid(self):
        inhibitor_Grid = np.zeros((self.currentInhibitorGrid.width, self.currentInhibitorGrid.height))
        for cell in model.currentInhibitorGrid.coord_iter():
            cell_content, x, y = cell
            inhibitor_Grid[x][y] = cell_content.inh
        return inhibitor_Grid

    def step(self):
        #Determine what the original activator and inhibitor distributions are
        oldActivatorGrid = self.currentActivatorGrid
        oldInhibitorGrid = self.currentInhibitorGrid
        
        #Perform a step of the model, where we calculate all of the new concentrations
        self.schedule.step()
        
        #Determine the new activator and inhibitor distributions
        currentActivatorGrid = self.getActivatorGrid()
        currentInhibitorGrid = self.getInhibitorGrid()

#-----------------------------------------------------------------------------

model = ReactionDiffusionModel(N)

for i in range(5):
    currentActivatorGrid = model.getActivatorGrid()
    currentInhibitorGrid = model.getInhibitorGrid()
    
    #print(currentActivatorGrid)
    #print(currentInhibitorGrid)
   
    fig_act = plt.pcolor(currentActivatorGrid)
    plt.axis([0, len(currentActivatorGrid[0]), 0, len(currentActivatorGrid)])
    plt.colorbar()
    plt.show(fig_inh)
    
    model.step()

#-----------------------------------------------------------------------------

model = ReactionDiffusionModel(N)

for i in range(5):
    currentActivatorGrid = model.getActivatorGrid()
    currentInhibitorGrid = model.getInhibitorGrid()
    
    #print(currentInhibitorGrid)
    
    fig_inh = plt.pcolor(currentInhibitorGrid)
    plt.axis([0, len(currentInhibitorGrid[0]), 0, len(currentInhibitorGrid)])
    plt.colorbar()
    plt.show(fig_inh)
    
    model.step()
