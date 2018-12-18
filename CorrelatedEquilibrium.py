import numpy as np

numPlayers=2
numStrategies=np.array([2,2])

player1=np.array([[4,1],[5,0]])
player2=np.array([[4,5],[1,0]])
payoffMatrix=np.array([player1,player2])

print(payoffMatrix[1])

it = np.nditer(payoffMatrix,flags=['multi_index'])
while not it.finished:
    print("%d <%s>" % (it[0], it.multi_index)),
    rightNow=it.multi_index,
    print(rightNow[0])
    print(rightNow[0][1])
    it.iternext()

#for pEquation in range(numPlayers):
#    for iEquation in range(numStrategies[pEquation]):
#        for jEquation in range(numStrategies[pEquation]):
#            Aub = [Aub,giveInequality(pEquation,iEquation,jEquation)]