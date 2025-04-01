#nem nevesitett
def printargs(*args):
    for arg in args:
        print(arg)

printargs()
printargs("alma", 2)
printargs(1, 2,2,3,)

#nevesitett
def kvargs(**kvargs):
    print(kvargs)

kvargs(a=1,b=2,c=3)