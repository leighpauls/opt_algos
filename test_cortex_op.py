from cortex.algo.operation import Insert, Delete, Create, Remove, Move
from cortex.algo import CortexNode

def main():
    value1 = CortexNode(['a', 'b', 'c'])
    value2 = CortexNode(['a', 'b', 'c'])
    
    op1 = Insert(None, 1, [], 1, 'd')
    op2 = Insert(None, 2, [], 1, 'e')
    
    op1_over2 = op1.transform(over=op2, end_node=None)
    op2_over1 = op2.transform(over=op1, end_node=None)

    print value1.__dict__
    print value2.__dict__
    print
    
    op1.apply(value1)
    op2_over1.apply(value1)

    op2.apply(value2)
    op1_over2.apply(value2)

    print value1.__dict__
    print value2.__dict__
    

if __name__ == "__main__":
    main()
