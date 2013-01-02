from algos.cortex.operation import Insert, Delete, Create, Remove, Move
from algos.cortex import ValueNode

def main():
    value1 = ValueNode(['a', 'b', 'c'])
    value2 = value1.clone()
    
    op1 = Insert(None, 1, [], 1, 'd')
    op2 = Insert(None, 2, [], 1, 'e')
    
    op1_over2 = op1.transform(over=op2, end_node=None)
    op2_over1 = op2.transform(over=op1, end_node=None)

    print value1.__dict__, value2.__dict__
    
    op1.apply(value1)
    op2_over1.apply(value1)

    op2.apply(value2)
    op1_over2.apply(value2)

    print value1.__dict__, value2.__dict__
    

if __name__ == "__main__":
    main()
