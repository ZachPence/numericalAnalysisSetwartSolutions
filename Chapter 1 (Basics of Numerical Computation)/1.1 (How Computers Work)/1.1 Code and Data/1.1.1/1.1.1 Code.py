"""
Author: Zachariah Pence
Date: 5/4/2024

This is code for Problem 1 in Section 1.1. In particular, we want to perform
the following:
    1. Implement the matrix multiplication algorithm in base python (i.e., 
       without using NumPy)
    2. Set up an experiment to compare the speed of the function created in 
       Task (1) to the built-in matrix multiplication function in NumPy. 
"""
import numpy as np
from time import time
from typing import List


def matrix_multiply(A, B):
    """
    Performs matrix multiplication using base Python. We also generalize it a bit
    further than what Problem 1.1.1 asks of us. We allow for rectangular matrices
    (not just square). This function will assume each input is not jagged. 
    
    Parameters
    ----------
    A: List[List[float]]
        An n x m matrix
        
    B: List[List[float]]
        An m x p matrix
        
    Returns
    -------
    A: List[List[float]]
        An n x p matrix
        
    Raises
    ------
    ValueError
        If the dimensions are in valid. That is, when the width of matrix A does
        not match the length of B
    """   
    
    WIDTH_A = len(A[0])
    LENGTH_B = len(B)
    
    # Verify dimension are correct
    if WIDTH_A != LENGTH_B:
        raise ValueError("Invalid dimensions.")
    
    # Start multiplication
    n = len(A) # length of output matrix
    p = len(B[0]) # width of output matrix
    
    
    C = [[0 for _ in range(p)] for _ in range(n)]
    for i in range(n):
        for j in range(p):
            for k in range(min(n, p)):
                C[i][j] += A[i][k]*B[k][j]
    return C

def verify_implementation():
    # Verify Implementation of matrix_multiplication
    A_matrices = [[[1,2],[3,4]],
                  [[1,2],[3,4],[5,6]],
                  [[1,2],[3,4]],
                  [[1,0],[0,1]],
                  [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],
                  [[0,-1,0,-0.5],[12,11,10,9],[8,7,6,5],[4,3,2,1]]]
    B_matrices = [[[1,2],[3,4]],
                  [[1,2],[3,4]],
                  [[1,2,5],[3,4,6]],
                  [[1,2],[3,4]],
                  [[16,15,14,13],[12,11,10,9],[8,7,6,5],[4,3,2,1]],
                  [[1,2,3,4],[0,-1,-0.5,0],[9,10,11,12],[13,14,15,16]]]
    
    for A,B in zip(A_matrices, B_matrices):
        print(f"{A=}")
        print(f"{B=}")
        print(f"C={matrix_multiply(A,B)}")
        print("")
        
        
def main():
    ## Speed Test (Generate Data)
    rand_mat_1 = np.loadtxt("rand_matrix_1_100_by_100.txt")
    rand_mat_2 = np.loadtxt("rand_matrix_2_100_by_100.txt")
    
    # Without numpy
    base_python_data = np.zeros(100)
    for i in range(100):
        start = time()
        matrix_multiply(rand_mat_1, rand_mat_2)
        end = time()
        base_python_data[i] = end-start
        
    # With numpy
    numpy_data = np.zeros(100)
    for i in range(100):
        start = time()
        rand_mat_1 * rand_mat_2
        end = time()
        numpy_data[i] = end-start
     
    # Save data to files
    # np.savetxt("base_python_data.txt", base_python_data.transpose())
    # np.savetxt("numpy_data.txt", numpy_data.transpose())
    
    ## Speed Test (Results)
    print(f"Mean Execution Time for Base Python: {np.mean(base_python_data)}")
    print(f"Mean Execution Time with Numpy: {np.mean(numpy_data)}")
    print(f"Percent Difference: {np.abs(np.mean(numpy_data) - np.mean(base_python_data))*100 /np.mean(base_python_data) }%")
if __name__ == "__main__":
    main()