program matrixMultiplication
    implicit none
    integer :: i,j,k,m,n,p,seed,trial_num
    real :: start_time, stop_time, sum
    real, allocatable, dimension(:,:) :: A,B,C 
    real, dimension(100) :: execution_times
    
    m = 1000
    n = 1000
    p = 1000
    allocate(A(m,n), B(n,p),C(m,p))

    ! Generate two psuedorandom matrices A and B
    ! With integer entries on [0,100]
    seed = 5
    do i = 1,m 
        do j = 1, n 
            A(i, j) = seed
            seed = mod(58*seed + 73, 101)
        end do
    end do

    seed = 7
    do i = 1,n 
        do j = 1, p 
            B(i, j) = seed
            seed = mod(5*seed + 7, 101)
        end do
    end do

    ! C (for now) will be a matrix containing zeros
    do i = 1,m
        do j = 1,p
            C(i, j) = 0
        end do
    end do

    ! Perform matrix multiplication the 3! = 6 ways
    ! and record the execution time

    ! i, then j, then k
   
    do trial_num = 1, size(execution_times)
        call cpu_time(start_time)
        do i = 1,m
            do j = 1,p 
                do k = 1, min(m,p)
                    C(i,j) = C(i,j) + A(i,k)*B(k,j)
                end do
            end do
        end do
        call cpu_time(stop_time)

        execution_times(trial_num) = stop_time - start_time
    end do
    
    sum = 0
    do i = 1, size(execution_times)
        sum = sum + execution_times(i)
    end do
    print *, "Mean Execution Time (i,j,k):", sum / size(execution_times), " seconds."

    ! Reset C
    do i = 1,m
        do j = 1,p
            C(i, j) = 0
        end do
    end do

    ! i, then k, then j

    do trial_num = 1, size(execution_times)
        call cpu_time(start_time)
        do i = 1,m 
            do k = 1, min(m,p)
                do j = 1,p
                    C(i,j) = C(i,j) + A(i,k)*B(k,j)
                end do
            end do
        end do
        call cpu_time(stop_time)

        execution_times(trial_num) = stop_time - start_time
    end do
    
    sum = 0
    do i = 1, size(execution_times)
        sum = sum + execution_times(i)
    end do

    
    print *, "Mean Execution Time (i,k,j):", sum / size(execution_times), " seconds."

    ! Reset C
    do i = 1,m
        do j = 1,p
            C(i, j) = 0
        end do
    end do

    ! j, then i, then k
    do trial_num = 1, size(execution_times)
        call cpu_time(start_time)
        do j = 1,p
            do i = 1,m 
                do k = 1, min(m,p)
                    C(i,j) = C(i,j) + A(i,k)*B(k,j)
                end do
            end do
        end do
        call cpu_time(stop_time)

        execution_times(trial_num) = stop_time - start_time
    end do
    
    sum = 0
    do i = 1, size(execution_times)
        sum = sum + execution_times(i)
    end do
    
    print *, "Mean Execution Time (j,i,k):", sum / size(execution_times), " seconds."

    ! Reset C
    do i = 1,m
        do j = 1,p
            C(i, j) = 0
        end do
    end do

    ! j, then k, then i
    do trial_num = 1, size(execution_times)
        call cpu_time(start_time)
        do j = 1,p 
            do k = 1, min(m,p)
                do i = 1,m
                    C(i,j) = C(i,j) + A(i,k)*B(k,j)
                end do
            end do
        end do
        call cpu_time(stop_time)

        execution_times(trial_num) = stop_time - start_time
    end do
    
    sum = 0
    do i = 1, size(execution_times)
        sum = sum + execution_times(i)
    end do

    print *, "Mean Execution Time (j,k,i):", sum / size(execution_times), " seconds."

    ! Reset C
    do i = 1,m
        do j = 1,p
            C(i, j) = 0
        end do
    end do

    ! k, then i, then j
    do trial_num = 1, size(execution_times)
        call cpu_time(start_time)
        do k = 1, min(m,p)    
            do i = 1,m
                do j = 1,p 
                    C(i,j) = C(i,j) + A(i,k)*B(k,j)
                end do
            end do
        end do
        call cpu_time(stop_time)

        execution_times(trial_num) = stop_time - start_time
    end do
    
    sum = 0
    do i = 1, size(execution_times)
        sum = sum + execution_times(i)
    end do

    print *, "Mean Execution Time (k,i,j):", sum / size(execution_times), " seconds."

    ! Reset C
    do i = 1,m
        do j = 1,p
            C(i, j) = 0
        end do
    end do

    ! k, then j, then i

    do trial_num = 1, size(execution_times)
        call cpu_time(start_time)
        do k = 1, min(m,p)    
            do j = 1,p 
                do i = 1,m
                    C(i,j) = C(i,j) + A(i,k)*B(k,j)
                end do
            end do
        end do
        call cpu_time(stop_time)

        execution_times(trial_num) = stop_time - start_time
    end do
    
    sum = 0
    do i = 1, size(execution_times)
        sum = sum + execution_times(i)
    end do

    print *, " Mean Execution Time (k,j,i):", sum / size(execution_times), " seconds."

    deallocate(A,B,C)
end program matrixMultiplication