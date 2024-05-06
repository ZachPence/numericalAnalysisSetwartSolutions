program matrixMultiplication
    implicit none
    integer :: i,j,k,m,n,p
    real, allocatable, dimension(:,:) :: A,B,C 

    m = 3
    n = 3
    p = 3
    allocate(A(m,n),B(n,p),C(m,p))

    A = transpose(reshape((/1, 2, 3, 4,5,6,7,8,9/), (/m,n/)))
    B = transpose(reshape((/1, 2, 3, 4,5,6,7,8,9/), (/n,p/)))

    do i = 1,m
        do j = 1,p 
            C(i,j)=0
            do k = 1, min(m,p)
                C(i,j) = C(i,j) + A(i,k)*B(k,j)
            end do
        end do
    end do
    deallocate(A,B)


    ! Print output in a "matrix-like" format
    do i = 1,size(C(:,1))
        do j = 1,size(C(i,:))
            print "(f20.2,$)", C(i,j)
        end do
        print *, ""
    end do
    deallocate(C)
end program matrixMultiplication