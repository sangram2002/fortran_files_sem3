program matrix_3x3
    implicit none
    integer i,j,k
    real A(3,3),B(3,3),C(3,3),sum
  
    open (unit=1,file='matrixA.dat') !To read matrix A
    write (*,*) "matrix A"
    do i=1,3
      read (1,*) (A(i,j),j=1,3)
    end do 
  
    do i=1,3                         !To show matrix A
      print*, (A(i,j),j=1,3)
    end do

    open (unit=2,file='matrixB.dat') !To read matrix B
    write (*,*) "matrix B"

    do i=1,3
      read (2,*) (B(i,j),j=1,3)
    end do 

    do i=1,3                         !To show matrix B 
      print*, (B(i,j),j=1,3)
    end do
!To calculate the result
    do i=1,3                         !To change rows
      do j=1,3                       !To change columns
        sum=0                        !To reset the sum for the next element in the row
        do k=1,3
          sum= sum+A(i,k)*B(k,j)     !To do the matrix multiplication of 2 3x3 matrices
        end do
        C(i,j)=sum                   !To assign values to each element of the product
      end do
    end do

    print*, 'matrix A*B'
    do i=1,3                         !To show matrix C
      print*, (C(i,j),j=1,3)
    end do
!opening a new file to show matrix C   
    open(unit=3,file='matrix AxB.dat',status='new')
    write(3,*) 'matrix A*B'
    do i=1,3                         
      write(3,*) (C(i,j),j=1,3)
    end do
    close(3)

end program 
  