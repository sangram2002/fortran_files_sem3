program q1
implicit none
integer,parameter::n=6
integer::i,j,k
double precision::temp,sum
double precision::matrixA(n,n),I_matrix(n,n),m(n,n),comp_matrix(n,n),ans(n,n)=0

open (unit=1,file='matrix') !To write in a file 

!initialising the matrix
print*, 'Hilbert matrux of order 6:'
do i=1,n
    do j=1,n
        temp= i+j-1
        matrixA(i,j) = 1/temp
        
    end do
    write(1,*) (matrixA(i,j),j=1,6)
    write(*,*) (matrixA(i,j),j=1,6)
end do
print*,

!initialising the Identity matrix
do i=1,n
    do j=1,n
        if ( i==j ) then
            I_matrix(i,j) = 1
        else 
            I_matrix(i,j) = 0
        end if 
    end do
end do

!Gaussian elimination method
do i = 1,n
    do j= 1,n
        if ( i==j ) then
            m(i,j)=1  
        end if
            
    end do
end do

!calling a subroutine
do i = 1,n
    call gaussian_elim(matrixA,I_matrix,m,ans,i,n)
end do

print*,'Inverse of Hilbert matrix:'
do i=1,n                         
    print*, (ans(i,j),j=1,n)
  end do
print*,
!To multiply the given matrix with that of the inverse matrix calculated by the program
print*,'checking by multiplying the given matrix with that of the inverse matrix'
  do i=1,n                         !To change rows
    do j=1,n                       !To change columns
      sum=0                        !To reset the sum for the next element in the row
      do k=1,n
        sum= sum+matrixA(i,k)*ans(k,j)     !To do the matrix multiplication of 2 nxn matrices
      end do
      comp_matrix(i,j)=sum                   !To assign values to each element of the product
    end do
  end do

  do i=1,n                         
    print*, (comp_matrix(i,j),j=1,n)
  end do

  print*,'This is almost equal to the Identity matrix as expected.'



end program


subroutine gaussian_elim(matrixA,I_matrix,m,ans,col,n)
    implicit none
    double precision::aug_matrix(n,n+1)
    double precision::matrixA(n,n),I_matrix(n,n),m(n,n),ans(n,n),temp(n+1)
    integer::n
    integer::i,j,k,col
    double precision::temp1

    !creating the augmented matrix each time for each column
    do i = 1,n
        do j = 1,n+1
            if(j==n+1)then
                aug_matrix(i,j)= I_matrix(i,col)
            else
                aug_matrix(i,j)=matrixA(i,j)
            end if
            
        end do
    end do

    !Partial pivoting, by making multipliers |m(i,k)|<= 1 for 1<=k<i<=n to reduce error 
    do k = 1, n-1
        
        do i=k,n
            do j=i+1,n
                if ( abs(aug_matrix(i,k))<abs(aug_matrix(j,k)) ) then
                    temp=aug_matrix(i,1:)        !temp is a temporary array to swap two rows
                    aug_matrix(i,1:)=aug_matrix(j,1:)     !bubble sorting method to sort the whole row in descending order by absolute value of aug_matrix(i,k)
                    aug_matrix(j,1:)=temp
                end if
            end do
        end do
        
!Gaussian elimination method        
        do i = k+1, n
            m(i, k) = aug_matrix(i, k) / aug_matrix(k, k) !To calculate the multipliers 
        end do
       
        do j = k, n+1
            do i = k+1, n
                temp1=m(i, k) * aug_matrix(k, j)     !temp1 is a temporary variable to store the product 
                aug_matrix(i, j) = aug_matrix(i, j) - temp1  !{Elimination method}To make n-k(4,3...) elements (from bottom) of  kth(1,2...) column 0
                
            end do
        end do  

    end do
    
    do i=n,1,-1
        temp1=0                                
        do k=i+1,n
            temp1=temp1 + aug_matrix(i,k)*ans(k,col)      !Back substitution method
        end do
        ans(i,col) = (aug_matrix(i,n+1) - temp1) / aug_matrix(i,i)
    end do

end subroutine gaussian_elim