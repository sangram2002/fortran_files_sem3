program LU_factorization
    implicit none
    integer i,j,k
    logical:: isMatched = .TRUE.
    real:: a(4,5),c(4,4),m(4,4)=0,ans(4,1)=0,temp,sum,original_a(4,5),LU(4,4)

a=reshape((/4,3,2,1,3,4,3,2,2,3,4,3,1,2,3,4,1,1,-1,-1/),shape(a))
original_a = a

print*,'Given matrix'
do i=1,4                         
    print*, (a(i,j),j=1,4)
  end do

print*,'Augmented matrix'
  do i=1,4                         
    print*, (a(i,j),j=1,5)
  end do

!To calculate upper and lower triangular matrix
do i = 1,4
    do j= 1,4
        if ( i==j ) then
            m(i,j)=1  
        end if
            
    end do
end do

do k = 1, 3
    do i = k+1, 4
        m(i, k) = a(i, k) / a(k, k)
    end do
   
    do j = k, 5
        do i = k+1, 4
            temp=m(i, k) * a(k, j)
            a(i, j) = a(i, j) - temp
        end do
    end do  
end do

do i=4,1,-1
    temp=0
    do k=i+1,4
        temp=temp + a(i,k)*ans(k,1) 
        end do
    ans(i,1) = (a(i,5) - temp) / a(i,i)
end do

print*,'Upper triangular matrix "U"'
 do i=1,4                         
    print*, (a(i,j),j=1,4)
  end do
print*,'Lower triangular matrix "L"'
  do i=1,4                         
    print*, (m(i,j),j=1,4)
  end do

!To calculate the product of Lower triangular matrix and upper triangular matrix
  do i=1,4                         !To change rows
    do j=1,4                       !To change columns
      sum=0                        !To reset the sum for the next element in the row
      do k=1,4
        sum= sum+m(i,k)*a(k,j)     !To do the matrix multiplication of 2 3x3 matrices
      end do
      LU(i,j)=sum                   !To assign values to each element of the product
    end do
  end do

print*,'The product of Lower triangular matrix and upper triangular matrix "LU"'
do i=1,4                         
    print*, (LU(i,j),j=1,4)
end do

print*,'Solution matrix'
do i=1,4                         
    print*, ans(i,1)
end do


!To verify the answer you got by multipling the given matrix with the solution
do i=1,4                         !To change rows                
    sum=0                        !To reset the sum for the next element in the row
    do k=1,4
        sum= sum+original_a(i,k)*ans(k,1)     !To do the matrix multiplication of the given matrix with the solution
    end do
    c(i,1)=sum                   !To assign values to each element of the product
end do

do i=1,4
    if ( original_a(i,5) /= c(i,1) ) then
        print*,'your answer is wrong,expected',original_a(i,5),'but got',c(i,1)
        isMatched = .FALSE.
    end if
end do
if (isMatched) then
    print*,' you have successfully verified the answer you got by multipling the given matrix with the solution matrix'
end if    
    
  

end program LU_factorization