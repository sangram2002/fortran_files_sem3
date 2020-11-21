
program ciruit_equations_with_pivotting
    implicit none
    
    integer, parameter::n=5
    real:: a(n,n+1),temp(n+1),temp1,ans(n,1)=0,m(n,n)
    integer i, j, k
    
    !Augmented matrix initialization(coefficients of the equations obtained from kirchhoff's law)
    a(1,:)=(/1,0,0,1,0,1/)
    a(2,:)=(/0,1,1,0,0,1/)
    a(3,:)=(/1,-1,0,0,-1,0/)
    a(4,:)=(/1,0,0,-4,5,0/)
    a(5,:)=(/0,2,-3,0,-5,0/)

print*,'Given matrix'
do i=1,n                  
    print*, (a(i,j),j=1,n)
  end do

print*,'Augmented matrix'
  do i=1,n                        
    print*, (a(i,j),j=1,n+1)
  end do

!Partial pivoting, by making multipliers |m(i,k)|<= 1 for 1<=k<i<=n to reduce error 
    print*,
    do k = 1, n-1
        
        do i=k,n
            do j=i+1,n
                if ( abs(a(i,k))<abs(a(j,k)) ) then
                    temp=a(i,1:)        !temp is a temporary array to swap two rows
                    a(i,1:)=a(j,1:)     !bubble sorting method to sort the whole row in descending order by absolute value of a(i,k)
                    a(j,1:)=temp
                end if
            end do
        end do
        
!Gaussian elimination method        
        do i = k+1, n
            m(i, k) = a(i, k) / a(k, k) !To calculate the multipliers 
        end do
       
        do j = k, n+1
            do i = k+1, n
                temp1=m(i, k) * a(k, j)     !temp1 is a temporary variable to store the product 
                a(i, j) = a(i, j) - temp1  !{Elimination method}To make n-k(4,3...) elements (from bottom) of  kth(1,2...) column 0
                
            end do
        end do  

    end do
    
    do i=n,1,-1
        temp1=0                                
        do k=i+1,n
            temp1=temp1 + a(i,k)*ans(k,1)      !Back substitution method
            end do
        ans(i,1) = (a(i,n+1) - temp1) / a(i,i)
    end do

print*,'Solution matrix'
do i=1,n                        
    write(*,'(a1,i0,1x,a1,f15.10)') 'I',i,'=',ans(i,1)  !To print solution with appropriate formatting
end do

end program 