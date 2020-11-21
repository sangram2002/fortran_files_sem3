
program Cubic_spline_interpolation
    implicit none
    
    integer, parameter::n=7
    double precision:: eqn(n-2,n)=0,temp(n-1),temp1,x,p,q,r,s,Sdf(n,1)=0,m(n-2,n-2),a(n-2,n-1)
    integer i, j, k
    double precision, dimension(n) :: xvect, yvect

! =================================================================================================
! Variable          Explanation
! -----------------------------------------
! n                 no. of data points
! eqn               original (n-2)xn matrix i.e. It has n-2 equations with  s"(x)_i as variables
! a                 the matrix with dimension (n-2)x(n-2) to calculate n-2 variables of s"(x) 
! xvect,yvect       vectors of known data: x,y-values 
! x                 The point at which you want the value of s(x)
! Sdf               matix of double derivative of s(x) i.e. s"(x)
! m                 multipliers in gaussian elimination method
! temp              Temporary Array
! temp1             temoprary variable
! p                 (xvect(i)-x)
! q                 (x-xvect(i-1))  
! r                 (xvect(i)-xvect(i-1))
! s                 value of the Cubic spline polynomial s(x)
! =================================================================================================

    
! The data
data (xvect(i), i=1,n)/0,1,2,2.5,3,3.5,4/
data (yvect(i), i=1,n)/2.5,0.5,0.5,1.5,1.5,1.125,0/

!initialing the coefficients of the matrix which is to be solved to get n values of s"(x)
do i = 1, n-2
    eqn(i,i)=(xvect(i+1)-xvect(i)) / 6.0d0
    eqn(i,i+1)=(xvect(i+2)-xvect(i)) / 3.0d0
    eqn(i,i+2)=(xvect(i+2)-xvect(i+1)) / 6.0d0
      
end do
do i = 1, n-2
    do j = 1, n-2
        a(i,j)=eqn(i,j+1)
        
    end do
    a(i,n-1)=((yvect(i+2)-yvect(i+1)) / (xvect(i+2)-xvect(i+1))) &
             - ((yvect(i+1)-yvect(i)) / (xvect(i+1)-xvect(i)))
end do

do i=1,n-2                  
    print*, (a(i,j),j=1,n-1)
  end do
!Partial pivoting, by making multipliers |m(i,k)|<= 1 for 1<=k<i<=n to reduce error 
print*,
do k = 1, n-3
    
    do i=k,n-2
        do j=i+1,n-2
            if ( abs(a(i,k))<abs(a(j,k)) ) then
                temp=a(i,1:)        !temp is a temporary array to swap two rows
                a(i,1:)=a(j,1:)     !bubble sorting method to sort the whole row in descending order by absolute value of a(i,k)
                a(j,1:)=temp
            end if
        end do
    end do
    
   

!Gaussian elimination method        
    do i = k+1, n-2
        m(i, k) = a(i, k) / a(k, k) !To calculate the multipliers 
    end do
   
    do j = k, n-1
        do i = k+1, n-2
            temp1=m(i, k) * a(k, j)     !temp1 is a temporary variable to store the product 
            a(i, j) = a(i, j) - temp1  !{Elimination method}To make n-k(4,3...) elements (from bottom) of  kth(1,2...) column 0
            
        end do
    end do  

end do


do i=n-1,2,-1
    temp1=0                                
    do k=i+1,n-1
        temp1=temp1 + a(i-1,k-1)*Sdf(k,1)      !Back substitution method
        end do
    Sdf(i,1) = (a(i-1,n-1) - temp1) / a(i-1,i-1)
end do


    x = 0  ! initilisation of the first point of evaluation

    do while(x<4.01)                !for x ranging from 0 to 4 with step size 0.05
      do i = 2, n
          if ( x>=xvect(i-1) .and. x<=xvect(i) ) then
              exit
          end if
      end do
      p=(xvect(i)-x)
      q=(x-xvect(i-1))  
      r=(xvect(i)-xvect(i-1))

      s = ((p**3 * Sdf(i-1,1) + q**3 * Sdf(i,1)) / (6.0d0*r)) &
          + ((p*yvect(i-1) + q*yvect(i)) / r) &
          - ((r/6.0d0) * (p*Sdf(i-1,1) + q*Sdf(i,1)))   !The value of the Cubic spline polynomial s(x) found for these set of points 
        open(unit=1,file='Data_to_plot.dat')  !creating a new file to plot the data
        write(1,*) x,s
        x=x+0.05
    end do
  
print*,'you have succesfully created a data file to be plotted.'

end program