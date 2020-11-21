program Lagrange_int

    ! A program that approximates a value of a function/experimental quantity at a point based on a given set of data (n points)
    
      implicit none
      integer:: n! number of data points
      integer :: i, j
     real ,allocatable,dimension(:):: xvect, yvect,L! vectors of known data: x,y-values & Lagrange cardinal function
       
      real :: x, Px  ! the point at which the value of lagrangian polynomial P(x) is to be calculated and the value of the  Lagrangian polynomial at that point
    
     
    ! The data
      Print*,'write the number of data points you have which you want to interpolate'
      read*,n

      !allocating the arrays
      allocate(xvect(n))  
      allocate(yvect(n))
      allocate(L(n))

      ! Initializations of Px and L
      Px = 0 ! initializing the polynomia value at x
      L  = 1  ! initalizing the vector of cardinal functions to 1

      write(*,'(a13,i0,a20)') 'Give all the ',n," x values serially"
      read*,xvect
        
      write(*,'(a13,i0,a65)') "Give all the ",n," corresponding y values serially in the same manner as x values"
      read*,yvect

      write(*,*)'write the point at which you want to know the value of lagrangian polynomial P(x)'
      read*,x
   
      ! Performing the interpolation
      do i = 1,n
         do j = 1, n
            if (i /= j) then
               L(i) = ( (x - xvect(j)) / (xvect(i) - xvect(j)) )* L(i) ! part of L(i)
            end if
         end do
         Px = Px + L(i)*yvect(i) ! update Px ~ f(x)
      end do
    
      write(*,*) "The approximate value of P(x) at x=",x,"is", Px 
    
    end program Lagrange_int 