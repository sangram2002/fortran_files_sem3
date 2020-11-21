program q2
   ! A program that approximates a value of a function/experimental quantity at a point based on a given set of data (n points)
    
implicit none
integer,parameter:: n=3! number of data points
integer :: i,k
real ,dimension(n):: xvect, yvect! vectors of known data: x,y-values 

real :: x, Px  ! the point at which the value of lagrangian polynomial P(x) is to be calculated and the value of the  Lagrangian polynomial at that point

! The data
  data (xvect(i), i=1,n)/0.82,0.83,0.84/
  data (yvect(i), i=1,n)/2.270500,2.293319,2.31637/



! initilisation of the first point of evaluation
x = 0.82
  do k=1,41                !for x ranging from 0.82 to 0.86 with step size 0.001,hence no. of x values =[(0.86-0.82)/0.001]+1=41(no. of rows)
    
    ! Performing the interpolation
    Px = Lagrange(x,n,xvect,yvect)
    open(unit=1,file='Data_to_plot(1).dat')  !opening a new file to plot the data
    write(1,*) x, Px
    x=x+0.001
end do

print*,'The approximate value of the function at 0.826 i.e. e^(o.826)=',Lagrange(0.826,n,xvect,yvect)

x = 0
  do k=1,87                !for x ranging from 0 to 0.86 with step size 0.01,hence no. of x values =[(0.86-0)/0.01]+1=87(no. of rows)
    
    ! Performing the interpolation
    Px = Lagrange(x,n,xvect,yvect)
    open(unit=1,file='Data_to_plot(2).dat')  !opening a new file to plot the data
    write(1,*) x, Px
    x=x+0.01
end do

contains
function Lagrange(x,n,xvect,yvect) result(Px)

    implicit none
    real :: x,Px
    integer :: i, j,n
    real ,dimension(n):: xvect, yvect,L! vectors of known data: x,y-values & Lagrange cardinal function
    
    ! Initializations of Px and L
    Px = 0 ! initializing the polynomia value at x
    L  = 1  ! initalizing the vector of cardinal functions to 1
        do i = 1,n
            do j = 1, n
               if (i /= j) then
                  L(i) = ( (x - xvect(j)) / (xvect(i) - xvect(j)) )* L(i) ! part of L(i)
               end if
            end do
            Px = Px + L(i)*yvect(i) ! update Px ~ f(x)
          end do
    
    end function Lagrange


end program