program Lagrange_int

! A program that approximates a value of a function/experimental quantity at a point based on a given set of data

  implicit none
  integer, parameter :: n = 3 ! number of data points
  integer :: i
  double precision, dimension(n) :: xvect, yvect! vectors of known data: x,y-values & Lagrange cardinal function
  double precision :: x, P  ! the point at which the value of lagrangian polynomial P(x) is to be calculated and the value of the  Lagrangian polynomial at that point

! The data
  data (xvect(i), i=1,n)/0,1,2/
  data (yvect(i), i=1,n)/-1,-1,7/

  x = 0  ! initilisation of the first point of evaluation

do i=1,81                !for x ranging from 0 to 2 with step size 0.025,hence no. of x values =[(2-0)/0.025]+1=81(no. of rows)
  P = 4*x**2-4*x-1       !The value of the lagrangian polynomial P(x) found for these set of points 
    open(unit=1,file='Data_to_plot.dat')  !opening a new file to plot the data
    write(1,*) x, P
    x=x+0.025
end do


end program Lagrange_int 