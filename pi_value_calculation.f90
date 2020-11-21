PROGRAM pi_value_calculation
    IMPLICIT NONE
    integer :: count,rate,max,i,j,r,m,n
    real :: points1(100,2),k,pi_temp=0.0,points2(10000,2),points3(1000000,2)

    
    CALL SYSTEM_CLOCK(count,rate,max)  !To generate random seed by using time of the system clock 
    CALL srand(count) 

!Program for 100 points
    do r=1,5
    k=0
    do i=1,100
        do j = 1,2
        CALL SYSTEM_CLOCK(count,rate,max)
        points1(i,j)=rand()
               
      end do
    end do

    
    do i=1,100
       if ((points1(i,1)**2+points1(i,2)**2)<=1)then
        k=k+1
    end if

    end do
    pi_temp = (k/25.0)+pi_temp  !pi_temp is the temporary variable to store pi
    
end do

open (unit=1,file='point.dat',status='new') !To write in a file to plot
do m=1,100
    write(1,*)(points1(m,n),n=1,2)
   end do 

print*,'no. of points out of 100 inside the quadrant-',k,'average value of pi-',pi_temp/5

!Program for 10,000 points
pi_temp=0
do r=1,5
    k=0
    
    do i=1,10000
        do j = 1,2
        CALL SYSTEM_CLOCK(count,rate,max)
        points2(i,j)=rand()        
      end do
    end do
    
    do i=1,10000
       if ((points2(i,1)**2+points2(i,2)**2)<=1)then
        k=k+1
        end if

    end do
    pi_temp = (k/2500.0)+pi_temp
    
end do
print*,'no. of points out of 10,000 inside the quadrant-',k,'average value of pi-',pi_temp/5

!Program for 1000,000 points
pi_temp=0
do r=1,5
    k=0
    
    do i=1,1000000
        do j = 1,2
        CALL SYSTEM_CLOCK(count,rate,max)
        points3(i,j)=rand() 
        
        end do
    end do

    

    do i=1,1000000
       if ((points3(i,1)**2+points3(i,2)**2)<=1)then
        k=k+1
         end if

    end do
    pi_temp = (k/250000.0)+pi_temp
    
end do
print*,'no. of points out of 1,000,000 inside the quadrant-',k,'average value of pi-',pi_temp/5
print*, 'The value of pi upto 8 decimal places is- 3.14159265'
 
end program



