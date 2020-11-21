PROGRAM random_number_generator
  IMPLICIT NONE
  INTEGER :: count,rate,max,j
  REAL :: real_numbers(5)
  INTEGER :: integers(5)
  real_numbers = [10,10,10,10,10]     !giving any random initial values to this array(which is changed afterwards hence doesnot matter)
  integers = [14,155,107,10,10]       !giving any random initial values to this array(which is changed afterwards hence doesnot matter)
  CALL SYSTEM_CLOCK(count,rate,max)
  CALL srand(count) 
  do j = 1,5
    CALL SYSTEM_CLOCK(count,rate,max)
    real_numbers(j)=1+9*rand()        !scales number to make it a rational from 1 to 10
  end do
  print*, "5 random rational numbers from 1 to 10:"
  print*,real_numbers
  do j = 1,5
    CALL SYSTEM_CLOCK(count,rate,max)
    integers(j)=100+int(9900*rand())  !scales number to make it a natural no. from 100 to 10000, by rounding off to nearest integer
  end do
print*, "5 random natural numbers from 100 to 10,000:"
print*, integers
END PROGRAM 

