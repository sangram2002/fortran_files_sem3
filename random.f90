program random_number_generator
  REAL :: arr(2,5)
  CALL RANDOM_NUMBER(r)                    !generates 10 random reals b/w 0 and 1

  DO j = 1,5
    call random_number(arr(1,j))
    arr(1,j) = arr(1,j)*9+1                !scales number to make it a rational from 1 to 10
END DO
write(*,*) '5 random rational numbers between 1 to 10 :'
print*, arr(1,:5)
DO j = 1,5
    call random_number(arr(2,j))
    arr(2,j) = int((arr(2,j)*99+1)*100)    !scales number to make it a natural no. from 100 to 10000, by rounding off to nearest integer
END DO
write(*,*) '5 random natural numbers  between 100 to 10,000 :'
print*, arr(2,:5)
end program