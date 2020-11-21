program sorting
    implicit none
    integer::n,m
    character(len=15)::A(10,3) 
    character(len=15) :: temp(3)  !temporary variable for swapping two rows
    !open and read the file
    open(unit=1,file='student_info.dat') 
    do n=1,10
        read (1,*) (A(n,m),m=1,3) 
    end do 
!it sort the rows by order of marks in decreasing order (highest marks at the top)    
    do n=1,10
        do m=n+1,10
            if(A(n,3)<A(m,3)) then
                temp = A(n,1:)
                A(n,1:) = A(m,1:)
                A(m,1:) = temp
            end if
        end do
    end do
    
    do n=1,10 
        print*, (A(n,m),m=1,3)
    end do
!open a new file to store the sorted file    
    open(unit=2,file='student_info_sorted.dat',status='new')
    write(2,"(*(1x,a6,6x))") 'ROLLNO', 'NAME', 'MARKS'    
    do n=1,10 
        write(2,*) (A(n,m),m=1,3)
    end do

end program sorting
