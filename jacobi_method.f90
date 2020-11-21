program jacobi_method
    implicit none
    integer, parameter::kmax=100,n=4
    double precision:: a(n,n), b(n), x(n),soln(n),new_x(n)
    double precision:: sum, tolerance = 0.01, max_error = 100, error,ratio,prev_error
    integer i, j, k
    
    a=reshape((/4,3,2,1,3,4,3,2,2,3,4,3,1,2,3,4/),shape(a)) !given matrix initialization
    b=reshape((/1,1,-1,-1/),shape(b))                       !given RHS 
    new_x=reshape((/0,0,0,0/),shape(new_x))                 !initial guess of the solution
    soln=reshape((/0,1,-1,0/),shape(soln))                  !solution matrix of the equations
    
    
    
write(*,'((1x,a10)(7x,a10)(25x,a2)2(20x,a10)(10x,a20)(5x,a10))')'Iteration','x1','x2','x3','x4','ERROR(||x-x^(k)||)','RATIO'
do k=1,kmax
    
    
    if(max_error < tolerance) then                      !to make error (||x-x^(k)||) less than tolerance
        print*,
        print*,'no. of iterations-',k-1
        exit
    end if

    write(*,'(2x,i0,3x,4(10x,e19.13))',advance='no') k,x
    max_error = -1                                      !initialising error as any negative number to compare with error of each variable
    
    do i=1,n
        x(i)=new_x(i)
    end do

! Jacobi method
        do i=1,n
            sum = 0
            do j=1,n
                if(i/=j) then
                    sum = sum + a(i,j) * x(j)
                end if
            end do
            new_x(i) = (b(i) - sum)/a(i,i)
            ! print*,x(i)
            error = abs(x(i) - soln(i))                     !calculating error in each variable
            if(max_error < error) then                      
                max_error = error                           !calculating max error 
            end if
        end do
       

    if (k>1)then
        ratio=max_error/prev_error
        write(*,'(5x,e19.13)',advance='no') max_error
        write(*,'(5x,e19.13)') ratio
    else
        write(*,'(5x,e19.13)') max_error
    end if
    prev_error=max_error

 end do
    print*,'solution matrix'
    do i=1,n
        print*,x(i)
    end do
    
     
    
    end program
    