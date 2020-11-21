program operation_on_vectors
    implicit none
real::read_file(4,4)=0
real::a(4,2),b(2,2),c(4,4)=0,sum
integer::i,j,k

open (unit=1,file='vector.dat') !To read 4 vectors

!POPULATING 4X4 MATRIX FROM 4X2
do i=1,4
    read (1,*) (read_file(i,j+2),j=1,2) 
end do 

open(unit=2,file='initial_vector.dat',status='new') !TODO 
  do i=1,4                         
    write(2,*) (read_file(i,j),j=1,4)  !WRITING 4X4 INITIAL VECTORS TO PLOT
  end do
close(1)
close(2)

!CONVERTING 4X4 MATRIX INTO 4X2 FOR OPERATION
do i=1,4                         
  do j=3,4
    a(i,j-2) = read_file(i,j)
  end do
end do

!+90 operation 

!RESHAPE IS USED TO ASSIGN MULTIDIMENSIONAL ARRAY 
b=reshape((/0,-1,1,0/),shape(b))

do i=1,4
  do j=1,2
    sum=0
    do k=1,2
      sum=sum+a(i,k)*b(k,j)
    end do
    c(i,j+2)=sum
  end do
end do

open(unit=3,file='after_+90.dat',status='old')
  do i=1,4                         
    write(3,*) (c(i,j),j=1,4)
  end do
close(3)

call system('gnuplot -c plot_graph.plt "+90 OPERATION ON VECTORS" +90_OPERATION.png after_+90.dat')
 
!-90 operation 

!RESHAPE IS USED TO ASSIGN MULTIDIMENSIONAL ARRAY 
b=reshape((/0,1,-1,0/),shape(b))

do i=1,4
  do j=1,2
    sum=0
    do k=1,2
      sum=sum+a(i,k)*b(k,j)
    end do
    c(i,j+2)=sum
  end do
end do

open(unit=4,file='after_-90.dat',status='old')
  do i=1,4                         
    write(4,*) (c(i,j),j=1,4)
  end do
close(4)

call system('gnuplot -c plot_graph.plt "-90 OPERATION ON VECTORS" -90_OPERATION.png after_-90.dat')

!reflection on the line y=x operation 

!RESHAPE IS USED TO ASSIGN MULTIDIMENSIONAL ARRAY 
b=reshape((/0,1,1,0/),shape(b))

do i=1,4
  do j=1,2
    sum=0
    do k=1,2
      sum=sum+a(i,k)*b(k,j)
    end do
    c(i,j+2)=sum
  end do
end do

open(unit=5,file='REFLECTION.dat',status='old')
  do i=1,4                         
    write(5,*) (c(i,j),j=1,4)
  end do
close(5)

call system('gnuplot -c plot_graph.plt "REFLECTION ON Y=X OPERATION ON VECTORS" REFLECTION_OPERATION.png REFLECTION.dat')

!stretching by a factor of 2 operation 

!RESHAPE IS USED TO ASSIGN MULTIDIMENSIONAL ARRAY 
b=reshape((/2,0,0,2/),shape(b))

do i=1,4
  do j=1,2
    sum=0
    do k=1,2
      sum=sum+a(i,k)*b(k,j)
    end do
    c(i,j+2)=sum
  end do
end do

open(unit=6,file='stretching.dat',status='old')
  do i=1,4                         
    write(6,*) (c(i,j),j=1,4)
  end do
close(6)

call system('gnuplot -c plot_graph.plt "STRETCHING BY FACTOR OF 2 OPERATION" stretching.png stretching.dat')

end program 