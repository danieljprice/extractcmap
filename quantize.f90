module quantize
 implicit none

contains

subroutine quantize_colour_table_interactive(ncmax,nc,index,rgb,lrgb)
 use iso_c_binding, only:c_char
 use cmaputils,     only:print_cmap_splash
 use giza
 integer, intent(in)    :: ncmax,nc
 integer, intent(inout) :: index
 real, dimension(3,0:ncmax-1), intent(in)  :: rgb
 real, dimension(4,ncmax),     intent(out) :: lrgb
 integer :: i,i1,ierr
 real :: xpt,ypt,x0,y0
 real, dimension(ncmax) :: larr
 character(kind=c_char) :: ch

 print*,'attempting to quantize colour table... nc = ',index
 do i=1,ncmax
    larr(i) = i-1
 enddo
 call giza_open('/xw')
 call giza_stop_prompting()
!
!--plot red, green and blue arrays in turn
!
 ch = ''
 i1 = 1
 do while(ch /= 'q')
    call giza_set_colour_index(1)
    call giza_set_line_style(1)
    call giza_set_environment(0.,real(nc-1),0.,1.,0,0)
    call giza_set_colour_index(2)
    call giza_line(ncmax,larr,rgb(1,:))
    call giza_set_colour_index(3)
    call giza_line(ncmax,larr,rgb(2,:))
    call giza_set_colour_index(4)
    call giza_line(ncmax,larr,rgb(3,:))

    call giza_set_line_style(2)
    call giza_set_colour_index(2)
    call giza_line(index,lrgb(1,1:index)*(nc-1),lrgb(2,1:index))
    call giza_set_colour_index(3)
    call giza_line(index,lrgb(1,1:index)*(nc-1),lrgb(3,1:index))
    call giza_set_colour_index(4)
    call giza_line(index,lrgb(1,1:index)*(nc-1),lrgb(4,1:index))
    x0 = 0.
    y0 = rgb(1,0)
    ch = ' '
    ierr = giza_band(0,0,x0,y0,xpt,ypt,ch)
    if (ch=='i') then
       call insert_point(xpt,ypt,index,rgb,lrgb,nc)
    elseif (ch=='d' .or. ch=='x') then
       call delete_point(xpt,index,rgb,lrgb,nc)
    endif
    call print_cmap_splash(index,lrgb,2)
    x0 = xpt
    y0 = ypt
 enddo
 call giza_close()
 print "(a,/)",'<<< done >>>'

end subroutine quantize_colour_table_interactive

subroutine insert_point(x,y,n,rgb,lrgb,nc)
 real, intent(inout) :: x,y
 integer, intent(inout) :: n
 real, dimension(:,:), intent(in) :: rgb
 real, dimension(:,:), intent(inout) :: lrgb
 integer, intent(in) :: nc
 integer :: ival,i,j
 real :: lval
 ival = nint(x)
 lval = ival/real(nc)

 !print*,'n=',n,' nodes=',lrgb(1,1:n)
 ! add a point
 j = 1
 do while (lrgb(1,j) < lval)
    j = j + 1
 enddo
 !print "(/,a)",'>>>'
 print*,ival,', l=',lval
 n = n + 1
 do i=n,j+1,-1
    lrgb(:,i) = lrgb(:,i-1)
 enddo
 lrgb(:,j) = (/lval,rgb(:,ival)/)

 x = ival
 y = rgb(1,ival)
 !print*,'n=',n,' nodes=',lrgb(1,1:n)

end subroutine insert_point

subroutine delete_point(x,n,rgb,lrgb,nc)
 real, intent(in) :: x
 integer, intent(inout) :: n
 real, dimension(:,:), intent(in) :: rgb
 real, dimension(:,:), intent(inout) :: lrgb
 integer, intent(in) :: nc
 integer :: i,imin
 real :: dl,dlmin,lval

 if (n <= 3) then
    !--minimum is just end points
    n = 2
    lrgb(:,1) = (/0.,rgb(:,1)/)
    lrgb(:,2) = (/1.,rgb(:,nc)/)
    !print*,'n=',n,' l=',lrgb(1,1),lrgb(1,2)
    return
 endif
 lval = x/(nc-1)
 imin = 256
 dlmin = huge(dlmin)
 do i=1,n
    dl = abs(lrgb(1,i) - lval)
    if (dl < dlmin) then
       dlmin = dl
       imin = i
    endif
 enddo
 !print*,'n=',n,' x=',x,' lval=',lval,' nearest point is ',imin,' l=',lrgb(1,imin)
 do i=imin,n
    lrgb(:,i) = lrgb(:,i+1)
 enddo
 n = n - 1
! print*,'x = ',x,'lrgb=',lrgb(:,1:n)

end subroutine delete_point

end module quantize
