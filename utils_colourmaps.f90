!--------------------------------------------------------------
!+
!  utilities related to ripping off colour maps from ppm files
!+
!--------------------------------------------------------------
module cmaputils
 implicit none
 public :: rgbtohls,write_colourmap,quantize_colour_table
 public :: print_cmap_splash

 private

contains

function rgbtohls(rgb)
 integer, dimension(3), intent(in) :: rgb
 real,    dimension(3) :: rgbtohls
 integer :: M, ml
 integer :: c,r,g,b
 real :: hdash,h,l,s

 r = rgb(1)
 g = rgb(2)
 b = rgb(3)
 M = maxval(rgb)
 ml = minval(rgb)
 c = M - ml
 if (c.le.0) then
    hdash = 0
 elseif (M.eq.r) then
    hdash = mod((g-b)/real(c),6.)
 elseif (M.eq.g) then
    hdash = (b-r)/real(c) + 2.
 else
    hdash = (r-g)/real(c) + 4.
 endif
 h = 60.*hdash
 l = ((M + ml)/2.)/255.
 if (c.le.0) then
    s = 0
 else
    s = c/(1. - abs(2.*l - 1.))/255.
 endif

! print*,'c = ',c,',hdash = ',hdash,'2l - 1 = ',1 - abs(2*l - 1),'hls = ',h,l,s
 rgbtohls = (/h,l,s/)

end function rgbtohls

subroutine write_colourmap(filename,nc,rgb)
 character(len=*), intent(in) :: filename
 integer, intent(in) :: nc
 real, dimension(3,0:nc-1), intent(in) :: rgb
 integer :: iunit,i

 print*,' writing colour map to '//trim(filename)
 open(newunit=iunit,file=filename,status='replace')
 write(iunit,*) '#   lum    red    green    blue'
 do i=1,nc
    write(iunit,*) (i-1)/real(nc),rgb(:,i-1)
 enddo
 close(iunit)

end subroutine write_colourmap

subroutine quantize_colour_table(max_table,ncmax,nc,index,rgb,lrgb,interactive)
 integer, intent(in) :: max_table,ncmax,nc
 integer, intent(out) :: index
 real, dimension(3,0:ncmax-1), intent(in)  :: rgb
 real, dimension(4,ncmax),     intent(out) :: lrgb
 logical, intent(in) :: interactive
 integer :: i,iprev,nsmooth,ifac
 real :: dx,grad_thresh
 real, dimension(3) :: gradcolour,gradprev,grad2

 print*,'attempting to quantize colour table...'
!
!--print in a tabulated format
!
 index = max_table + 1
 nsmooth = 3 !int((npixx-1)/255.)
 ifac = 3
 grad_thresh = 0.15
 do while (index > max_table)
 print "('lum  r    g    b')"
 gradprev = 0
 index = 1
 i = 1
 iprev = i
 lrgb(:,index) = (/(i-1)/real(nc),rgb(:,i-1)/)
 print 10,(i-1)/real(nc),rgb(:,i-1)

 dx = nsmooth/real(nc)
 open(unit=3,file='gradient.out',status='replace')
 do i=nsmooth,nc
     gradcolour(:) = (rgb(:,i) - rgb(:,i-nsmooth))/(2.*dx)
     if (i > ifac*nsmooth) gradprev(:) = (rgb(:,i) - rgb(:,i-ifac*nsmooth))/(2.*dx*ifac)
     grad2(:)      = (rgb(:,i) - 2.*rgb(:,i-nsmooth/2) + rgb(:,i-nsmooth))/(dx**2)
     write(3,*) i/real(nc),gradcolour,grad2
     !print*,i,' colour = ',rcolour(:,i)
     !print*,i,' gradient = ',gradcolour
     !print*,i,' second deriv = ',grad2
     if (ANY(ABS(gradcolour(:) - gradprev(:)).gt.grad_thresh).and.i.gt.iprev+nsmooth) then
        print 10,(i-1)/real(nc),rgb(:,i-1)
        index = index + 1
        iprev = i
        !print*,'index ',index,' i = ',i-1,rcolour(:,i-1),'gradient = ',gradcolour(:)
        if (index < size(lrgb(1,:))) then
           lrgb(:,index) = (/(i-1)/real(nc),rgb(:,i-1)/)
        endif
     endif
     !read*
     gradprev = gradcolour
 enddo
 close(3)

 index = index + 1
 if (index < size(lrgb(1,:))) lrgb(:,index) = (/1.0,rgb(:,nc)/)

 print 10, 1.0,rgb(:,nc)

 if (index > max_table) then
    grad_thresh = grad_thresh*1.05
    print*,index,' too many colours: increasing threshold to ',grad_thresh
    if (interactive) then
       print*,'<press enter to continue>'
       read*
    endif
 endif

 enddo

10 format (4(f5.3,1x))

end subroutine quantize_colour_table

subroutine print_cmap_splash(nc,lrgb,format)
 integer, intent(in) :: nc
 real, dimension(4,nc), intent(in) :: lrgb
 integer, intent(in), optional :: format
 integer :: iformat
 character(len=64) :: fmtstring

 iformat = 0
 if (present(format)) iformat = format

 print "(a,i2)",'nset = ',nc
 if (iformat==2) then
    write(fmtstring,"(a,i2,a)") '(''lumarr(1:nset)  = (/'',',nc-1,'(f4.2,'',''),f4.2''/)'')'
    print fmtstring,lrgb(1,1:nc)
    write(fmtstring,"(a,i2,a)") '(''redarr(1:nset)  = (/'',',nc-1,'(f4.2,'',''),f4.2''/)'')'
    print fmtstring,lrgb(2,1:nc)
    write(fmtstring,"(a,i2,a)") '(''greenarr(1:nset)= (/'',',nc-1,'(f4.2,'',''),f4.2''/)'')'
    print fmtstring,lrgb(3,1:nc)
    write(fmtstring,"(a,i2,a)") '(''bluearr(1:nset) = (/'',',nc-1,'(f4.2,'',''),f4.2''/)'')'
    print fmtstring,lrgb(4,1:nc)
 else
    fmtstring="(10(f5.3,','),'&',5(/,20x,10(f5.3,','),'&'),'/)')"
    write(*,"(a)",advance='no') 'lumarr(1:nset)  = (/'
    print fmtstring,lrgb(1,1:nc)
    write(*,"(a)",advance='no') 'readarr(1:nset) = (/'
    print fmtstring,lrgb(2,1:nc)
    write(*,"(a)",advance='no') 'greenarr(1:nset)= (/'
    print fmtstring,lrgb(3,1:nc)
    write(*,"(a)",advance='no') 'bluearr(1:nset) = (/'
    print fmtstring,lrgb(4,1:nc)
 endif

end subroutine print_cmap_splash

end module cmaputils
