!
! This program extracts the colour table from a plain format pnm file
! as produced by netpbm tools using e.g. pnmcolormap
!
! output format is a table in brightness,red,green,blue
! showing only the changes in colour gradients
! this can be used to call e.g. PGCTAB in PGPLOT
!
program extract_colourmap
 use ppmutils,  only:read_plainppm
 use cmaputils, only:write_colourmap,quantize_colour_table,print_cmap_splash
 use quantize,  only:quantize_colour_table_interactive
 implicit none
 character(len=120) :: filename
 integer :: npixx,npixy,ncolors,i,index,ncoloursfile
 integer, parameter :: maxpix = 4096
 integer, dimension(3,0:maxpix) :: icolour
 real,    dimension(3,0:maxpix) :: rcolour
 integer, parameter :: ncmax=256
 real, dimension(4,ncmax) :: lrgb
 real, dimension(3,ncmax) :: rgb
 real :: frac,fracprev,dx
 integer :: ierr,n,m,ipt,nsample
 integer, parameter :: ninterp = 7
 real, dimension(ninterp), parameter :: ptarray = (/0.,0.125,0.25,0.392,0.5,0.75,1.0/)
 real, dimension(3) :: drgb
 logical, parameter :: use_preset_table = .false., interactive = .true.
 integer, parameter :: max_table = 32 !5 ! maximum allowable interpolation points

 call getarg(1,filename)
 if (len_trim(filename).eq.0) stop 'Usage: readppm ppmfile'

 call read_plainppm(filename,npixx,npixy,ncoloursfile,icolour,ierr)
 if (ierr /= 0) stop 'error reading ppm file'

 ! normalise colours to 0->1
 rcolour = icolour/real(ncoloursfile)

 ncolors = npixx-1
 call write_colourmap('colours.orig',ncolors,rcolour)

 ! resample to 256 colours
 nsample = 240
 if (ncolors > nsample) then
    do i=1,nsample
       frac = (i-1)/real(nsample-1)
       ipt = int(frac*ncolors)
       print*,i,frac,'->',ipt,(ipt-1)/real(ncolors)
       if (ipt+1 > npixx-1) then
          rgb(:,i) = rcolour(:,ipt)
       else
          !dx = rcolour(ipt+1)
          rgb(:,i) = rcolour(:,ipt) !+ dx*rcolour(:,ipt+1)
       endif
    enddo
    ncolors = nsample
    rcolour(:,0:ncmax-1) = rgb
 endif

 call write_colourmap('colours.256',ncolors,rcolour)

 print*,' writing colour map to colours.gp'
 open(3,file='colours.gp',status='replace')
 write(3,*) 'ncolors = ',ncmax
 write(3,*) 'ntsc = 1'
 do i=1,ncolors
    write(3,*) int(rcolour(:,i-1)*255.)
 enddo
 close(3)

 if (use_preset_table) then
    fracprev = 0.
    m = 0
    do i=0,ncolors
   !  hls(:) = rgbtohls(icolour(:,i))
   !  print*,
     frac = i/real(npixx-1)
     do n=1,ninterp
        if (frac.ge.ptarray(n) .and. fracprev.lt.ptarray(n) .or. i.eq.0) then
           dx = ptarray(n) - fracprev
           if (i.gt.0) then
              m = m + 1
              drgb(:) = (icolour(:,i) - icolour(:,i-1))/real(ncolors)
              print "(4(1x,f8.3))",ptarray(n),(icolour(:,i-1) + dx*drgb(:))/255.
              lrgb(1,m) = ptarray(n)
              lrgb(2:4,m) = (icolour(:,i-1) + dx*drgb(:))/255.
           elseif (n.eq.1) then
              m = m + 1
              drgb(:) = 0.
              print "(4(1x,f8.3))",ptarray(1),icolour(:,0)/255.
              lrgb(1,m) = ptarray(1)
              lrgb(2:4,m) = icolour(:,0)/255.
           endif
        endif
     enddo
     fracprev = frac
    enddo
    index = m
 else
    if (interactive) then
       call quantize_colour_table(12,ncmax,ncolors,index,rcolour,lrgb,.false.)
       call quantize_colour_table_interactive(ncmax,ncolors,index,rcolour,lrgb)
    else
       call quantize_colour_table(max_table,ncmax,ncolors,index,rcolour,lrgb,.true.)
    endif
 endif

! print*,'removing identical colour values'
! index = 0
! do i=1,npixx-1
!    index = index + 1
!    if (ALL(icolour(:,i).eq.icolour(:,i-1))) then
!       index = index - 1
!       ncolors = ncolors - 1
!       icolour(:,index) = icolour(:,i)
!       !print*,'copying ',i,' to ',index
!    endif
! enddo
! print*,'image contains ',ncolors+1,' unique colours in first row'
! do i=0,ncolors
!    print*,icolour(:,i)
! enddo
!
!--print again in splash code format
!
 call print_cmap_splash(index,lrgb)
!
!--print again in splash code format with two decimal places
!
 call print_cmap_splash(index,lrgb,2)

 print*,' writing quantized colour map to colours.fit'
 open(3,file='colours.fit',status='replace')
 write(3,*) '#   lum    red    green    blue '
 do i=1,index
    write(3,*) lrgb(:,i)
 enddo
 close(3)

end program extract_colourmap
