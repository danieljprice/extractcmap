!--------------------------------------------------------------
!+
!  utilities related to reading/writing ppm files
!+
!--------------------------------------------------------------
module ppmutils
 implicit none
 public :: read_plainppm

 private

contains

subroutine read_plainppm(filename,nx,ny,nc,icolour,ierr)
 character(len=*), intent(in) :: filename
 integer, intent(out) :: nx,ny,nc,ierr
 integer, dimension(:,:), intent(out) :: icolour
 character(len=2)   :: ptype
 character(len=120) :: line
 integer :: iunit,i

 open(newunit=iunit,file=filename,status='old',form='formatted',iostat=ierr)
 if (ierr /= 0) return

 read(iunit,*,iostat=ierr) ptype
 if (ptype(1:1).ne.'P') then
    print*,trim(filename),' is not pnm format -> convert first using pbm tools'
    ierr = 2
    return
 elseif (ptype.ne.'P3') then
    print*,trim(filename),' is not plain pnm -> convert first using pnmtoplainpnm'
    ierr = 3
    return
 endif
 print*,'-> reading ',trim(filename)
!--skip comment lines
 line = '#'
 do while (line(1:1).eq.'#')
    read(iunit,"(a)") line
    print*,trim(line)
 enddo
 read(line,*) nx,ny
 print*,'image size is ',nx,'x',ny
 read(iunit,*) nc
 print*,'image contains ',nc+1,' colours'
 read(iunit,*,iostat=ierr) (icolour(:,i),i=1,nx)
 close(iunit)

end subroutine read_plainppm

end module ppmutils
