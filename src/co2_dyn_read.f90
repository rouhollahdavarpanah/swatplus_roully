subroutine co2_dyn_read

      use climate_module
      use basin_module
      use time_module

      implicit none

      character(len=80) :: titldum = ""
      character(len=80) :: header = ""
      character(len=10) :: res_str = ""
      integer :: eof = 0
      integer :: irec = 0
      integer :: iyr = 0
      integer :: imo = 0
      integer :: idy = 0
      integer :: iday = 0
      integer :: co2_ndays = 0
      real :: co2val = 0.
      logical :: i_exist = .false.

      !! check if new dynamic file exists
      inquire(file="co2.cli", exist=i_exist)

      !! if not found - fall back to original reader
      if (.not. i_exist) then
        call co2_read
        return
      end if

      !! open co2.cli
      open(108, file="co2.cli", status="old", action="read")

      !! read header lines
      read(108, *, iostat=eof) titldum
      read(108, *, iostat=eof) res_str, co2_nrec, co2_interp
      read(108, *, iostat=eof) header

      !! allocate records
      allocate(co2_yr_in(co2_nrec))
      allocate(co2_mo_in(co2_nrec))
      allocate(co2_dy_in(co2_nrec))
      allocate(co2_val_in(co2_nrec))

      !! read records based on resolution
      do irec = 1, co2_nrec
        select case (trim(res_str))
          case ("annual")
            read(108,*,iostat=eof) iyr, co2val
            imo = 7
            idy = 1
          case ("monthly")
            read(108,*,iostat=eof) iyr, imo, co2val
            idy = 15
          case ("daily")
            read(108,*,iostat=eof) iyr, imo, idy, co2val
        end select
        co2_yr_in(irec) = iyr
        co2_mo_in(irec) = imo
        co2_dy_in(irec) = idy
        co2_val_in(irec) = co2val
      end do

      close(108)

      !! allocate daily array
      co2_ndays = time%nbyr * 365
      allocate(co2_daily(co2_ndays), source=0.)

      !! fill daily array using linear interpolation
      call co2_interpolate(co2_ndays)

      !! also fill co2y for backward compatibility
      allocate(co2y(time%nbyr), source=0.)
      do iyr = 1, time%nbyr
        iday = (iyr - 1) * 365 + 183
        co2y(iyr) = co2_daily(iday)
      end do
    
      !! write co2.out for verification
      open (2222, file="co2.out")
      write (2222,*) "         YR    CO2(ppm)"
      do iyr = 1, time%nbyr
        write (2222,*) time%yrc_start + iyr - 1, co2y(iyr)
      end do
      close (2222)
      
      return
      end subroutine co2_dyn_read