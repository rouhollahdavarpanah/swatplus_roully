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
      open(9999, file="co2_debug.txt")
      write(9999,*) "co2_dyn_read called"
      write(9999,*) "co2.cli exists=", i_exist

      !! if not found - fall back to original reader
      if (.not. i_exist) then
        call co2_read
        return
      end if

      !! open co2.cli
      open(108, file="co2.cli", status="old", action="read")
      write(9999,*) "file opened"

      !! read header lines
      read(108, '(a)', iostat=eof) titldum
      write(9999,*) "title read:", trim(titldum)
      read(108, *, iostat=eof) res_str, co2_nrec, co2_interp
      write(9999,*) "res_str=", trim(res_str), " nrec=", co2_nrec
      write(*,*) "DEBUG res_str=", trim(res_str), " nrec=", co2_nrec
      read(108, '(a)', iostat=eof) header
      write(9999,*) "header read:", trim(header)
      
      close(9999)

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
      open(9999, file="co2_debug.txt", position="append")
      write(9999,*) "records read successfully"
      write(9999,*) "first record:", co2_yr_in(1), co2_mo_in(1), co2_val_in(1)
      write(9999,*) "last record:", co2_yr_in(co2_nrec), co2_mo_in(co2_nrec), co2_val_in(co2_nrec)
      close(9999)

      !! allocate daily array
      co2_ndays = time%nbyr * 365
      allocate(co2_daily(co2_ndays), source=0.)

      !! fill daily array using linear interpolation
      call co2_interpolate(co2_ndays)
      open(9999, file="co2_debug.txt", position="append")
      write(9999,*) "co2_ndays=", co2_ndays
      write(9999,*) "co2_daily allocated=", allocated(co2_daily)
      write(9999,*) "co2_daily(1)=", co2_daily(1)
      write(9999,*) "co2_daily(183)=", co2_daily(183)
      write(9999,*) "co2_daily(co2_ndays)=", co2_daily(co2_ndays)
      close(9999)
      
      !! also fill co2y for backward compatibility
      allocate(co2y(time%nbyr), source=0.)
      do iyr = 1, time%nbyr
        iday = (iyr - 1) * 365 + 183
        co2y(iyr) = co2_daily(iday)
      end do
    
      !! write co2.out for verification
      open(2222, file="co2.out")
      write(2222,*) "    YR    MO    DY    CO2(ppm)"
      iday = 0
      do iyr = 1, time%nbyr
        do imo = 1, 12
          do idy = 1, 30
            iday = iday + 1
            if (iday <= co2_ndays) then
              write(2222,*) time%yrc_start + iyr - 1, imo, idy, co2_daily(iday)
            end if
          end do
         end do
      end do
      close(2222)
      
      return
      end subroutine co2_dyn_read