subroutine co2_dyn_read

      use climate_module
      use basin_module
      use time_module

      implicit none

      character(len=200) :: line = ""
      integer :: eof = 0
      integer :: iyr = 0
      integer :: imo = 0
      integer :: idy = 0
      integer :: iday = 0
      integer :: irec = 0
      integer :: co2_ndays = 0
      real :: co2val = 0.
      logical :: i_exist = .false.
      integer :: pass = 0

      !! check if co2.cli exists
      inquire(file="co2.cli", exist=i_exist)

      !! if not found - fall back to original reader
      if (.not. i_exist) then
        call co2_read
        return
      end if

      !! ─────────────────────────────────────────
      !! PASS 1 — read keywords and count records
      !! ─────────────────────────────────────────

      co2_data_res = 2    ! defaults
      co2_use_res  = 1
      co2_interp   = 1
      co2_nrec     = 0

      open(108, file="co2.cli", status="old", action="read")

      do
        read(108, '(a)', iostat=eof) line
        if (eof < 0) exit

        line = adjustl(trim(line))

        !! skip empty lines
        if (len_trim(line) == 0) cycle

        !! skip comment lines
        if (line(1:1) == '!') cycle

        !! read keywords
        if (line(1:8) == 'data_res') then
          read(line(9:), *) co2_data_res
        else if (line(1:7) == 'use_res') then
          read(line(8:), *) co2_use_res
        else if (line(1:6) == 'interp') then
          read(line(7:), *) co2_interp
        else if (line(1:4) == 'year') then
          !! data starts after this line — count records
          do
            read(108, *, iostat=eof) line
            if (eof < 0) exit
            co2_nrec = co2_nrec + 1
          end do
          exit
        end if

      end do

      close(108)

      !! ─────────────────────────────────────────
      !! PASS 2 — allocate and read data
      !! ─────────────────────────────────────────

      allocate(co2_yr_in(co2_nrec))
      allocate(co2_mo_in(co2_nrec))
      allocate(co2_dy_in(co2_nrec))
      allocate(co2_val_in(co2_nrec))

      co2_yr_in = 0
      co2_mo_in = 1
      co2_dy_in = 1
      co2_val_in = 0.

      open(108, file="co2.cli", status="old", action="read")

      !! skip to data section
      do
        read(108, '(a)', iostat=eof) line
        if (eof < 0) exit
        line = adjustl(trim(line))
        if (line(1:4) == 'year') exit
      end do

      !! read data records
      do irec = 1, co2_nrec
        select case (co2_data_res)
          case (3)   ! annual: year  co2
            read(108, *, iostat=eof) iyr, co2val
            imo = 7
            idy = 1
          case (2)   ! monthly: year  month  co2
            read(108, *, iostat=eof) iyr, imo, co2val
            idy = 15
          case (1)   ! daily: year  month  day  co2
            read(108, *, iostat=eof) iyr, imo, idy, co2val
        end select
        if (eof < 0) exit
        co2_yr_in(irec)  = iyr
        co2_mo_in(irec)  = imo
        co2_dy_in(irec)  = idy
        co2_val_in(irec) = co2val
      end do

      close(108)

      !! ─────────────────────────────────────────
      !! PASS 3 — build co2_daily array
      !! ─────────────────────────────────────────

      co2_ndays = time%nbyr * 365
      allocate(co2_daily(co2_ndays), source=0.)

      call co2_interpolate(co2_ndays)

      !! ─────────────────────────────────────────
      !! PASS 4 — fill co2y for backward compat
      !! ─────────────────────────────────────────

      allocate(co2y(time%nbyr), source=0.)
      do iyr = 1, time%nbyr
        iday = (iyr - 1) * 365 + 183
        co2y(iyr) = co2_daily(iday)
      end do

      !! ─────────────────────────────────────────
      !! PASS 5 — write co2.out (always daily)
      !! ─────────────────────────────────────────

      open(2222, file="co2.out")
      write(2222,*) "    YR    MO    DY    CO2(ppm)"
      do iyr = 1, time%nbyr
        do iday = (iyr-1)*365 + 1, iyr*365
          if (iday <= co2_ndays) then
            !! convert simulation day to month and day
            imo = ((iday - (iyr-1)*365) - 1) / 30 + 1
            idy = iday - (iyr-1)*365 - (imo-1)*30
            imo = min(imo, 12)
            write(2222,*) time%yrc_start + iyr - 1, &
                        imo, idy, co2_daily(iday)
          end if
        end do
     end do
     close(2222)

      return
      end subroutine co2_dyn_read