subroutine co2_interpolate(co2_ndays)
      use climate_module
      use time_module
      implicit none
      integer, intent(in) :: co2_ndays
      integer :: iday = 0
      integer :: irec = 0
      integer :: day1 = 0
      integer :: day2 = 0
      integer :: iyr = 0
      integer :: imo = 0
      integer :: idy = 0
      integer :: yr_start = 0
      integer :: yr_end = 0
      integer :: mo_start = 0
      integer :: mo_end = 0
      real :: co2_1 = 0.
      real :: co2_2 = 0.
      real :: frac = 0.
      real :: co2_sum = 0.
      integer :: co2_count = 0
      real, dimension(:), allocatable :: co2_temp

      !! ─────────────────────────────────────────────────────
      !! STEP 1 — build intermediate daily array from raw data
      !! ─────────────────────────────────────────────────────

      allocate(co2_temp(co2_ndays), source=0.)

      !! case A — daily data: copy directly
      if (co2_data_res == 1) then
        do iday = 1, co2_ndays
          if (iday <= co2_nrec) then
            co2_temp(iday) = co2_val_in(iday)
          else
            co2_temp(iday) = co2_val_in(co2_nrec)
          end if
        end do

      !! case B — monthly or annual: interpolate to daily first
      else
        !! convert year/month/day to simulation day number
        do irec = 1, co2_nrec
          co2_yr_in(irec) = (co2_yr_in(irec) - time%yrc_start) * 365 + &
                            (co2_mo_in(irec) - 1) * 30 + co2_dy_in(irec)
        end do

        !! fill temp array with interpolated daily values
        do iday = 1, co2_ndays
          !! before first record
          if (iday <= co2_yr_in(1)) then
            co2_temp(iday) = co2_val_in(1)
          !! after last record
          else if (iday >= co2_yr_in(co2_nrec)) then
            co2_temp(iday) = co2_val_in(co2_nrec)
          !! between records
          else
            do irec = 1, co2_nrec - 1
              day1 = co2_yr_in(irec)
              day2 = co2_yr_in(irec + 1)
              if (iday >= day1 .and. iday < day2) then
                co2_1 = co2_val_in(irec)
                co2_2 = co2_val_in(irec + 1)
                if (co2_interp == 1) then
                  frac = real(iday - day1) / real(day2 - day1)
                  co2_temp(iday) = co2_1 + frac * (co2_2 - co2_1)
                else
                  co2_temp(iday) = co2_1
                end if
                exit
              end if
            end do
          end if
        end do
      end if

      !! ─────────────────────────────────────────────────────
      !! STEP 2 — apply use_res to fill co2_daily
      !! ─────────────────────────────────────────────────────

      !! use_res=1: daily — use temp directly
      if (co2_use_res == 1) then
        do iday = 1, co2_ndays
          co2_daily(iday) = co2_temp(iday)
        end do

      !! use_res=2: monthly — average temp to monthly, apply same value all days
      else if (co2_use_res == 2) then
        iday = 0
        do iyr = 1, time%nbyr
          do imo = 1, 12
            !! calculate average for this month
            co2_sum   = 0.
            co2_count = 0
            do idy = 1, 30
              iday = (iyr-1)*365 + (imo-1)*30 + idy
              if (iday <= co2_ndays) then
                co2_sum   = co2_sum + co2_temp(iday)
                co2_count = co2_count + 1
              end if
            end do
            !! apply monthly average to all days in month
            if (co2_count > 0) then
              do idy = 1, 30
                iday = (iyr-1)*365 + (imo-1)*30 + idy
                if (iday <= co2_ndays) then
                  co2_daily(iday) = co2_sum / co2_count
                end if
              end do
            end if
          end do
        end do

      !! use_res=3: annual — average records per year, same value all days
      else if (co2_use_res == 3) then
        do iyr = 1, time%nbyr
          !! find records belonging to this calendar year
          !! use original year values stored before conversion
          co2_sum   = 0.
          co2_count = 0
          do irec = 1, co2_nrec
            if (co2_yr_orig(irec) == time%yrc_start + iyr - 1) then
              co2_sum   = co2_sum + co2_val_in(irec)
              co2_count = co2_count + 1
            end if
          end do
          !! DEBUG — write after inner loop
          write(9998,*) iyr, co2_sum, co2_count, co2_sum/max(1,co2_count)
          !! if no records found use mid-year value
          if (co2_count == 0) then
            co2_sum   = co2_temp((iyr-1)*365 + 183)
            co2_count = 1
          end if
          !! apply to ALL days in year
          do iday = (iyr-1)*365 + 1, min(iyr*365, co2_ndays)
            co2_daily(iday) = co2_sum / real(co2_count)
          end do
          !! DEBUG check first and last day of each year
          if (iyr <= 2) then
            write(9998,*) "yr=", iyr, &
              " day1=", co2_daily((iyr-1)*365+1), &
              " day365=", co2_daily(iyr*365)
          end if
        end do

      end if

      !! cleanup
      deallocate(co2_temp)

      return
      end subroutine co2_interpolate