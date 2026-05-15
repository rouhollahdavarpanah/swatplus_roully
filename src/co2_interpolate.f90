subroutine co2_interpolate(co2_ndays)

      use climate_module
      use time_module

      implicit none

      integer, intent(in) :: co2_ndays
      integer :: iday = 0
      integer :: irec = 0
      integer :: day1 = 0
      integer :: day2 = 0
      real :: co2_1 = 0.
      real :: co2_2 = 0.
      real :: frac = 0.

      !! convert year/month/day of each record to simulation day number
      do irec = 1, co2_nrec
        !! simple conversion: simulation day = (year - start_year)*365 + day_of_year
        co2_yr_in(irec) = (co2_yr_in(irec) - time%yrc_start) * 365 + &
                          (co2_mo_in(irec) - 1) * 30 + co2_dy_in(irec)
      end do

      !! fill co2_daily using linear interpolation between records
      do iday = 1, co2_ndays

        !! before first record
        if (iday <= co2_yr_in(1)) then
          co2_daily(iday) = co2_val_in(1)

        !! after last record
        else if (iday >= co2_yr_in(co2_nrec)) then
          co2_daily(iday) = co2_val_in(co2_nrec)

        !! between records
        else
          do irec = 1, co2_nrec - 1
            day1 = co2_yr_in(irec)
            day2 = co2_yr_in(irec + 1)
            if (iday >= day1 .and. iday < day2) then
              co2_1 = co2_val_in(irec)
              co2_2 = co2_val_in(irec + 1)
              if (co2_interp == 1) then
                !! linear interpolation
                frac = real(iday - day1) / real(day2 - day1)
                co2_daily(iday) = co2_1 + frac * (co2_2 - co2_1)
              else
                !! step function
                co2_daily(iday) = co2_1
              end if
              exit
            end if
          end do
        end if

      end do

      return
      end subroutine co2_interpolate