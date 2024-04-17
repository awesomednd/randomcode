program parking
        implicit none

        character(len=30), dimension(20) :: day1a, day1b, day1c
        character(len=30), dimension(20) :: day2a, day2b, day2c
        character(len=30), dimension(20) :: day3a, day3b, day3c
        character(len=30), dimension(20) :: day4a, day4b, day4c
        character(len=30), dimension(20) :: day5a, day5b, day5c
        character(len=30), dimension(20) :: day6a, day6b, day6c
        character(len=30), dimension(20) :: day7a, day7b, day7c
        character(len=30), dimension(20) :: day8a, day8b, day8c
        character(len=30), dimension(20) :: day9a, day9b, day9c
        character(len=30), dimension(20) :: day10a, day10b, day10c
        character(len=30), dimension(20) :: day11a, day11b, day11c
        character(len=30), dimension(20) :: day12a, day12b, day12c
        character(len=30), dimension(20) :: day13a, day13b, day13c
        character(len=30), dimension(20) :: day14a, day14b, day14c
        logical :: isrunning
        integer :: option1
        integer :: dayslected1
        integer :: optionbooktype
        integer :: dayslected
        integer :: lottobook
        integer :: statsoption
        integer :: daystatslected
        integer :: totalnormalparkings
        integer :: totaldisabledparkings
        integer :: loopy1
        integer :: loopy2
        integer :: loopy3
        integer :: loopy4
        integer :: loopy5
        integer :: loopy6
        integer :: loopy7
        integer :: loopy8
        integer :: loopy9
        integer :: loopy10
        integer :: loopy11
        integer :: loopy12
        integer :: loopy13
        integer :: loopy14
        character(len=30) :: enteredname, enteredreg

        lottobook = 0
        totalnormalparkings = 0
        totaldisabledparkings = 0

        day1a = ''
        day1b = ''
        day1c = ''
        day2a = ''
        day2b = ''
        day2c = ''
        day3a = ''
        day3b = ''
        day3c = ''
        day4a = ''
        day4b = ''
        day4c = ''
        day5a = ''
        day5b = ''
        day5c = ''
        day6a = ''
        day6b = ''
        day6c = ''
        day7a = ''
        day7b = ''
        day7c = ''
        day8a = ''
        day8b = ''
        day8c = ''
        day9a = ''
        day9b = ''
        day9c = ''
        day10a = ''
        day10b = ''
        day10c = ''
        day11a = ''
        day11b = ''
        day11c = ''
        day12a = ''
        day12b = ''
        day12c = ''
        day13a = ''
        day13b = ''
        day13c = ''
        day14a = ''
        day14b = ''
        day14c = ''

        isrunning = .true.

        do while (isrunning .eqv. .true.)
                print *, 'Please select an option'
                print *, 'Option 1 | View Parking'
                print *, 'Option 2 | Book Parking'
                print *, 'Optiom 3 | Statistics'
                read(*,*) option1
                if (option1 == 1) then
                        print *, 'Please select the day you want to view'
                        read(*,*) dayslected1
                        if (dayslected1 == 1) then
                                call parkingprinter(day1a, day1b, day1c)
                        else if (dayslected1 == 2) then
                                call parkingprinter(day2a, day2b, day2c)
                        else if (dayslected1 == 3) then
                                call parkingprinter(day3a, day3b, day3c)
                        else if (dayslected1 == 4) then
                                call parkingprinter(day4a, day4b, day4c)
                        else if (dayslected1 == 5) then
                                call parkingprinter(day5a, day5b, day5c)
                        else if (dayslected1 == 6) then
                                call parkingprinter(day6a, day6b, day6c)
                        else if (dayslected1 == 7) then
                                call parkingprinter(day7a, day7b, day7c)
                        else if (dayslected1 == 8) then
                                call parkingprinter(day8a, day8b, day8c)
                        else if (dayslected1 == 9) then
                                call parkingprinter(day9a, day9b, day9c)
                        else if (dayslected1 == 10) then
                                call parkingprinter(day10a, day10b, day10c)
                        else if (dayslected1 == 11) then
                                call parkingprinter(day11a, day11b, day11c)
                        else if (dayslected1 == 12) then
                                call parkingprinter(day12a, day12b, day12c)
                        else if (dayslected1 == 13) then
                                call parkingprinter(day13a, day13b, day13c)
                        else if (dayslected1 == 14) then
                                call parkingprinter(day14a, day14b, day14c)
                        end if
                else if (option1 == 2) then
                        print *, 'Please select if it is a normal or Disabled booking'
                        print *, 'Option 1 | Normal'
                        print *, 'Option 2 | Disabled'
                        read(*,*) optionbooktype
                        if (optionbooktype == 1) then
                                print *, 'Please select a day in the 14 day period'
                                read(*,*) dayslected
                                if (dayslected == 1) then
                                        if (day1a(20) == '') then
                                                lottobook = 20
                                        else if (day1a(19) == '') then
                                                lottobook = 19
                                        else if (day1a(18) == '') then
                                                lottobook = 18
                                        else if (day1a(17) == '') then
                                                lottobook = 17
                                        else if (day1a(16) == '') then
                                                lottobook = 16
                                        else if (day1a(15) == '') then
                                                lottobook = 15
                                        else if (day1a(14) == '') then
                                                lottobook = 14
                                        else if (day1a(13) == '') then
                                                lottobook = 13
                                        else if (day1a(12) == '') then
                                                lottobook = 12
                                        else if (day1a(11) == '') then
                                                lottobook = 11
                                        else if (day1a(10) == '') then
                                                lottobook = 10
                                        else if (day1a(9) == '') then
                                                lottobook = 9
                                        else if (day1a(8) == '') then
                                                lottobook = 8
                                        else if (day1a(7) == '') then
                                                lottobook = 7
                                        else if (day1a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day1a(lottobook) = '1'
                                                day1b(lottobook) = enteredreg
                                                day1c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 2) then
                                        if (day2a(20) == '') then
                                                lottobook = 20
                                        else if (day2a(19) == '') then
                                                lottobook = 19
                                        else if (day2a(18) == '') then
                                                lottobook = 18
                                        else if (day2a(17) == '') then
                                                lottobook = 17
                                        else if (day2a(16) == '') then
                                                lottobook = 16
                                        else if (day2a(15) == '') then
                                                lottobook = 15
                                        else if (day2a(14) == '') then
                                                lottobook = 14
                                        else if (day2a(13) == '') then
                                                lottobook = 13
                                        else if (day2a(12) == '') then
                                                lottobook = 12
                                        else if (day2a(11) == '') then
                                                lottobook = 11
                                        else if (day2a(10) == '') then
                                                lottobook = 10
                                        else if (day2a(9) == '') then
                                                lottobook = 9
                                        else if (day2a(8) == '') then
                                                lottobook = 8
                                        else if (day2a(7) == '') then
                                                lottobook = 7
                                        else if (day2a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day2a(lottobook) = '1'
                                                day2b(lottobook) = enteredreg
                                                day2c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 3) then
                                        if (day3a(20) == '') then
                                                lottobook = 20
                                        else if (day3a(19) == '') then
                                                lottobook = 19
                                        else if (day3a(18) == '') then
                                                lottobook = 18
                                        else if (day3a(17) == '') then
                                                lottobook = 17
                                        else if (day3a(16) == '') then
                                                lottobook = 16
                                        else if (day3a(15) == '') then
                                                lottobook = 15
                                        else if (day3a(14) == '') then
                                                lottobook = 14
                                        else if (day3a(13) == '') then
                                                lottobook = 13
                                        else if (day3a(12) == '') then
                                                lottobook = 12
                                        else if (day3a(11) == '') then
                                                lottobook = 11
                                        else if (day3a(10) == '') then
                                                lottobook = 10
                                        else if (day3a(9) == '') then
                                                lottobook = 9
                                        else if (day3a(8) == '') then
                                                lottobook = 8
                                        else if (day3a(7) == '') then
                                                lottobook = 7
                                        else if (day3a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day3a(lottobook) = '1'
                                                day3b(lottobook) = enteredreg
                                                day3c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 4) then
                                        if (day4a(20) == '') then
                                                lottobook = 20
                                        else if (day4a(19) == '') then
                                                lottobook = 19
                                        else if (day4a(18) == '') then
                                                lottobook = 18
                                        else if (day4a(17) == '') then
                                                lottobook = 17
                                        else if (day4a(16) == '') then
                                                lottobook = 16
                                        else if (day4a(15) == '') then
                                                lottobook = 15
                                        else if (day4a(14) == '') then
                                                lottobook = 14
                                        else if (day4a(13) == '') then
                                                lottobook = 13
                                        else if (day4a(12) == '') then
                                                lottobook = 12
                                        else if (day4a(11) == '') then
                                                lottobook = 11
                                        else if (day4a(10) == '') then
                                                lottobook = 10
                                        else if (day4a(9) == '') then
                                                lottobook = 9
                                        else if (day4a(8) == '') then
                                                lottobook = 8
                                        else if (day4a(7) == '') then
                                                lottobook = 7
                                        else if (day4a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day4a(lottobook) = '1'
                                                day4b(lottobook) = enteredreg
                                                day4c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 5) then
                                        if (day5a(20) == '') then
                                                lottobook = 20
                                        else if (day5a(19) == '') then
                                                lottobook = 19
                                        else if (day5a(18) == '') then
                                                lottobook = 18
                                        else if (day5a(17) == '') then
                                                lottobook = 17
                                        else if (day5a(16) == '') then
                                                lottobook = 16
                                        else if (day5a(15) == '') then
                                                lottobook = 15
                                        else if (day5a(14) == '') then
                                                lottobook = 14
                                        else if (day5a(13) == '') then
                                                lottobook = 13
                                        else if (day5a(12) == '') then
                                                lottobook = 12
                                        else if (day5a(11) == '') then
                                                lottobook = 11
                                        else if (day5a(10) == '') then
                                                lottobook = 10
                                        else if (day5a(9) == '') then
                                                lottobook = 9
                                        else if (day5a(8) == '') then
                                                lottobook = 8
                                        else if (day5a(7) == '') then
                                                lottobook = 7
                                        else if (day5a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day5a(lottobook) = '1'
                                                day5b(lottobook) = enteredreg
                                                day5c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 6) then
                                        if (day6a(20) == '') then
                                                lottobook = 20
                                        else if (day6a(19) == '') then
                                                lottobook = 19
                                        else if (day6a(18) == '') then
                                                lottobook = 18
                                        else if (day6a(17) == '') then
                                                lottobook = 17
                                        else if (day6a(16) == '') then
                                                lottobook = 16
                                        else if (day6a(15) == '') then
                                                lottobook = 15
                                        else if (day6a(14) == '') then
                                                lottobook = 14
                                        else if (day6a(13) == '') then
                                                lottobook = 13
                                        else if (day6a(12) == '') then
                                                lottobook = 12
                                        else if (day6a(11) == '') then
                                                lottobook = 11
                                        else if (day6a(10) == '') then
                                                lottobook = 10
                                        else if (day6a(9) == '') then
                                                lottobook = 9
                                        else if (day6a(8) == '') then
                                                lottobook = 8
                                        else if (day6a(7) == '') then
                                                lottobook = 7
                                        else if (day6a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day6a(lottobook) = '1'
                                                day6b(lottobook) = enteredreg
                                                day6c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 7) then
                                        if (day7a(20) == '') then
                                                lottobook = 20
                                        else if (day7a(19) == '') then
                                                lottobook = 19
                                        else if (day7a(18) == '') then
                                                lottobook = 18
                                        else if (day7a(17) == '') then
                                                lottobook = 17
                                        else if (day7a(16) == '') then
                                                lottobook = 16
                                        else if (day7a(15) == '') then
                                                lottobook = 15
                                        else if (day7a(14) == '') then
                                                lottobook = 14
                                        else if (day7a(13) == '') then
                                                lottobook = 13
                                        else if (day7a(12) == '') then
                                                lottobook = 12
                                        else if (day7a(11) == '') then
                                                lottobook = 11
                                        else if (day7a(10) == '') then
                                                lottobook = 10
                                        else if (day7a(9) == '') then
                                                lottobook = 9
                                        else if (day7a(8) == '') then
                                                lottobook = 8
                                        else if (day7a(7) == '') then
                                                lottobook = 7
                                        else if (day7a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day7a(lottobook) = '1'
                                                day7b(lottobook) = enteredreg
                                                day7c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 8) then
                                        if (day8a(20) == '') then
                                                lottobook = 20
                                        else if (day8a(19) == '') then
                                                lottobook = 19
                                        else if (day8a(18) == '') then
                                                lottobook = 18
                                        else if (day8a(17) == '') then
                                                lottobook = 17
                                        else if (day8a(16) == '') then
                                                lottobook = 16
                                        else if (day8a(15) == '') then
                                                lottobook = 15
                                        else if (day8a(14) == '') then
                                                lottobook = 14
                                        else if (day8a(13) == '') then
                                                lottobook = 13
                                        else if (day8a(12) == '') then
                                                lottobook = 12
                                        else if (day8a(11) == '') then
                                                lottobook = 11
                                        else if (day8a(10) == '') then
                                                lottobook = 10
                                        else if (day8a(9) == '') then
                                                lottobook = 9
                                        else if (day8a(8) == '') then
                                                lottobook = 8
                                        else if (day8a(7) == '') then
                                                lottobook = 7
                                        else if (day8a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day8a(lottobook) = '1'
                                                day8b(lottobook) = enteredreg
                                                day8c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 9) then
                                        if (day9a(20) == '') then
                                                lottobook = 20
                                        else if (day9a(19) == '') then
                                                lottobook = 19
                                        else if (day9a(18) == '') then
                                                lottobook = 18
                                        else if (day9a(17) == '') then
                                                lottobook = 17
                                        else if (day9a(16) == '') then
                                                lottobook = 16
                                        else if (day9a(15) == '') then
                                                lottobook = 15
                                        else if (day9a(14) == '') then
                                                lottobook = 14
                                        else if (day9a(13) == '') then
                                                lottobook = 13
                                        else if (day9a(12) == '') then
                                                lottobook = 12
                                        else if (day9a(11) == '') then
                                                lottobook = 11
                                        else if (day9a(10) == '') then
                                                lottobook = 10
                                        else if (day9a(9) == '') then
                                                lottobook = 9
                                        else if (day9a(8) == '') then
                                                lottobook = 8
                                        else if (day9a(7) == '') then
                                                lottobook = 7
                                        else if (day9a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day9a(lottobook) = '1'
                                                day9b(lottobook) = enteredreg
                                                day9c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 10) then
                                        if (day10a(20) == '') then
                                                lottobook = 20
                                        else if (day10a(19) == '') then
                                                lottobook = 19
                                        else if (day10a(18) == '') then
                                                lottobook = 18
                                        else if (day10a(17) == '') then
                                                lottobook = 17
                                        else if (day10a(16) == '') then
                                                lottobook = 16
                                        else if (day10a(15) == '') then
                                                lottobook = 15
                                        else if (day10a(14) == '') then
                                                lottobook = 14
                                        else if (day10a(13) == '') then
                                                lottobook = 13
                                        else if (day10a(12) == '') then
                                                lottobook = 12
                                        else if (day10a(11) == '') then
                                                lottobook = 11
                                        else if (day10a(10) == '') then
                                                lottobook = 10
                                        else if (day10a(9) == '') then
                                                lottobook = 9
                                        else if (day10a(8) == '') then
                                                lottobook = 8
                                        else if (day10a(7) == '') then
                                                lottobook = 7
                                        else if (day10a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day10a(lottobook) = '1'
                                                day10b(lottobook) = enteredreg
                                                day10c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 11) then
                                        if (day11a(20) == '') then
                                                lottobook = 20
                                        else if (day11a(19) == '') then
                                                lottobook = 19
                                        else if (day11a(18) == '') then
                                                lottobook = 18
                                        else if (day11a(17) == '') then
                                                lottobook = 17
                                        else if (day11a(16) == '') then
                                                lottobook = 16
                                        else if (day11a(15) == '') then
                                                lottobook = 15
                                        else if (day11a(14) == '') then
                                                lottobook = 14
                                        else if (day11a(13) == '') then
                                                lottobook = 13
                                        else if (day11a(12) == '') then
                                                lottobook = 12
                                        else if (day11a(11) == '') then
                                                lottobook = 11
                                        else if (day11a(10) == '') then
                                                lottobook = 10
                                        else if (day11a(9) == '') then
                                                lottobook = 9
                                        else if (day11a(8) == '') then
                                                lottobook = 8
                                        else if (day11a(7) == '') then
                                                lottobook = 7
                                        else if (day11a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day11a(lottobook) = '1'
                                                day11b(lottobook) = enteredreg
                                                day11c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 12) then
                                        if (day12a(20) == '') then
                                                lottobook = 20
                                        else if (day12a(19) == '') then
                                                lottobook = 19
                                        else if (day12a(18) == '') then
                                                lottobook = 18
                                        else if (day12a(17) == '') then
                                                lottobook = 17
                                        else if (day12a(16) == '') then
                                                lottobook = 16
                                        else if (day12a(15) == '') then
                                                lottobook = 15
                                        else if (day12a(14) == '') then
                                                lottobook = 14
                                        else if (day12a(13) == '') then
                                                lottobook = 13
                                        else if (day12a(12) == '') then
                                                lottobook = 12
                                        else if (day12a(11) == '') then
                                                lottobook = 11
                                        else if (day12a(10) == '') then
                                                lottobook = 10
                                        else if (day12a(9) == '') then
                                                lottobook = 9
                                        else if (day12a(8) == '') then
                                                lottobook = 8
                                        else if (day12a(7) == '') then
                                                lottobook = 7
                                        else if (day12a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day12a(lottobook) = '1'
                                                day12b(lottobook) = enteredreg
                                                day12c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 13) then
                                        if (day13a(20) == '') then
                                                lottobook = 20
                                        else if (day13a(19) == '') then
                                                lottobook = 19
                                        else if (day13a(18) == '') then
                                                lottobook = 18
                                        else if (day13a(17) == '') then
                                                lottobook = 17
                                        else if (day13a(16) == '') then
                                                lottobook = 16
                                        else if (day13a(15) == '') then
                                                lottobook = 15
                                        else if (day13a(14) == '') then
                                                lottobook = 14
                                        else if (day13a(13) == '') then
                                                lottobook = 13
                                        else if (day13a(12) == '') then
                                                lottobook = 12
                                        else if (day13a(11) == '') then
                                                lottobook = 11
                                        else if (day13a(10) == '') then
                                                lottobook = 10
                                        else if (day13a(9) == '') then
                                                lottobook = 9
                                        else if (day13a(8) == '') then
                                                lottobook = 8
                                        else if (day13a(7) == '') then
                                                lottobook = 7
                                        else if (day13a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day13a(lottobook) = '1'
                                                day13b(lottobook) = enteredreg
                                                day13c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 14) then
                                        if (day14a(20) == '') then
                                                lottobook = 20
                                        else if (day14a(19) == '') then
                                                lottobook = 19
                                        else if (day14a(18) == '') then
                                                lottobook = 18
                                        else if (day14a(17) == '') then
                                                lottobook = 17
                                        else if (day14a(16) == '') then
                                                lottobook = 16
                                        else if (day14a(15) == '') then
                                                lottobook = 15
                                        else if (day14a(14) == '') then
                                                lottobook = 14
                                        else if (day14a(13) == '') then
                                                lottobook = 13
                                        else if (day14a(12) == '') then
                                                lottobook = 12
                                        else if (day14a(11) == '') then
                                                lottobook = 11
                                        else if (day14a(10) == '') then
                                                lottobook = 10
                                        else if (day14a(9) == '') then
                                                lottobook = 9
                                        else if (day14a(8) == '') then
                                                lottobook = 8
                                        else if (day14a(7) == '') then
                                                lottobook = 7
                                        else if (day14a(6) == '') then
                                                lottobook = 6
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day14a(lottobook) = '1'
                                                day14b(lottobook) = enteredreg
                                                day14c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else 
                                        print *, 'Invalid day selected'
                                end if
                        else if (optionbooktype == 2) then
                                print *, 'Please select a day in the 14 day period'
                                read(*,*) dayslected
                                if (dayslected == 1) then
                                        if (day1a(1) == '') then
                                                lottobook = 1
                                        else if (day1a(2) == '') then
                                                lottobook = 2
                                        else if (day1a(3) == '') then
                                                lottobook = 3
                                        else if (day1a(4) == '') then
                                                lottobook = 4
                                        else if (day1a(5) == '') then
                                                lottobook = 5
                                        else if (day1a(6) == '') then
                                                lottobook = 6
                                        else if (day1a(7) == '') then
                                                lottobook = 7
                                        else if (day1a(8) == '') then
                                                lottobook = 8
                                        else if (day1a(9) == '') then
                                                lottobook = 9
                                        else if (day1a(10) == '') then
                                                lottobook = 10
                                        else if (day1a(11) == '') then
                                                lottobook = 11
                                        else if (day1a(12) == '') then
                                                lottobook = 12
                                        else if (day1a(13) == '') then
                                                lottobook = 13
                                        else if (day1a(14) == '') then
                                                lottobook = 14
                                        else if (day1a(15) == '') then
                                                lottobook = 15
                                        else if (day1a(16) == '') then
                                                lottobook = 16
                                        else if (day1a(17) == '') then
                                                lottobook = 17
                                        else if (day1a(18) == '') then
                                                lottobook = 18
                                        else if (day1a(19) == '') then
                                                lottobook = 19
                                        else if (day1a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if 
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day1a(lottobook) = '1'
                                                day1b(lottobook) = enteredreg
                                                day1c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 2) then
                                        if (day2a(1) == '') then
                                                lottobook = 1
                                        else if (day2a(2) == '') then
                                                lottobook = 2
                                        else if (day2a(3) == '') then
                                                lottobook = 3
                                        else if (day2a(4) == '') then
                                                lottobook = 4
                                        else if (day2a(5) == '') then
                                                lottobook = 5
                                        else if (day2a(6) == '') then
                                                lottobook = 6
                                        else if (day2a(7) == '') then
                                                lottobook = 7
                                        else if (day2a(8) == '') then
                                                lottobook = 8
                                        else if (day2a(9) == '') then
                                                lottobook = 9
                                        else if (day2a(10) == '') then
                                                lottobook = 10
                                        else if (day2a(11) == '') then
                                                lottobook = 11
                                        else if (day2a(12) == '') then
                                                lottobook = 12
                                        else if (day2a(13) == '') then
                                                lottobook = 13
                                        else if (day2a(14) == '') then
                                                lottobook = 14
                                        else if (day2a(15) == '') then
                                                lottobook = 15
                                        else if (day2a(16) == '') then
                                                lottobook = 16
                                        else if (day2a(17) == '') then
                                                lottobook = 17
                                        else if (day2a(18) == '') then
                                                lottobook = 18
                                        else if (day2a(19) == '') then
                                                lottobook = 19
                                        else if (day2a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day2a(lottobook) = '1'
                                                day2b(lottobook) = enteredreg
                                                day2c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 3) then
                                        if (day3a(1) == '') then
                                                lottobook = 1
                                        else if (day3a(2) == '') then
                                                lottobook = 2
                                        else if (day3a(3) == '') then
                                                lottobook = 3
                                        else if (day3a(4) == '') then
                                                lottobook = 4
                                        else if (day3a(5) == '') then
                                                lottobook = 5
                                        else if (day3a(6) == '') then
                                                lottobook = 6
                                        else if (day3a(7) == '') then
                                                lottobook = 7
                                        else if (day3a(8) == '') then
                                                lottobook = 8
                                        else if (day3a(9) == '') then
                                                lottobook = 9
                                        else if (day3a(10) == '') then
                                                lottobook = 10
                                        else if (day3a(11) == '') then
                                                lottobook = 11
                                        else if (day3a(12) == '') then
                                                lottobook = 12
                                        else if (day3a(13) == '') then
                                                lottobook = 13
                                        else if (day3a(14) == '') then
                                                lottobook = 14
                                        else if (day3a(15) == '') then
                                                lottobook = 15
                                        else if (day3a(16) == '') then
                                                lottobook = 16
                                        else if (day3a(17) == '') then
                                                lottobook = 17
                                        else if (day3a(18) == '') then
                                                lottobook = 18
                                        else if (day3a(19) == '') then
                                                lottobook = 19
                                        else if (day3a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day3a(lottobook) = '1'
                                                day3b(lottobook) = enteredreg
                                                day3c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 4) then
                                        if (day4a(1) == '') then
                                                lottobook = 1
                                        else if (day4a(2) == '') then
                                                lottobook = 2
                                        else if (day4a(3) == '') then
                                                lottobook = 3
                                        else if (day4a(4) == '') then
                                                lottobook = 4
                                        else if (day4a(5) == '') then
                                                lottobook = 5
                                        else if (day4a(6) == '') then
                                                lottobook = 6
                                        else if (day4a(7) == '') then
                                                lottobook = 7
                                        else if (day4a(8) == '') then
                                                lottobook = 8
                                        else if (day4a(9) == '') then
                                                lottobook = 9
                                        else if (day4a(10) == '') then
                                                lottobook = 10
                                        else if (day4a(11) == '') then
                                                lottobook = 11
                                        else if (day4a(12) == '') then
                                                lottobook = 12
                                        else if (day4a(13) == '') then
                                                lottobook = 13
                                        else if (day4a(14) == '') then
                                                lottobook = 14
                                        else if (day4a(15) == '') then
                                                lottobook = 15
                                        else if (day4a(16) == '') then
                                                lottobook = 16
                                        else if (day4a(17) == '') then
                                                lottobook = 17
                                        else if (day4a(18) == '') then
                                                lottobook = 18
                                        else if (day4a(19) == '') then
                                                lottobook = 19
                                        else if (day4a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day4a(lottobook) = '1'
                                                day4b(lottobook) = enteredreg
                                                day4c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 5) then
                                        if (day5a(1) == '') then
                                                lottobook = 1
                                        else if (day5a(2) == '') then
                                                lottobook = 2
                                        else if (day5a(3) == '') then
                                                lottobook = 3
                                        else if (day5a(4) == '') then
                                                lottobook = 4
                                        else if (day5a(5) == '') then
                                                lottobook = 5
                                        else if (day5a(6) == '') then
                                                lottobook = 6
                                        else if (day5a(7) == '') then
                                                lottobook = 7
                                        else if (day5a(8) == '') then
                                                lottobook = 8
                                        else if (day5a(9) == '') then
                                                lottobook = 9
                                        else if (day5a(10) == '') then
                                                lottobook = 10
                                        else if (day5a(11) == '') then
                                                lottobook = 11
                                        else if (day5a(12) == '') then
                                                lottobook = 12
                                        else if (day5a(13) == '') then
                                                lottobook = 13
                                        else if (day5a(14) == '') then
                                                lottobook = 14
                                        else if (day5a(15) == '') then
                                                lottobook = 15
                                        else if (day5a(16) == '') then
                                                lottobook = 16
                                        else if (day5a(17) == '') then
                                                lottobook = 17
                                        else if (day5a(18) == '') then
                                                lottobook = 18
                                        else if (day5a(19) == '') then
                                                lottobook = 19
                                        else if (day5a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day5a(lottobook) = '1'
                                                day5b(lottobook) = enteredreg
                                                day5c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 6) then
                                        if (day6a(1) == '') then
                                                lottobook = 1
                                        else if (day6a(2) == '') then
                                                lottobook = 2
                                        else if (day6a(3) == '') then
                                                lottobook = 3
                                        else if (day6a(4) == '') then
                                                lottobook = 4
                                        else if (day6a(5) == '') then
                                                lottobook = 5
                                        else if (day6a(6) == '') then
                                                lottobook = 6
                                        else if (day6a(7) == '') then
                                                lottobook = 7
                                        else if (day6a(8) == '') then
                                                lottobook = 8
                                        else if (day6a(9) == '') then
                                                lottobook = 9
                                        else if (day6a(10) == '') then
                                                lottobook = 10
                                        else if (day6a(11) == '') then
                                                lottobook = 11
                                        else if (day6a(12) == '') then
                                                lottobook = 12
                                        else if (day6a(13) == '') then
                                                lottobook = 13
                                        else if (day6a(14) == '') then
                                                lottobook = 14
                                        else if (day6a(15) == '') then
                                                lottobook = 15
                                        else if (day6a(16) == '') then
                                                lottobook = 16
                                        else if (day6a(17) == '') then
                                                lottobook = 17
                                        else if (day6a(18) == '') then
                                                lottobook = 18
                                        else if (day6a(19) == '') then
                                                lottobook = 19
                                        else if (day6a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day6a(lottobook) = '1'
                                                day6b(lottobook) = enteredreg
                                                day6c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 7) then
                                        if (day7a(1) == '') then
                                                lottobook = 1
                                        else if (day7a(2) == '') then
                                                lottobook = 2
                                        else if (day7a(3) == '') then
                                                lottobook = 3
                                        else if (day7a(4) == '') then
                                                lottobook = 4
                                        else if (day7a(5) == '') then
                                                lottobook = 5
                                        else if (day7a(6) == '') then
                                                lottobook = 6
                                        else if (day7a(7) == '') then
                                                lottobook = 7
                                        else if (day7a(8) == '') then
                                                lottobook = 8
                                        else if (day7a(9) == '') then
                                                lottobook = 9
                                        else if (day7a(10) == '') then
                                                lottobook = 10
                                        else if (day7a(11) == '') then
                                                lottobook = 11
                                        else if (day7a(12) == '') then
                                                lottobook = 12
                                        else if (day7a(13) == '') then
                                                lottobook = 13
                                        else if (day7a(14) == '') then
                                                lottobook = 14
                                        else if (day7a(15) == '') then
                                                lottobook = 15
                                        else if (day7a(16) == '') then
                                                lottobook = 16
                                        else if (day7a(17) == '') then
                                                lottobook = 17
                                        else if (day7a(18) == '') then
                                                lottobook = 18
                                        else if (day7a(19) == '') then
                                                lottobook = 19
                                        else if (day7a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day7a(lottobook) = '1'
                                                day7b(lottobook) = enteredreg
                                                day7c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 8) then
                                        if (day8a(1) == '') then
                                                lottobook = 1
                                        else if (day8a(2) == '') then
                                                lottobook = 2
                                        else if (day8a(3) == '') then
                                                lottobook = 3
                                        else if (day8a(4) == '') then
                                                lottobook = 4
                                        else if (day8a(5) == '') then
                                                lottobook = 5
                                        else if (day8a(6) == '') then
                                                lottobook = 6
                                        else if (day8a(7) == '') then
                                                lottobook = 7
                                        else if (day8a(8) == '') then
                                                lottobook = 8
                                        else if (day8a(9) == '') then
                                                lottobook = 9
                                        else if (day8a(10) == '') then
                                                lottobook = 10
                                        else if (day8a(11) == '') then
                                                lottobook = 11
                                        else if (day8a(12) == '') then
                                                lottobook = 12
                                        else if (day8a(13) == '') then
                                                lottobook = 13
                                        else if (day8a(14) == '') then
                                                lottobook = 14
                                        else if (day8a(15) == '') then
                                                lottobook = 15
                                        else if (day8a(16) == '') then
                                                lottobook = 16
                                        else if (day8a(17) == '') then
                                                lottobook = 17
                                        else if (day8a(18) == '') then
                                                lottobook = 18
                                        else if (day8a(19) == '') then
                                                lottobook = 19
                                        else if (day8a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day8a(lottobook) = '1'
                                                day8b(lottobook) = enteredreg
                                                day8c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 9) then
                                        if (day9a(1) == '') then
                                                lottobook = 1
                                        else if (day9a(2) == '') then
                                                lottobook = 2
                                        else if (day9a(3) == '') then
                                                lottobook = 3
                                        else if (day9a(4) == '') then
                                                lottobook = 4
                                        else if (day9a(5) == '') then
                                                lottobook = 5
                                        else if (day9a(6) == '') then
                                                lottobook = 6
                                        else if (day9a(7) == '') then
                                                lottobook = 7
                                        else if (day9a(8) == '') then
                                                lottobook = 8
                                        else if (day9a(9) == '') then
                                                lottobook = 9
                                        else if (day9a(10) == '') then
                                                lottobook = 10
                                        else if (day9a(11) == '') then
                                                lottobook = 11
                                        else if (day9a(12) == '') then
                                                lottobook = 12
                                        else if (day9a(13) == '') then
                                                lottobook = 13
                                        else if (day9a(14) == '') then
                                                lottobook = 14
                                        else if (day9a(15) == '') then
                                                lottobook = 15
                                        else if (day9a(16) == '') then
                                                lottobook = 16
                                        else if (day9a(17) == '') then
                                                lottobook = 17
                                        else if (day9a(18) == '') then
                                                lottobook = 18
                                        else if (day9a(19) == '') then
                                                lottobook = 19
                                        else if (day9a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day9a(lottobook) = '1'
                                                day9b(lottobook) = enteredreg
                                                day9c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 10) then
                                        if (day10a(1) == '') then
                                                lottobook = 1
                                        else if (day10a(2) == '') then
                                                lottobook = 2
                                        else if (day10a(3) == '') then
                                                lottobook = 3
                                        else if (day10a(4) == '') then
                                                lottobook = 4
                                        else if (day10a(5) == '') then
                                                lottobook = 5
                                        else if (day10a(6) == '') then
                                                lottobook = 6
                                        else if (day10a(7) == '') then
                                                lottobook = 7
                                        else if (day10a(8) == '') then
                                                lottobook = 8
                                        else if (day10a(9) == '') then
                                                lottobook = 9
                                        else if (day10a(10) == '') then
                                                lottobook = 10
                                        else if (day10a(11) == '') then
                                                lottobook = 11
                                        else if (day10a(12) == '') then
                                                lottobook = 12
                                        else if (day10a(13) == '') then
                                                lottobook = 13
                                        else if (day10a(14) == '') then
                                                lottobook = 14
                                        else if (day10a(15) == '') then
                                                lottobook = 15
                                        else if (day10a(16) == '') then
                                                lottobook = 16
                                        else if (day10a(17) == '') then
                                                lottobook = 17
                                        else if (day10a(18) == '') then
                                                lottobook = 18
                                        else if (day10a(19) == '') then
                                                lottobook = 19
                                        else if (day10a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day10a(lottobook) = '1'
                                                day10b(lottobook) = enteredreg
                                                day10c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 11) then
                                        if (day11a(1) == '') then
                                                lottobook = 1
                                        else if (day11a(2) == '') then
                                                lottobook = 2
                                        else if (day11a(3) == '') then
                                                lottobook = 3
                                        else if (day11a(4) == '') then
                                                lottobook = 4
                                        else if (day11a(5) == '') then
                                                lottobook = 5
                                        else if (day11a(6) == '') then
                                                lottobook = 6
                                        else if (day11a(7) == '') then
                                                lottobook = 7
                                        else if (day11a(8) == '') then
                                                lottobook = 8
                                        else if (day11a(9) == '') then
                                                lottobook = 9
                                        else if (day11a(10) == '') then
                                                lottobook = 10
                                        else if (day11a(11) == '') then
                                                lottobook = 11
                                        else if (day11a(12) == '') then
                                                lottobook = 12
                                        else if (day11a(13) == '') then
                                                lottobook = 13
                                        else if (day11a(14) == '') then
                                                lottobook = 14
                                        else if (day11a(15) == '') then
                                                lottobook = 15
                                        else if (day11a(16) == '') then
                                                lottobook = 16
                                        else if (day11a(17) == '') then
                                                lottobook = 17
                                        else if (day11a(18) == '') then
                                                lottobook = 18
                                        else if (day11a(19) == '') then
                                                lottobook = 19
                                        else if (day11a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day11a(lottobook) = '1'
                                                day11b(lottobook) = enteredreg
                                                day11c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 12) then
                                        if (day12a(1) == '') then
                                                lottobook = 1
                                        else if (day12a(2) == '') then
                                                lottobook = 2
                                        else if (day12a(3) == '') then
                                                lottobook = 3
                                        else if (day12a(4) == '') then
                                                lottobook = 4
                                        else if (day12a(5) == '') then
                                                lottobook = 5
                                        else if (day12a(6) == '') then
                                                lottobook = 6
                                        else if (day12a(7) == '') then
                                                lottobook = 7
                                        else if (day12a(8) == '') then
                                                lottobook = 8
                                        else if (day12a(9) == '') then
                                                lottobook = 9
                                        else if (day12a(10) == '') then
                                                lottobook = 10
                                        else if (day12a(11) == '') then
                                                lottobook = 11
                                        else if (day12a(12) == '') then
                                                lottobook = 12
                                        else if (day12a(13) == '') then
                                                lottobook = 13
                                        else if (day12a(14) == '') then
                                                lottobook = 14
                                        else if (day12a(15) == '') then
                                                lottobook = 15
                                        else if (day12a(16) == '') then
                                                lottobook = 16
                                        else if (day12a(17) == '') then
                                                lottobook = 17
                                        else if (day12a(18) == '') then
                                                lottobook = 18
                                        else if (day12a(19) == '') then
                                                lottobook = 19
                                        else if (day12a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day12a(lottobook) = '1'
                                                day12b(lottobook) = enteredreg
                                                day12c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 13) then
                                        if (day13a(1) == '') then
                                                lottobook = 1
                                        else if (day13a(2) == '') then
                                                lottobook = 2
                                        else if (day13a(3) == '') then
                                                lottobook = 3
                                        else if (day13a(4) == '') then
                                                lottobook = 4
                                        else if (day13a(5) == '') then
                                                lottobook = 5
                                        else if (day13a(6) == '') then
                                                lottobook = 6
                                        else if (day13a(7) == '') then
                                                lottobook = 7
                                        else if (day13a(8) == '') then
                                                lottobook = 8
                                        else if (day13a(9) == '') then
                                                lottobook = 9
                                        else if (day13a(10) == '') then
                                                lottobook = 10
                                        else if (day13a(11) == '') then
                                                lottobook = 11
                                        else if (day13a(12) == '') then
                                                lottobook = 12
                                        else if (day13a(13) == '') then
                                                lottobook = 13
                                        else if (day13a(14) == '') then
                                                lottobook = 14
                                        else if (day13a(15) == '') then
                                                lottobook = 15
                                        else if (day13a(16) == '') then
                                                lottobook = 16
                                        else if (day13a(17) == '') then
                                                lottobook = 17
                                        else if (day13a(18) == '') then
                                                lottobook = 18
                                        else if (day13a(19) == '') then
                                                lottobook = 19
                                        else if (day13a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day13a(lottobook) = '1'
                                                day13b(lottobook) = enteredreg
                                                day13c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else if (dayslected == 14) then
                                        if (day14a(1) == '') then
                                                lottobook = 1
                                        else if (day14a(2) == '') then
                                                lottobook = 2
                                        else if (day14a(3) == '') then
                                                lottobook = 3
                                        else if (day14a(4) == '') then
                                                lottobook = 4
                                        else if (day14a(5) == '') then
                                                lottobook = 5
                                        else if (day14a(6) == '') then
                                                lottobook = 6
                                        else if (day14a(7) == '') then
                                                lottobook = 7
                                        else if (day14a(8) == '') then
                                                lottobook = 8
                                        else if (day14a(9) == '') then
                                                lottobook = 9
                                        else if (day14a(10) == '') then
                                                lottobook = 10
                                        else if (day14a(11) == '') then
                                                lottobook = 11
                                        else if (day14a(12) == '') then
                                                lottobook = 12
                                        else if (day14a(13) == '') then
                                                lottobook = 13
                                        else if (day14a(14) == '') then
                                                lottobook = 14
                                        else if (day14a(15) == '') then
                                                lottobook = 15
                                        else if (day14a(16) == '') then
                                                lottobook = 16
                                        else if (day14a(17) == '') then
                                                lottobook = 17
                                        else if (day14a(18) == '') then
                                                lottobook = 18
                                        else if (day14a(19) == '') then
                                                lottobook = 19
                                        else if (day14a(20) == '') then
                                                lottobook = 20
                                        else
                                                print *, 'Sorry their are now parking avalible today'
                                        end if
                                        if (lottobook > 0) then
                                                print *, 'Please enter the name'
                                                read(*,*) enteredname
                                                print *, 'Please enter the regesration number'
                                                read(*,*) enteredreg
                                                day14a(lottobook) = '1'
                                                day14b(lottobook) = enteredreg
                                                day14c(lottobook) = enteredname
                                                lottobook = 0
                                        end if
                                else 
                                        print *, 'Invalid day selected'
                                end if
                        end if
                else if (option1 == 3) then
                        print *, 'Please select an option'
                        print *, 'Option 1 | Statistics of a specific day'
                        print *, 'Option 2 | Statistics of all 14 days'
                        read(*,*) statsoption
                        if (statsoption == 1) then
                                print *, 'Please select a day for statistics'
                                read(*,*) daystatslected
                                if (daystatslected == 1) then
                                        call statscalc(day1a)
                                else if (daystatslected == 2) then
                                        call statscalc(day2a)
                                else if (daystatslected == 3) then
                                        call statscalc(day3a)
                                else if (daystatslected == 4) then
                                        call statscalc(day4a)
                                else if (daystatslected == 5) then
                                        call statscalc(day5a)
                                else if (daystatslected == 6) then
                                        call statscalc(day6a)
                                else if (daystatslected == 7) then
                                        call statscalc(day7a)
                                else if (daystatslected == 8) then
                                        call statscalc(day8a)
                                else if (daystatslected == 9) then
                                        call statscalc(day9a)
                                else if (daystatslected == 10) then
                                        call statscalc(day10a)
                                else if (daystatslected == 11) then
                                        call statscalc(day11a)
                                else if (daystatslected == 12) then
                                        call statscalc(day12a)
                                else if (daystatslected == 13) then
                                        call statscalc(day13a)
                                else if (daystatslected == 14) then
                                        call statscalc(day14a)
                                end if 
                        else if (statsoption == 2) then
                                do loopy1 = 1, 20
                                        if (trim(day1a(loopy1)) == '1') then
                                                if (loopy1 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                do loopy2 = 1, 20
                                        if (trim(day2a(loopy2)) == '1') then
                                                if (loopy2 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                do loopy3 = 1, 20
                                        if (trim(day3a(loopy3)) == '1') then
                                                if (loopy3 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                do loopy4 = 1, 20
                                        if (trim(day4a(loopy4)) == '1') then
                                                if (loopy4 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                do loopy5 = 1, 20
                                        if (trim(day5a(loopy5)) == '1') then
                                                if (loopy5 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                do loopy6 = 1, 20
                                        if (trim(day6a(loopy6)) == '1') then
                                                if (loopy6 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                do loopy7 = 1, 20
                                        if (trim(day7a(loopy7)) == '1') then
                                                if (loopy7 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                do loopy8 = 1, 20
                                        if (trim(day8a(loopy8)) == '1') then
                                                if (loopy8 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                do loopy9 = 1, 20
                                        if (trim(day9a(loopy9)) == '1') then
                                                if (loopy9 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                do loopy10 = 1, 20
                                        if (trim(day10a(loopy10)) == '1') then
                                                if (loopy10 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                do loopy11 = 1, 20
                                        if (trim(day11a(loopy11)) == '1') then
                                                if (loopy11 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                do loopy12 = 1, 20
                                        if (trim(day12a(loopy12)) == '1') then
                                                if (loopy12 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                do loopy13 = 1, 20
                                        if (trim(day13a(loopy13)) == '1') then
                                                if (loopy13 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                do loopy14 = 1, 20
                                        if (trim(day14a(loopy14)) == '1') then
                                                if (loopy14 < 6) then
                                                        totaldisabledparkings = totaldisabledparkings + 1
                                                else 
                                                        totalnormalparkings = totalnormalparkings + 1
                                                end if
                                        end if
                                end do
                                print *, 'Total Number of Normal Parkings Used', totalnormalparkings
                                print *, 'Total Number of Disabled Parkings Used', totaldisabledparkings
                                print *, 'Total Number of Parkings Used', totaldisabledparkings + totalnormalparkings
                        end if

                end if

        end do

        contains

        subroutine parkingprinter(daysa, daysb, daysc)
                character(len=*), intent(in) :: daysa(:)
                character(len=*), intent(in) :: daysb(:)
                character(len=*), intent(in) :: daysc(:)

                print *, 'Lot Number | Ocqupied                       | Registration                   | Name'
                print *, 'Lot 1 DIS  |', daysa(1), '|', daysb(1), '|', daysc(1)
                print *, 'Lot 2 DIS  |', daysa(2), '|', daysb(2), '|', daysc(2)
                print *, 'Lot 3 DIS  |', daysa(3), '|', daysb(3), '|', daysc(3)
                print *, 'Lot 4 DIS  |', daysa(4), '|', daysb(4), '|', daysc(4)
                print *, 'Lot 5 DIS  |', daysa(5), '|', daysb(5), '|', daysc(5)
                print *, 'Lot 6      |', daysa(6), '|', daysb(6), '|', daysc(6)
                print *, 'Lot 7      |', daysa(7), '|', daysb(7), '|', daysc(7)
                print *, 'Lot 8      |', daysa(8), '|', daysb(8), '|', daysc(8)
                print *, 'Lot 9      |', daysa(9), '|', daysb(9), '|', daysc(9)
                print *, 'Lot 10     |', daysa(10), '|', daysb(10), '|', daysc(10)
                print *, 'Lot 11     |', daysa(11), '|', daysb(11), '|', daysc(11)
                print *, 'Lot 12     |', daysa(12), '|', daysb(12), '|', daysc(12)
                print *, 'Lot 13     |', daysa(13), '|', daysb(13), '|', daysc(13)
                print *, 'Lot 14     |', daysa(14), '|', daysb(14), '|', daysc(14)
                print *, 'Lot 15     |', daysa(15), '|', daysb(15), '|', daysc(15)
                print *, 'Lot 16     |', daysa(16), '|', daysb(16), '|', daysc(16)
                print *, 'Lot 17     |', daysa(17), '|', daysb(17), '|', daysc(17)
                print *, 'Lot 18     |', daysa(18), '|', daysb(18), '|', daysc(18)
                print *, 'Lot 19     |', daysa(19), '|', daysb(19), '|', daysc(19)
                print *, 'Lot 20     |', daysa(20), '|', daysb(20), '|', daysc(20)
        end subroutine parkingprinter

        subroutine statscalc(dayse)
                character(len=*), intent(in) :: dayse(:)
                integer :: usednormlparking
                integer :: useddisblparking

                usednormlparking = 0
                useddisblparking = 0

                if (trim(dayse(1)) == '1') then
                        useddisblparking = useddisblparking + 1
                end if
                if (trim(dayse(2)) == '1') then
                        useddisblparking = useddisblparking + 1
                end if
                if (trim(dayse(3)) == '1') then
                        useddisblparking = useddisblparking + 1
                end if
                if (trim(dayse(4)) == '1') then
                        useddisblparking = useddisblparking + 1 
                end if
                if (trim(dayse(5)) == '1') then
                        useddisblparking = useddisblparking + 1
                end if
                if (trim(dayse(6)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(7)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(8)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(9)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(10)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(11)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(12)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(13)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(14)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(15)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(16)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(17)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(18)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(19)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                if (trim(dayse(20)) == '1') then
                        usednormlparking = usednormlparking + 1
                end if
                print *, 'Number of Disabled Parkings Used ', useddisblparking
                print *, 'Number of Normal Parkings Used', usednormlparking
                print *, 'Number of Parkings Used in Total', useddisblparking + usednormlparking

        end subroutine statscalc

end program parking
