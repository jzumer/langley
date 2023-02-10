i := 20 ;

loop {
	if i = 0 then
		break
	else {
		if { i % 15 ; } = 0 then print ( "FizzBuzz" )
		else if { i % 5 ; } = 0 then print ( "Buzz" )
		else if { i % 3 ; } = 0 then print ( "Fizz" )
		else printint ( i ) ; ; ; ;
		i := i - 1 ;
	} ; ;
} ;
