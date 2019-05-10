// TEST COVERAGE

int main() {
	// zmienne typu int
	int x1, x2;
	printInt(x1, x2) ;
	printString("\n") ;

	// operacja przypisania
	x1 = 1 ;
	x2 = 10 ;

	// if bez elsa, różne porównania
	if (x1 < x2)
		printString("mniejsze", "\n") ;
	
	// if z elsem
	if (x1 >= x2)
		printString("wieksze rowne", "\n") ;
	else 
		printString("mniejsze", "\n") ;

	// while
	while (x1 < x2) {
		printInt(x1) ;
		printString("\n") ;
		x1++ ;
	}

	// typ bool, porównania
	boolean x3 = x1 <= x2 ;
	if (x3)
		printString("true", "\n") ;
	else
		printString("false", "\n") ; 

	// typ string
	string x4 = "Hello world!" ;

	// jawne wypisanie wartości na wyjście
	printString(x4, "\n") ;

	// operacje arytmetyczne
	int x5 = 10 / 3;

	rec();
}

void rec() {
	printString("loop", "\n") ;
	rec();
}