int main () {
	// zmienne o typie int, domyślna wartość 0
	int x1, x2 = 1;
	printString("x1 = ") ;
	printInt(x1) ;
	printString("\n\n") ;

	// zmienne o typie bool, domyślna wartość false
	boolean x3 = true ;

	// zmienne o typie string, domyślna wartość ""
	string x4 = "Hello world!" ;
	printString(x4, "\n\n") ;
	
	// zmienne o typie void
	void x5 ;

	// przypisanie do zmiennej
	printString("Przypisanie:\n") ;
	printString("x2 = ") ;
	printInt(x2) ;
	x2 = 10 ;
	printString("\nx2 = ") ;
	printInt(x2) ;
	printString("\n\n") ;

	string x6 ;

	// if bez else
	if (x3) {
		x6 = "If" ;
	} 
	printString(x6, "\n\n");

	// while, porównania
	while (x1 < x2) {
		x1++ ;
		printInt(x1) ;
		printString("\n") ;
	}
}