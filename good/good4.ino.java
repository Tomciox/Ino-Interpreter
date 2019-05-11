int main () {
	// argumenty przekazywane do funkcji przez wartość lub referencję
	int x = 2222, y = 3333 ;
	printString("x, y:\n") ;
	printInt(x, y) ;
	printString("\n") ;

	f(x, y) ;

	// pierwszy argument ma oryginalną wartość, drugi zmienioną
	printString("x, y:\n") ;
	printInt(x, y) ;
	printString("\n") ;
}

void f (int a, int &b) {
	printString("a, b:\n") ;
	printInt(a, b) ;
	printString("\n") ;

	a = a * 2 ;
	b = b * 3 ;

	printString("a, b:\n") ;
	printInt(a, b) ;
	printString("\n") ;
}