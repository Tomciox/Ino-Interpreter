// Funkcje z parametrami przez wartość, rekurencja

void printSequence(int i) {
	if (i > 0) {
		printInt(i) ;
		i-- ;
		printSequence(i) ;
	}
}

int main () {
	int x1 = 10 ;
	printString("x1 = ") ;
	printInt(x1) ;
	printString("\n\n") ;

	printSequence(x1) ;

	// zmienna nie została zmieniona
	printString("\n\n") ;	
	printString("x1 = ") ;
	printInt(x1) ;
	printString("\n\n") ;	
}