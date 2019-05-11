int main () {
	// arytmetyka
	int x1 = 7, x2 = 3 ;
	// jawne wypisywanie intów na wyjście
	printInt(x1 + x2, x1 - x2, x1 / x2, x1 * x2, x1 % x2) ;
	// jawne wypisywanie stringów na wyjście
	printString("\n") ;

	// else łączy się z najbliższym ifem
	// brak wypisania
	if (false)
		if (true)
			printString("If\n") ;
		else 
			printString("Else\n") ;
	// wypisanie "else"
	if (true)
		if (false)
			printString("If\n") ;
		else 
			printString("Else\n") ;
}