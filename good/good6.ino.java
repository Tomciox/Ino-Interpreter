int main () {
	int x = 5 ;

	printInt(x) ;
	printString("\n") ;

	f(x) ;

	printInt(x) ;
	printString("\n") ;
}

// przesłanianie funkcją tego samego identyfikatora 
void f (int x) {
	printInt(x) ;
	printString("\n") ;

	x++ ;

	printInt(x) ;
	printString("\n") ;

	// przesłanianie identyfikatorów innym typem (lub tym samym)
	{
		string x = "Hello world!" ;
		printString(x, "\n") ;
	}
	
	printInt(x) ;
	printString("\n") ;

	x++ ;

	printInt(x) ;
	printString("\n") ;
}
