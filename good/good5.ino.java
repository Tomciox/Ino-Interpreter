int main () {
	// przesłanianie identyfikatorów ze statycznym ich wiązaniem
	int x = 1;
	printInt(x) ;
	printString("\n") ;

	{
		int x = 2;
		printInt(x) ;
		printString("\n") ;

		{
			int x = 3;
			printInt(x) ;
			printString("\n") ;
		}

		int x = 2;
		printInt(x) ;
		printString("\n") ;
	}

	int x = 1;
	printInt(x) ;
	printString("\n") ;
}