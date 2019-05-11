int main () {
	int x = 1 ;
	// zły typ argumentu przekazywanego przez wartość
	f(1, "x", x) ;

	// argument przekazywany przez referencję nie jest identyfikatorem
	// f(1, 1, 1) ;
	
	// zły typ argumentu przekazywanego przez referencję
	// string y = "y" ;
	// f(1, 1, y) ;
}

void f(int a, int b, int &c) {
	printString() ;
} 