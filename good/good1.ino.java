int main() {
	// zmienne o typie int, domyślna wartość 0
	int x1, x2 = 1;
	print("x1 = ", x1, "\n");

	// zmienne o typie bool, domyślna wartość false
	boolean x3 = true;

	// zmienne o typie string, domyślna wartość ""
	string x4 = "Hello world!";
	print(x4, "\n");
	
	// zmienne o typie void
	void x5;

	// przypisanie do zmiennej
	print("Przypisanie:\n", "x2 = ", x2);
	x2 = 10;
	print("\nx2 = ", x2,"\n");

	string x6;

	// if bez else
	if(x3) {
		x6 = "If";
	} 
	print(x6, "\n");

	// while, porównania
	while(x1 < x2) {
		x1++;
		print(x1, "\n");
	}

	return 0;
}