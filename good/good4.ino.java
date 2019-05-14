// argumenty przekazywane do funkcji przez wartość lub referencję

void f (int a, int &b) {
	print("a = ", a, " b = ", b, "\n");
	a = a * 2 ;
	b = b * 3 ;
	print("a = ", a, " b = ", b, "\n");
	return;
}

int main () {
	int x = 2222, y = 3333;
	print("x = ", x, " y = ", y, "\n");

	f(x, y);

	// pierwszy argument ma oryginalną wartość, drugi zmienioną
	print("x = ", x, " y = ", y, "\n");
	
	return 0;
}