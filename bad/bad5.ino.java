// za mało argumentów funkcji (lub za dużo)

int f(int a, int b) {
	print(a, b) ;
	return 1;
}

int main() {
	f(1);
	// f(1, 2, 3);
	return 0;
}