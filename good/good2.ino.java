// Funkcje z parametrami przez wartość, rekurencja
void printSequence(int x) {
	if (x > 0) {
		print(x, " ");
		x--;
		printSequence(x);
	}
	return;
}

int main() {
	int x = 10;
	print("x = ", x, "\n");

	printSequence(x);

	// zmienna nie została zmieniona
	print("\n", "x = ", x, "\n");	
	return 0;
}