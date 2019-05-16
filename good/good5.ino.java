int main () {
	int x = 1;
	print(x, "\n");
	{
		// przysłonienie zmiennej wartością innego typu
		string x = "Hello world!";
		print(x, "\n");
		{
			// przysłonięcie zmiennej funkcją, dowolnie zagnieżdżone definicje funkcji z zachowaniem poprawności statycznego wiązania identyfikatorów
			void x() {
				print("Funkcja\n");
				return;
			}
			x();
			print(x);
			{
				// przysłonięcie funkcji zmienną
				boolean x = true;
				print(x, "\n");
			}
			x();
		}
		print(x, "\n");
	}
	print(x, "\n");

	return 0;
}