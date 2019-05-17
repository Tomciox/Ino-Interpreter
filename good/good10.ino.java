int main () {
	int c = 0;

	// Zaimplementowane zwalnianie pamięci z nieosiągalnymi zmiennymi.
	// Program może jedynie pamiętać 10`000 obiektów (initialFreeLocations = [1..10000]).
	// Tutaj, gdyby wielokrotnie redeklarowane zmienne x były wszystkie pamiętane,
	// program potrzebowałby co najmniej 300`000 lokacji w pamięci.
	while(c < 100000) {
		int x = 1;
		{
			int x = 2;
			{
				int x = 3;
			}
		}
		c++;
	}

	print("Koniec.");

	return 0;
}