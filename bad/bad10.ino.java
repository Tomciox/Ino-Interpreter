int positive(int x) {
	return x > 0;
}

int main () {
	// zły typ returnowanego wyniku
	int x = positive(1);
	print(x, "\n");

	return 0;
}