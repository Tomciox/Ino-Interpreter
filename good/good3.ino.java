int main() {
	// arytmetyka
	int x1 = 7, x2 = 3;

	// jawne wypisywanie na wyjście
	print(
		x1, " + ", x2, " = ", x1 + x2, "\n", 
		x1, " - ", x2, " = ", x1 - x2, "\n", 
		x1, " / ", x2, " = ", x1 / x2, "\n",
		x1, " * ", x2, " = ", x1 * x2, "\n",
		x1, " % ", x2, " = ", x1 % x2, "\n");

	// else łączy się z najbliższym ifem, brak wypisania
	if(false)
		if(true)
			print("If\n");
		else 
			print("Else\n");

	// wypisanie "else"
	if(true)
		if(false)
			print("If\n");
		else 
			print("Else\n");

	return 0;
}