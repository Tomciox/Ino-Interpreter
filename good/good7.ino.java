// funkcje zwracające wartości dowolnych typów

int min(int a, int b) {
	if (a < b)
		return a;
	return b;
}

int max(int a, int b) {
	if (a > b)
		return a;
	return b;
}

string f(boolean x) {
	if(x)
		return "True";
	return "False";
}

int main() {
	int x, y = 42;
	
	print(
		"x = ", x, "\n",
		"y = ", y, "\n",
		"min = ", min(x, y), "\n",
		"max = ", max(x, y), "\n");

	print(f(true), "\n");

	return 0;
}
