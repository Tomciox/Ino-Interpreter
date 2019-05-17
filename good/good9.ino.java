tuple<int, tuple<int, string>> f(tuple<tuple<tuple<int, int>>> &x, tuple<tuple<int, string>, boolean> y) {
	print("y: ", y, "\n");
	get<1, 1, 2>(x) = 10;
	return make_tuple(1, make_tuple(2, "three"));
}

int main() {
	// Konstruktor zagnieżdżonej tupli.
	tuple<int, boolean, tuple<int, string>> t;
	print(t, "\n");

	// Przypisanie wartości do tupli.
	t = make_tuple(100, true, make_tuple(69, "Hello world!"));
	print(t, "\n");

	// Przypisanie wartości zadanego elementu tupli.
	get<3, 2>(t) = "Hello there!"; 
	print(t, "\n");

	// Wyłuskanie zadanego elementu tupli.
	tuple<int, string> t2 = get<1012312>(t);
	print(t2, "\n");

	tuple<tuple<tuple<int, int>>> t3 = make_tuple(make_tuple(make_tuple(2, 3)));
	print("t3: ", t3, "\n");
	tuple<int, tuple<int, string>> t4 = f(t3, make_tuple(make_tuple(10, "Ino"), false));
	print("t4: ", t4, "\n");
	print("t3: ", t3, "\n");
	
	return 0;
}