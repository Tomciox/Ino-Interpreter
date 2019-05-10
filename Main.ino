int max (int a, int b) {
	if (a >= b) {
		return a ;
	}
	else {
		return b ;
	}
}

// Wykorzystanie tupli, funkcje jako argumenty

int test_tuple_fun (tuple <int, int> a, (int, int : int) f) {
	return f(t[0], t[1]);
}

tuple <(int, int : int), (int, int : int)> test_lambda () {
	return make_tuple <(int, int : int), (int, int : int)> (
		lambda (int a, int b : int) { return a - b; }, 
		lambda (int a, int b : int) { return a + b; }
	);
}

// Argument przekazywany przez wartość

int inc(int a) {
	a++;
	return a;
}

// Argument przekazywany przez referencję 

int incr(int &a) {
	a++;
	return a;
}

// Wynik: 
// 0	
// 1

void testref() {
	int a = 0;
	int b = inc(a);
 	printInt(a);
 	b = incr(a);
  	printInt(a);
}

// Tablice dowolnych typów, subskrybcja tablic/tupli.

void testarr() {
	int[] arr = new int[10];
	printInt(arr[0]);
	tuple <int, int> arr2 = new tuple <int, int>[10];
	arras[0, 1] = "string" ;
	int[][] arr2 = new int[2, 3];
	printString(arr2[0, 0]);
	tuple <(int, int : int), string> t = make_tuple <(int, int : int), string> (
		lambda (int a, int b : int) { return a + b; },
		"string"
	);
}

int main() {
  printString("Hello world");
  testtuplefun(make_tuple<int, int>(1, 2), max);
  testlambda();
  testref();
  testarr();
  return 0;
}

