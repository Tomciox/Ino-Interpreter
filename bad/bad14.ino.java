// redeklaracja w funkcji zmiennej, która jest argumentem 
int f(int a) { 
	int a = 2;
}

int main() {
	f(1);

	return 0;
}