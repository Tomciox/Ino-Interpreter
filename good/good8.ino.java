// użycie break
void f(int &x) {
	while(true) {
		print(x, "\n");
		if(x >= 3) {
			{
				{
					break;
				}
			}
		}
		x++;
	}
	return;
}

// użycie return
void g(int &x) {
	while(true) {
		print(x, "\n");
		if(x >= 6) {
			{
				{
					return;
				}
			}
		}
		x++;
	}
}

// użycie continue
void h(int x) {
	int i = 0;
	while(true) {
		i++;

		if(i > x)
			return;
		else if(i % 2 == 0)
			continue;
		// wypiszą się tylko nieparzyste wartości
		print(i, "\n");
	}
}

int main() {
	int x;
	print("f:\n");
	f(x);
	print("\ng:\n");
	g(x);
	print("\nh:\n");
	h(x);
	return 0;
}