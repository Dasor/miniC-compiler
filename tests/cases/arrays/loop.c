int main(){


	int a[128];
	// fibonacci
	for (int i = 0; i < 128; i++) {
		if (i == 0) {
			a[i] = 0;
		} else if (i == 1) {
			a[i] = 1;
		} else {
			a[i] = a[i - 1] + a[i - 2];
		}
	}

	return a[12];

}

