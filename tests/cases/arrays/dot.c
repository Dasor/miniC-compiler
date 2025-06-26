int main(){

	float a[10];
	float b[10];

	for(int i = 0; i < 10; i++) {
		a[i] = i * 1.0;
		b[i] = (i + 1) * 1.0;
	}

	float c = a @ b;

}

