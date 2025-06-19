int main(){

	// simple c program with a for and and a nested if
	int a = 5;
	int b = 3;
	for(int i = 0; i < 10; i++) {
		if(i % 2 == 0) {
			a = a + i;
		} else {
			b = b + i;
		}
	}
	return a;

}

