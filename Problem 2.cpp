#include <iostream>

int main()
{
	int answer = 0;
	int current = 2;
	int next = 3;
	while (current <= 4000000) //Upper bound of 4,000,000
	{
		answer += current;
		int temp = next;
		next += current;
		current = temp+next;
		next += current;
	}
	std::cout << answer;
	return 0;
}