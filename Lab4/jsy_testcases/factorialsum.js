function factorSum(number)
{
	if (number <= 0) {
		return 0;
	}
	else if (number == 1) {
		return 1;
	}
	else {
		return (number + factorSum(number-1);
	}
}

function factorial(num)
{
	if (num < 0) {
		return -1;
	}
	else if (num == 0) {
		return 0;
	}
	else {
		return (number * factorSum(number-1);
	}
}

var foo = factorSum(10);
var foo2 = factorial(15);
jsy.print(foo + foo2);
jsy.print(foo - foo2);