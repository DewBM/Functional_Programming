object Tutorial_5 {
	def main(args: Array[String]) = {
		print("Enter an Integer: ");
		val num: Int = scala.io.StdIn.readInt();
		println("Is prime: " + prime(num));

		println("--------Prime Sequence--------");
		primeSeq(num);

		println("Sum: " + sum(num));

		println("Is Even: " + isEven(num));
		println("Is Odd: " + isOdd(num));

		println("Sum of even numbers: " + evenSum(num));

		println("--------Fibonnaci Sequence--------");
		fibonacciSeq(num);
		println();
	}


	def prime(n: Int, i: Int=2): Boolean = {
		if (n<=1)
			false
		else if (n==i)
			true;
		else if (n%i==0)
			false;	
		else
			prime(n, i+1);
	}


	def primeSeq(num: Int, i: Int=2): Unit = {
		if (i==num)
			println();
		else {
			if (prime(i))
				print(s"$i\t");
			primeSeq(num, i+1);
		}
	}


	def sum(num: Int): Int = {
		if (num==0)
			0
		else
			num + sum(num-1);
	}


	def isEven(n: Int): Boolean = n match {
		case 0 => true
		case _ => isOdd(n-1);
	}

	def isOdd(n: Int): Boolean = !(isEven(n));


	def evenSum(n: Int): Int = {
		if (n==0) 
			0
		else{
			if(n%2==1)
				(n-1) + evenSum(n-3);
			else
				n + evenSum(n-2);
		}
	}


	def fibonacci(n: Int): Int = {
		if (n==0)
			0
		else if (n==1)
			1
		else
			fibonacci(n-1) + fibonacci(n-2);
	}


	def fibonacciSeq(n: Int): Unit = {
		if (n>0)
			fibonacciSeq(n-1);

		print(s"${fibonacci(n)}\t")
	}
}