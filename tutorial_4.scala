object Tutorial_4 {
	
	def main(args: Array[String]) = {
		calcInterest();
		patterMatching();
		q3();
	}


	def calcInterest() = {
		print("Enter a value: ");
		val deposit: Double = scala.io.StdIn.readDouble();
		
		val rate: Double = if (deposit <= 20000)
			2;
		else if (deposit <= 200000)
			4;
		else if (deposit <= 2000000)
			3.5
		else
			6.5

		val interest: Double = deposit*rate/100;

		println(s"Interest of $deposit = $interest");
	}



	def patterMatching() = {
		print("Enter a integer: ");
		val num: Int = scala.io.StdIn.readInt();

		num match
			case x if x<=0 => println("Negative/Zero")
			case y if y%2==0 => println("Even");
			case z if z%2==1 => println("Odd");
	}



	def toUpper(str: String): String = str.toUpperCase();


	def toLower(str: String): String = str.toLowerCase();


	def formatNames(name: String)(callback: String => String) = callback(name);



	def q3() = {
		var name = "Benny";
		println(formatNames(name)(toUpper));

		name = "Niroshan";
		println(name.substring(0,1) + formatNames(name.substring(1, 2))(toUpper) + name.substring(2));

		name = "Saman";
		println(formatNames(name)(toLower));

		name = "Kumara";
		println(name.substring(0,5) + formatNames(name.substring(5))(toUpper));
	}
}