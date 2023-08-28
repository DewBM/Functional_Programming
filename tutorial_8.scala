object Tutorial_8 {

	val calculateInterest: Double => Double = {
		case deposit if deposit <= 20000 => deposit * 0.02
		case deposit if deposit <= 200000 => deposit * 0.04
		case deposit if deposit <= 2000000 => deposit * 0.035
		case deposit => deposit * 0.065
	}


	val checkNumber: Int => String = {
		case n if n <= 0 => "Negative/Zero"
		case n if n % 2 == 0 => "Even number"
		case n => "Odd number"
	}


	val toUpper: String => String = _.toUpperCase;
	val toLower: String => String = _.toLowerCase;

	val formatNames: (String, String => String) => String = (name, formatFunction) => formatFunction(name);


	def main(args: Array[String]) = {
		q1();
		q2();
		q3();
	}

	def q1() = {
		print("Enter a value: ");
		val deposit: Double = scala.io.StdIn.readDouble();
		println(s"Interest of $deposit = ${calculateInterest(deposit)}");
	}


	def q2() = {
		print("Enter an integer: ");
		val num: Int = scala.io.StdIn.readInt();
		println(s"$num: ${checkNumber(num)}");
	}


	def q3() = {
		println(formatNames("Benny", toUpper));
		println(formatNames("Niroshan", name => toUpper(name.slice(0, 2)) + toLower(name.slice(2, 8))));
		println(formatNames("Saman", toLower));
		println(formatNames("Kumara", name => toUpper(name.slice(0, 1)) + toLower(name.slice(1, 5)) + toUpper(name.slice(5, 6))));
	}
}