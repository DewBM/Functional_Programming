object Tutorial_9 {

	def main(args: Array[String]) = {
		// Q1
		println("-------------------Q1----------------");
		val x: Rational = new Rational(3, 4);
		val y: Rational = new Rational(5, 8);
		val z: Rational = new Rational(2, 7);
		val x_negative = x.neg;
		println(s"Original: ${x.toString}");
		println(s"Negative: ${y.toString}");
		println("\n\n");

		// Q2
		println("-------------------Q2----------------");
		println(s"x-y-z = ${(x-y-z).toString}");
		println("\n\n");

		// Q3
		println("-------------------Q3----------------");
		val account1 = new Account(25000);
		val account2 = new Account(14999.99);
		println(s"Initial balance in account1: ${account1.getBalance}");
		println(s"Initial balance in account2: ${account2.getBalance}");

		account1.deposit(1000.0);
		account1.withdraw(800.0);
		account1.transfer(200.0, account2);

		println(s"Final balance in account1: ${account1.getBalance}");
		println(s"Final balance in account2: ${account2.getBalance}");
		println("\n\n");

		// Q4
		println("-------------------Q4----------------");
		val account3 = new Account(-200.0);
		val account4 = new Account(-100.0);

		val bank = new Bank(List(account1, account2, account3,account4));

		println("List of accounts with negative balances:");
		bank.negativeBalances.foreach(account => println(account.getBalance));

		println(s"\nThe sum of all account balances: ${bank.totalBalance}");

		bank.applyInterestToAll();

		println("\nFinal balances of all accounts after applying interest:");
		bank.getAccounts.foreach(account => println(account.getBalance));
		println("\n\n");
	}

	class Rational(x: Int, y: Int) {
		def numerator = x;
		def denominator = y;

		def neg = new Rational(-this.numerator, this.denominator);

		def +(r: Rational) = new Rational(this.numerator*r.denominator + r.numerator*this.denominator, this.denominator*r.denominator);
		def -(r: Rational) = this + r.neg;

		override def toString = s"$numerator/$denominator";

	}



	class Account(initial: Double) {
		private var balance: Double = initial

		def getBalance: Double = balance

		def deposit(amount: Double): Unit = {
			if (amount > 0) {
				balance += amount
				println(s"Deposited $amount. New balance: $balance")
			} else {
				println("Deposit amount must be positive.")
			}
		}

		def withdraw(amount: Double): Unit = {
			if (amount > 0 && amount <= balance) {
				balance -= amount
				println(s"Withdrew $amount. New balance: $balance")
			} else if (amount <= 0) {
				println("Withdraw amount must be positive.")
			} else {
				println("Insufficient funds for withdrawal.")
			}
		}

		def transfer(amount: Double, account: Account): Unit = {
			if (amount > 0 && amount <= balance) {
				balance -= amount
				account.balance += amount
				println(s"Transferred $amount to target account. New balance: $balance")
			} else if (amount <= 0) {
				println("Transfer amount must be positive.")
			} else {
				println("Insufficient funds for transfer.")
			}
		}

		def applyInterest(): Unit = {
			if (balance > 0) {
				balance += balance * 0.05
			} else if (balance < 0) {
				balance += balance * 0.1
			}
		}
	}


	class Bank(accounts: List[Account]) {
		def getAccounts: List[Account] = accounts

		def negativeBalances: List[Account] = accounts.filter(_.getBalance < 0)

		def totalBalance: Double = accounts.map(_.getBalance).sum

		def applyInterestToAll(): Unit = {
			accounts.foreach(_.applyInterest())
		}
	}
}