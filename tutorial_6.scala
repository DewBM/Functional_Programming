object Tutorial_6 {
	def main(args: Array[String]) = {
		print("Enter a phrase to encrypt: ");
		val str: String = scala.io.StdIn.readLine();

		val encrypted_str = cipher(str)(encrypt);
		println("Encrypted Phrase: " + encrypted_str);

		val decrypted_str = cipher(encrypted_str)(decrypt);
		println("Decrypted Phrase: " +decrypted_str);
	}


	def encrypt(str: String) = {
		var charArray = str.toArray;
		for (i <- 0 to (charArray.length-1))
			charArray(i) = (charArray(i)+1).toChar;

		charArray.mkString;
	}


	def decrypt(str: String) = {
		var charArray = str.toArray;
		for (i<- 0 to (charArray.length-1))
			charArray(i) = (charArray(i)-1).toChar;

		charArray.mkString;
	}


	def cipher(str: String)(callback: String => String) = callback(str);
}