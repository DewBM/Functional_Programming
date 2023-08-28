object Tutorial_10 {
    def main(args: Array[String]) = {
        calculateAverage(List(0, 10, 30, 20));
        countLetterOccurrences(List("apple", "banana", "cherry", "date"));
    }


    def calculateAverage(celsius: List[Double]) = {
        val len: Int = celsius.length;
        val avg: Double = celsius.map(c=>(c*9/5)+32).reduce((a,b)=> a+b)/len;
        println(s"Average Fahrenheit temperature: $avg");
    }

    def countLetterOccurrences(words: List[String]) = {
        val n_letters: Int = words.map(word=>word.length).reduce((a,b)=>a+b);
        println(s"Total count of letter occurrences: $n_letters");
    }
}