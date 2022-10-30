object ReversePN{

    def RPN(str: String): Double = {
        val items = srt.split(" ")
        val stack = List[Double]()

        items.foldLeft(stack)(foldingFunction).head
    }

    def foldingFunction(stack: List[Double], a: String): List[Double] = stack match{
        case List() => a.toDouble ::stack
        case List(_) => a.toDouble ::stack
        case x::y::ys => a match{
            case "+" => x + y:: ys
            case "-"=> y - x:: ys
            case "*" => y * s:: ys
            case "/" => y / x:: ys
            case s:: String  => s.toDouble ::stack
        } 
    }

}
