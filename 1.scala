
object ReversePN extends App{

    def RPN(str: String): Double = {
        val items = str.split(" ")
        val accumulator = List[Double]()

        items.foldLeft(accumulator)(foldingFunction).head
    }

    def foldingFunction(stack: List[Double], a: String): List[Double] = stack match{
        case List() => a.toDouble ::stack
        case List(_) => a.toDouble ::stack
        case x::y::ys => a match{
            case "+" => x + y:: ys
            case "-"=> y - x:: ys
            case "*" => y * x:: ys
            case "/" => y / x:: ys
            case s: String => s.toDouble :: stack

        } 
    }

}
