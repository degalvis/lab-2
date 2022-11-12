import scala.collection.mutable.Stack
import scala.math._
import java.util.Scanner
object Main {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    var pila = Stack[String]()
    var sqr = 0.0
    var a,b = 0
    var resultado = 0
    var enPila = sc.nextLine()
    var listaDeayuda:List[String] = enPila.split(" ").map(_.trim).toList
    for(i <- listaDeayuda){
      resultado = 0
              i match {
      case "+" =>  a = pila.pop().toInt
                   b =  pila.pop().toInt
                   resultado = a + b
                   pila.push(resultado.toString)
      case "-" => 
                   a = pila.pop().toInt
                   b =  pila.pop().toInt
                   resultado = b - a
                   pila.push(resultado.toString)
      case "*" => 
                   a = pila.pop().toInt
                   b =  pila.pop().toInt
                   resultado = a * b
                   pila.push(resultado.toString)
      case "/" => 
                   a = pila.pop().toInt
                   b =  pila.pop().toInt
                   if(a !=0){
                   resultado = b / a
                   }else{
                     resultado = 0
                   }
                   pila.push(resultado.toString)
      case "neg1" => 
                 a = pila.pop().toInt
                 resultado = a * -1
                 pila.push(resultado.toString)
     case "raiz2" =>
                a = pila.pop().toInt
                sqr = sqrt(a)
                pila.push(sqr.toString)
      case "condnumero" => 
                a = pila.pop().toInt
                a match {
                  case 3 => pila.push("100")
                  case 5 => pila.push("25")
                  case _ => pila.push("0")
                }
      case "sum"=>
           while(!pila.isEmpty){
             a=pila.pop().toInt
             resultado += a
           }
           pila.push(resultado.toString)
      case "producto"=>
           while(!pila.isEmpty){
             a=pila.pop().toInt
             resultado *= a
           }
           pila.push(resultado.toString)
      case "promedio"=>
           while(!pila.isEmpty){
             b +=1
             a=pila.pop().toInt
             resultado += a
           }
            resultado = resultado / b
           pila.push(resultado.toString)            
      case _ =>   pila.push(i)        
        }
    }
    print(pila)
    
  }
}
