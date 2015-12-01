package edu.neu.coe.scala
import scala.actors._
import scala.actors.Actor._

case class digitError(number: Int)
case class getDigitError(sender: Actor)
case class letterError(number: Int)
case class getLetterError(sender: Actor)
case class totalError(number: Int)
case class getTotalError(sender: Actor)

case class terminate(sender:Actor)
class errorActor extends Actor
{
   override def act(): Unit=process(0,0,0)
   def process(digit: Int, letter:Int,total:Int):Unit= 
   {
       receive
       {
         case digitError(number) => process(digit+number,letter,total)
         case getDigitError(a) =>a ! digit; process(digit,letter,total)
         case letterError(number) => process(digit,letter+number,total)
         case getLetterError(a) => a ! letter; process(digit,letter,total)
         case totalError(number) => process(digit,letter,total+number)
         case getTotalError(a) => a ! total; process(digit,letter,total)

         case terminate(a) => a.trapExit
       }
   }
}
