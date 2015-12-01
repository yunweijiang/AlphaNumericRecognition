package edu.neu.coe.scala
import scala.actors._
import scala.actors.Actor._


case class renewSumB(number:Float)
case class getSumB(sender:Actor)
case class renewWB(number:Int)
case class getWB(sender:Actor)

case class stop(sender:Actor)
class ProcessActor extends Actor
{
   override def act(): Unit=process(0,0,0)
   def process(sumB:Float,WB:Int,Threshold:Int):Unit= 
   {
       receive
       {
         case renewSumB(number)=>process(sumB+number,WB,Threshold)
         case getSumB(a)=>a ! sumB;process(sumB,WB,Threshold)
         case renewWB(number)=> process(sumB, WB+number,Threshold)
         case getWB(a)=>a ! WB; process(sumB,WB,Threshold)
         case stop(a) =>a.trapExit

       }
   }
}
