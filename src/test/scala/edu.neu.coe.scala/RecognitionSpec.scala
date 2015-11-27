package edu.neu.coe.scala
import org.scalatest.{ FlatSpec, Matchers }
import edu.neu.coe.scala._

import java.io.File
import java.util.{HashMap => JHashMap}
/**
 * @author Administrator
 */
class RecognitionSpec extends FlatSpec with Matchers{
    "allFiles(\"images\")" should "have 3600 files" in
    {
      val len:Int= Training.allFiles(new File(System.getProperty("user.dir")+"/src/main/resources/images")).length
      len shouldBe 3600
    }
    "getAllFiles(\"images\")" should "have 3600 files" in
    {
      val len:Int=Reader.getAllFiles("main/resources/images").length
      len shouldBe 3600
    }
    "argsort([9,8,10],3)" should "be [1,0,2]" in
    {
      val arr=new Array[Int](3);
      arr(0)=9
      arr(1)=8
      arr(2)=10
      val ans=Training.argsort(arr, 3)
      ans(0) shouldBe 1
      ans(1) shouldBe 0
      ans(2) shouldBe 2
    }
    "max({A:3, B:2, C:1, D:4, E:5})" should "be E" in
    {
      val count=new JHashMap[Character,Int]()
      count.put('A', 3)
      count.put('B', 2)
      count.put('C', 1)
      count.put('D', 4)
      count.put('E', 5)
      val ans=Training.max(count)
      ans shouldBe 'E'
    }
    "isBlack(0)" should "be true" in //0x000000 or 0 is black
    {
      Process.isBlack(0) shouldBe true
    }
    "isBlack(0xFFFFF)" should "be false" in //0xFFFFFF is white
    {
      Process.isBlack(0xFFFFF) shouldBe false//FFFFFF
    }
    "isWhite(0xFF0000)" should "be false" in//0xFF0000 is red
    {
      Process.isWhite(0xFF0000) shouldBe false
    }
    "isWhite(0x3CB371)" should "be true" in
    {
       Process.isWhite(0x3CB371) shouldBe true//0x3CB371 is springGreen
    }
}
