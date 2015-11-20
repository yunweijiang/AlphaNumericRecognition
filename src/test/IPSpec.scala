package src.main.test
import org.scalatest.{ FlatSpec, Matchers }
import src.main.scala.Reader
import java.io.File
import src.main.scala.Training

class IPSpec extends FlatSpec with Matchers{
    "allPngFiles(\"images\")" should "have 3600 files" in
    {
      val len:Int= Reader.allPngFiles(new File(System.getProperty("user.dir")+"/src/images")).length
      len shouldBe 3600
    }
    "getAllFiles(\"images\")" should "have 3600 files" in
    {
      val len:Int=Reader.getAllFiles("images").length
      len shouldBe 3600
    }
    "argsort([9,8,10],3)" should "be [1,0,2]" in
    {
      val arr=new Array[Int](3);
      arr(0)=9
      arr(1)=8
      arr(2)=10
      val training=new Training();
      val ans=training.argsort(arr, 3)
      ans(0) shouldBe 1
      ans(1) shouldBe 0
      ans(2) shouldBe 2
    }
}
