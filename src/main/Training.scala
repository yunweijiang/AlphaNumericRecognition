package src.main.scala
import scala.io._
import java.io._
import java.math._
import scala.collection.mutable.ArrayBuffer
import java.util.Arrays
import java.util.ArrayList
import java.math._
import java.util.{HashMap => JHashMap}
/*
 * most efficient version
 */
class Training {
  /*****get all files******/
  def allFiles(dir: File): Iterator[File]=
  {
    val d=dir.listFiles().filter { x => x.isDirectory() }
    val f=dir.listFiles().filter { x => x.isFile() }.toIterator
    f++d.toIterator.flatMap {allFiles _}
  }

  def argsort(X:Array[Int], size:Int):Array[Int]=
  {
     val p=new Array[Int](size)
     for(i<-0 to size-1)
       p(i)=i
     for(i<-0 to size-1)
     {
       for(j<-i+1 to size-1)
       {
         X(i)>X(j) match
         {
           case true=> val tmp=X(i);X(i)=X(j);X(j)=tmp;
                       val tmp2=p(i);p(i)=p(j);p(j)=tmp2;
           case false=>
         }
       }
     }
     p
  }
    // classify0(vectorUnderTest, trainingMat, hwLabels, 3)
  def classfy(X:Array[Char], dataSet: Array[Array[Char]],labels:ArrayBuffer[Char], k:Int):Character=
  {
    val dataSetSize=dataSet.length
 //   val tmpMat=for{i<- 0 to dataSetSize-1}yield{X}
    val tmpMat=new Array[Array[Char]](dataSetSize)
    for(i<-0 to dataSetSize-1)
      tmpMat(i)=X
    val diffMat=new Array[Array[Int]](dataSetSize)
    for(i<-0 to diffMat.length-1)
      diffMat(i)=new Array[Int](X.length)
    for(i<-0 to dataSetSize-1)
    {
      for(j<-0 to X.length-1)
        diffMat(i)(j)=tmpMat(i)(j)-dataSet(i)(j)
    }

    val sqDiffMat =new Array[Array[Int]](dataSetSize)

    for(i<- 0 to dataSetSize-1)
    {
      sqDiffMat(i)=new Array[Int](X.length)
    }
    for(i <- 0 to dataSetSize-1)
    {
      for(j <-0 to X.length-1)
      {
        sqDiffMat(i)(j)=diffMat(i)(j)*diffMat(i)(j)
      }
    }
  
    val sqDistances=new Array[Int](dataSetSize)
    for(i<-0 to dataSetSize-1)
    {
      //16-35 seconds for 625 images
       var cur=0
       for(j<- 0 to X.length-1)
       {
         cur+=sqDiffMat(i)(j)
       }    
       sqDistances(i)=cur
 //     sqDistances(i)=sqDiffMat(i).sum //28-65 seconds using this
    }
    val sortedDistIndicies=argsort(sqDistances,sqDistances.length)   
    val classCount=new JHashMap[Character,Double]()
    for(i<-0 to k)
    {
      val voteIlabel=labels(sortedDistIndicies(i))
      classCount.containsKey(voteIlabel) match
      {
        case true => val cc=classCount.get(voteIlabel)
                     classCount.remove(voteIlabel)
                     classCount.put(voteIlabel, cc+1)
        case false =>  classCount.put(voteIlabel, 1)
      }
    }
   max(classCount)
  }
  //this is where further work is needed
  def max(count:JHashMap[Character,Double]):Character=
  {
    val it=count.keySet().iterator()
    val first=it.next()
    var max=first
    while(it.hasNext())
    {
        val key=it.next();
        val value=count.get(key)
        val maxValue=count.get(max)
        maxValue<value match
        {
          case true=>max=key
          case false=>
        }
    }
    max
  }
}
