package edu.neu.coe.scala

import scala.io._
import java.io._
import java.math._
import scala.collection.mutable.ArrayBuffer
import java.util.Arrays
import java.util.ArrayList
import java.math._
import java.util.HashMap
//import scala.reflect.internal.util.Collections

/*
 * most efficient version
 */
object Training {
  type CharArray2D=Array[Array[Char]]
  /*****get all files******/
  def allFiles(dir: File): Iterator[File]=
  {
    val d=dir.listFiles().filter { x => x.isDirectory() }
    val f=dir.listFiles().filter { x => x.isFile() }.toIterator
    f++d.toIterator.flatMap {allFiles _}
  }

  def argsort(X:Array[Int], size:Int):Array[Int]=
  {
  
     val q=for(i<-0 until size)yield{i}
     val p=q.toArray[Int]
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
  
  def classfy(X:Array[Char], dataSet: CharArray2D,labels:ArrayBuffer[Char], k:Int):Character=
  {
    val dataSetSize=dataSet.length
    val tmpMat=for{i<- 0 to dataSetSize-1}yield{X}
    val diffMat=for(i<-0 until dataSetSize)yield
    {
      for{j<- 0 until X.length}yield{tmpMat(i)(j)-dataSet(i)(j)}
    }

    val sqDiffMat=diffMat.map { x => x.map { y => y*y } }
    val sqDistances=sqDiffMat.map { x => x.sum }.toArray
    val sortedDistIndicies=argsort(sqDistances,sqDistances.length)   
    val classCount=new HashMap[Character,Int]()
    for(i<-0 to k)
    {
      val voteIlabel=labels(sortedDistIndicies(i))
      if (classCount.containsKey(voteIlabel))
         classCount.replace(voteIlabel, classCount.get(voteIlabel)+1)
      else classCount.put(voteIlabel, 1)
    }
   max(classCount)
  }
  //
  def max(count:HashMap[Character,Int]):Character=
  {
    val arr=count.values().toArray()
    Arrays.sort(arr)
    val maxValue=Integer.valueOf((arr(arr.length-1)).toString())
    val buffer=new ArrayBuffer[Character]()
    val it=count.keySet().iterator()

    while(it.hasNext())
    {
      val key=it.next()
      if(count.get(key).==(maxValue))
        buffer.+=(key)
    }
    buffer(0)
  }
}
