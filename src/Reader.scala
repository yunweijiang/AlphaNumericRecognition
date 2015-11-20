package src.main.scala
import scala.io._
import java.io._
import java.util.Arrays
import scala.collection.mutable.ArrayBuffer
import java.util.ArrayList;
import java.text.SimpleDateFormat;
import java.util.Date
/*
 * main function process
 * most efficient version
 * Step 1: allImagesToArrays() transfers pics into ".txt" files and store them under the directory of "/textFiles"
 * Step 2: read all files under /textFiles and execute learning process
 */
object Reader extends App
{
 /*****************Part 1: All png files to text**********************/
def imageToArray(folder:String,jpgPath: String, toPath:String):Unit=
  {
      val path=System.getProperty("user.dir")+"/src/"+folder+"/"+jpgPath;
      val process=new Process()
      process.getImageFile(folder,path,toPath)
  }
  def allPngFiles(dir: File): Iterator[File]=
  {
    val d=dir.listFiles().filter { x => x.isDirectory() }
    val f=dir.listFiles().filter { x => x.isFile() }.toIterator
    f++d.toIterator.flatMap {allPngFiles _}
  }
  def allImagesToArrays(fromPath:String,toPath:String):Unit=//convert .png files under /images directory with excellent outputs
  {
     val dir=new File(System.getProperty("user.dir")+"/src/"+fromPath)
     val files=allPngFiles(dir); 
     files.toList.par.map { x => imageToArray(fromPath,x.getName,toPath)}
  }
 
  /*****************Part 2**********************/
  //get all files in the destination directory
  def getAllFiles(path:String):Iterator[File]=
  {
      val training=new Training()
      val dir:String=System.getProperty("user.dir")+"/src/"+path
      val myFile=new File(dir)
      val allFiles=training.allFiles(myFile)
      allFiles
  }
  def arrayToVector(fn:String,fromPath:String):Array[Char]=//convert two-dimensional array into a 1*1024 one-dimensional array
  {
       val filename=System.getProperty("user.dir")+"/src/"+fromPath+"/"+fn//"/src/textFiles/"+fn
       val lines=Source.fromFile(filename).getLines().toList
       val rows=lines.length
       val cols=lines(0).length()
    
       val vector=new Array[Char](rows*cols) 
       for(i<-0 until lines.length)
       {
          val line=lines(i)
          for(j<-0 until line.length())
          {
            vector(i*cols+j)=line.charAt(j)
          }
      }
       vector
  }
 def sequenceToVector(fn:String,fromPath:String):Array[Char]=
 {
       val filename=System.getProperty("user.dir")+"/src/"+fromPath+"/"+fn//"/src/textFiles/"+fn
       val lines=Source.fromFile(filename).getLines().toList
       val rows=lines.length
       val cols=lines(0).length()
       val vector=new Array[Char](rows*cols)
       for(i<-0 until cols/32)
       {
         for(j<-0 until lines.length)
         {
           val line=lines(j).substring(i*32, i*32+32)
           println(line)
//           for(k<-0 until line.length())
//           {
//             vector(i*1024+j*32+k)=line.charAt(k)
//           }
         }
        println("************************")
       }
       println("----------------------------")
  vector
 }
 
/*****************Part 3**********************/
  def sequenceRecognition():Unit=
  {
    allImagesToArrays("images","imagesFiles")
    val trainingFiles=getAllFiles("imagesFiles")
    val fileArr=trainingFiles.toStream
    val size=fileArr.size
    val hwLables=new ArrayBuffer[Char]()
    val trainingMat=Array.ofDim[Char](size,1024)
    trainingMat.foreach { x =>  Arrays.fill(x, '0') }
    for{i<-0 until fileArr.length}yield
    {
        val curFile=fileArr(i)
        val fileNameStr=curFile.getName
        val fileStr=fileNameStr.split('.')(0)
        val classNumber:Char=fileStr.charAt(0)
        hwLables.append(classNumber)
        trainingMat(i)=arrayToVector(fileNameStr,"imagesFiles")
    }
    allImagesToArrays("sequenceImages","sequenceText")
    val testFiles=getAllFiles("sequenceText")
    val testArr=testFiles.toStream
    val testSize=testArr.size
    var errorCount=0.0
    
    val training=new Training()
    for(file<-testArr)
    {
      val curFile=file
      val fileNameStr=curFile.getName
      val fileStr=fileNameStr.split('.')(0)
      val classNumber:String=fileStr
      val testVector=sequenceToVector(fileNameStr,"sequenceText")
      val arr:Array[Char]=new Array[Char](testVector.length/1024)
      for(i<-0 until arr.length)
      {
        val sub=Arrays.copyOfRange(testVector, 1024*i, 1024*i+1024)
        arr(i)=training.classfy(sub, trainingMat, hwLables, 3)
      }
      val classifierResult=String.valueOf(arr)
      println("the classifier came back with "+classifierResult+", the real answer is "+classNumber)
      classifierResult.equalsIgnoreCase(classNumber) match
      {
        case false =>errorCount+=1.0 
        
        case true =>
      }
    }
    println("the total number of errors is:"+errorCount)
    println("the total error rate is:"+errorCount/testSize)
  }
  def singleRecognition():Unit=
  {
      
    /*************get Training samples*****************/
    allImagesToArrays("images","imagesFiles")
    val trainingFiles=getAllFiles("imagesFiles")
    val fileArr=trainingFiles.toStream
    val size=fileArr.size
    val hwLables=new ArrayBuffer[Char]()
    val trainingMat=Array.ofDim[Char](size,1024)
    trainingMat.foreach { x =>  Arrays.fill(x, '0') }
    
    for{i<-0 until fileArr.length}yield
    {
        val curFile=fileArr(i)
        val fileNameStr=curFile.getName
        val fileStr=fileNameStr.split('.')(0)
        val classNumber:Char=fileStr.charAt(0)
        hwLables.append(classNumber)
        trainingMat(i)=arrayToVector(fileNameStr,"imagesFiles")
    }
    /*************get testing samples***********************/
    allImagesToArrays("testImages","testText")
    val testFiles=getAllFiles("testText")
    val testArr=testFiles.toStream
    val testSize=testArr.size
    val errors=new ArrayBuffer[Int]()
    val digitErrors=new ArrayBuffer[Int]()
    val letterErrors=new ArrayBuffer[Int]()
    val digitSample=new ArrayBuffer[Int]()
    val letterSample=new ArrayBuffer[Int]()
    val training=new Training()
    for(file<-testArr)
    {
      val curFile=file
      val fileNameStr=curFile.getName
      val fileStr=fileNameStr.split('.')(0)
      val classNumber:Char=fileStr.charAt(0) 
      classNumber.isDigit match
      {
        case true=>digitSample.+=(1)
        case false=>letterSample.+=(1)
      }
      val testVector=arrayToVector(fileNameStr,"testText")
      val classifierResult=training.classfy(testVector, trainingMat, hwLables, 3)
      println("the classifier came back with "+classifierResult+", the real answer is "+classNumber)
      classifierResult!=classNumber match
      {
        case true =>errors.+=(1)
        classNumber.isDigit match
        {
          case true => digitErrors.+=(1)
          case false => letterErrors.+=(1)
        }
        case false =>
      }
    }
    println("the total number of errors is:"+errors.length)
    println("the total error rate is:"+errors.length*1.0/testSize)
    println("Total digitError is:"+digitErrors.length)
    println("the total digit error rate is:"+digitErrors.length*1.0/digitSample.length)
    println("Total letterError is:"+letterErrors.length)
    println("the total letter error rate is:"+letterErrors.length*1.0/letterSample.length)
  }
 val start=new Date();
 println(start)
 singleRecognition()
 val end=new Date()
 println(end)
 println("from "+start+" to "+end)
// sequenceRecognition()
}
