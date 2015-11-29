package edu.neu.coe.scala
import scala.io._
import java.io._
import java.util.Arrays
import scala.collection.mutable.ArrayBuffer
import java.util.ArrayList;
import java.text.SimpleDateFormat;
import java.util.Date
import scala.actors._
import scala.actors.Actor._
import java.util.{HashMap => JHashMap}
//import com.github.tototoshi.csv._
import scala.collection.JavaConversions._

object Reader extends App
{
    
 /*****************Part 1: All png files to text**********************/
    def imageToText(folder:String,jpgPath: String, toPath:String):Boolean=
    {
        Process.getImageFile(folder,System.getProperty("user.dir")+"/src/"+folder+"/"+jpgPath,toPath)
    }
    def allImagesToTexts(fromPath:String,toPath:String):Unit=
    {
        val files=Training.allFiles(new File(System.getProperty("user.dir")+"/src/"+fromPath)).toList
       
        files.par.map { x => imageToText(fromPath,x.getName,toPath)}
  
    }
 
  /*****************Part 2**********************/
  //get all files in the destination directory
    def getAllFiles(path:String):List[File]=
    {
        Training.allFiles(new File(System.getProperty("user.dir")+"/src/"+path)).toList
    }
/****************************************************************/
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

           for(k<-0 until line.length())
           {
             vector(i*1024+j*32+k)=line.charAt(k)
           }
         }

       }

    vector
 }
 

/****************************************************************/
    def getTrainingResult():JHashMap[Array[Array[Char]],ArrayBuffer[Char]]=
    {
       /*************get Training samples*****************/
      allImagesToTexts("main/resources/images","main/resources/imagesText")
      val fileArr=getAllFiles("main/resources/imagesText").toStream
      val hwLables=new ArrayBuffer[Char]()
      val trainingMat=Array.ofDim[Char](fileArr.length,1024)
      trainingMat.map { x => Arrays.fill(x, '0') }
      for(i<-0 until fileArr.length)yield
      {
          hwLables.append(fileArr(i).getName.split('.')(0).charAt(0))
          trainingMat(i)=sequenceToVector(fileArr(i).getName,"main/resources/imagesText")
      }
      val map=new JHashMap[Array[Array[Char]],ArrayBuffer[Char]]()
      map.put(trainingMat, hwLables)
      map
    }
    
    def roundAt(p: Int)(n: Double): Double = 
    { 
      val s = math pow (10, p)
      (math round n * s) / s 
    }

    def singleCharacterRecognition(trainingMat:Array[Array[Char]],hwLables:ArrayBuffer[Char],err:errorActor):Unit=
    {
      allImagesToTexts("main/resources/testImages","main/resources/testText")
      val testArr=getAllFiles("main/resources/testText").toStream
      val testSize=testArr.size
      val digitSample=new ArrayBuffer[Int]()
      val letterSample=new ArrayBuffer[Int]()
      err.start()
      testArr.foreach 
      { 
          x => 
          val classNumber:Char=x.getName.split('.')(0).charAt(0)
          if(classNumber.isDigit) digitSample.+=(1) else letterSample.+=(1)
              
          val classifierResult=Training.classfy(sequenceToVector(x.getName,"main/resources/testText"), trainingMat, hwLables, 3)
          println("the classifier came back with "+classifierResult+", the real answer is "+classNumber)
          if (classifierResult.!=(classNumber))
          {
              err ! totalError(1)
              err ! (
                  if(classNumber.isDigit) {
                    dataArray.append("digit"+','+classifierResult+','+classNumber+'\n')
                    digitError(1)
                  } else {
                    dataArray.append("letter"+','+classifierResult+','+classNumber+'\n')
                    letterError(1)
                    })
          }
      }
        err ! getDigitError(self)
        receive
        {
          case result:Int =>println("Total number of digit errors is "+result+" out of " + digitSample.length + ", digit error rate = "+ roundAt(2)((result)*1.0/digitSample.length) + ", and corresponding accuracy ratio is " + roundAt(1)(100-(result)*100.0/digitSample.length) + "%.")
        }
        err ! getLetterError(self)
        receive
        {
          case result:Int =>println("Total number of letter errors is "+result+" out of " + letterSample.length + ", letter error rate = "+ roundAt(2)((result)*1.0/letterSample.length) + ", and corresponding accuracy ratio is " + roundAt(1)(100-(result)*100.0/letterSample.length) + "%.")
        }
        err ! getTotalError(self)
        receive
        {
           case result:Int =>println("Total number of errors combined is "+result+" out of " + testArr.length + ", error rate = " + roundAt(2)((result)*1.0/testArr.length) + ",and corresponding accuracy ratio is " + roundAt(1)(100-(result)*100.0/testArr.length) + "%.")
        } 
        err ! terminate(self)
    }
    
    def sequenceRecognition(trainingMat:Array[Array[Char]],hwLables:ArrayBuffer[Char],imageFolder:String,textFolder:String,numOfCharacters:Int):Unit=
    {
         println("Result of analyzing images with "+numOfCharacters+" characters:")
         allImagesToTexts("main/resources/"+imageFolder,"main/resources/"+textFolder)
         val testFiles=getAllFiles("main/resources/"+textFolder)
         val seqArr=testFiles.toStream
         val seqSize=seqArr.size
         val errorCount=new ArrayBuffer[Int]()
         errorCount.append(0)
         seqArr.foreach { x =>val fileStr=x.getName.split('.')(0)
           val classNumber:String=fileStr
           val testVector=sequenceToVector(x.getName,"main/resources/"+textFolder)
           val arr:Array[Char]=new Array[Char](testVector.length/1024)
           for(i<-0 until arr.length)
           {
             val sub=Arrays.copyOfRange(testVector, 1024*i, 1024*i+1024)
             arr(i)=Training.classfy(sub, trainingMat, hwLables, 3)
           }
           val classifierResult=String.valueOf(arr)
           println("the classifier came back with "+classifierResult+", the real answer is "+classNumber)
           if(!classifierResult.equalsIgnoreCase(classNumber))
           {
               errorCount.append(errorCount.last+1)
               for(i<-0 to classNumber.length-1;if classifierResult(i) != classNumber(i)) yield {dataArray.append("sequence"+','+classifierResult(i)+','+classNumber(i))}
           }
         }
        println("Total number of errors is "+errorCount.last + " out of " + seqSize + ".")
        println("Total error rate is "+roundAt(2)(errorCount.last*1.0/seqSize) +".")
        println("Corresponding accuracy ratio is " + roundAt(1)(100-errorCount.last*100.0/seqSize) + "%.")
    }
    
    def printToCSVFile(f: String)(op: java.io.PrintWriter => Unit) {
  		val p = new java.io.PrintWriter(new File(System.getProperty("user.dir")+"/src/"+f))
  		try { op(p) } finally { p.close() }
		}
    
    def recognition():Unit=
    {
       val start=new Date();
       println(start)
       val trainingResult=getTrainingResult()
       val trainingMat=trainingResult.keySet().iterator().next()
       val hwLables=trainingResult.get(trainingMat)   
       singleCharacterRecognition(trainingMat,hwLables,new errorActor)
       sequenceRecognition(trainingMat,hwLables,"sequenceWith2Characters","sequenceTextWith2Characters",2)  
       sequenceRecognition(trainingMat,hwLables,"sequenceWith3Characters","sequenceTextWith3Characters",3)  
       sequenceRecognition(trainingMat,hwLables,"sequenceWith4Characters","sequenceTextWith4Characters",4)  
       sequenceRecognition(trainingMat,hwLables,"sequenceWith5Characters","sequenceTextWith5Characters",5) 
       sequenceRecognition(trainingMat,hwLables,"sequenceWith6Characters","sequenceTextWith6Characters",6) 
       val end=new Date()
       println(end)
       println("from "+start+" to "+end)
		   printToCSVFile("main/resources/data.csv")(p => {dataArray.foreach(p.println)})
    }
    val dataArray = new ArrayBuffer[String]()
    dataArray.append("category,classified result,actual class\n")
    recognition()    
}


