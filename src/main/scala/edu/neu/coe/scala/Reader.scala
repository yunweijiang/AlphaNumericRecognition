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
import java.util.HashMap
//import com.github.tototoshi.csv._
import scala.collection.JavaConversions._

object Reader extends App
{
    type CharArray2D=Array[Array[Char]]
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
    def getTrainingResult():HashMap[CharArray2D,ArrayBuffer[Char]]=
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
      val map=new HashMap[CharArray2D,ArrayBuffer[Char]]()
      map.put(trainingMat, hwLables)
      map
    }
    
    def roundAt(p: Int)(n: Double): Double = 
    { 
      val s = math pow (10, p)
      (math round n * s) / s 
    }
    
    def printOutResult(command:String,err:ErrorActor,size:Int)
    {
        err ! (
            if(command.equals("digit"))
            {
              getDigitError(self)
            }
            else if(command.equals("letter"))
            {
              getLetterError(self)
            }
            else
              getTotalError(self)
        )
        receive
        {
           case result:Int =>println("Total number of errors combined is "+result+" out of " + size + ", error rate = " + roundAt(2)((result)*1.0/size) + ",and corresponding accuracy ratio is " + roundAt(1)(100-(result)*100.0/size) + "%.")
        } 
    }
    
    def countAllErrors(numOfCharacters:Int,classNumber:String,errorCount:ArrayBuffer[Int],classifierResult:String,err:ErrorActor,digits:ArrayBuffer[Int],letters:ArrayBuffer[Int]):Unit=
    {
            if(numOfCharacters.!=(1))
             {
               errorCount.append(errorCount.last+1)
               for(i<-0 to classNumber.length-1;if classifierResult(i) != classNumber(i)) yield {dataArray.append("sequence"+','+classifierResult(i)+','+classNumber(i))}
             }
             else
             {
                 err ! totalError(1)
                 err ! (
                  if(classNumber(0).isDigit) {
                    dataArray.append("digit"+','+classifierResult+','+classNumber)
                    digitError(1)
                  } else {
                    dataArray.append("letter"+','+classifierResult+','+classNumber)
                    letterError(1)
                    })
             }
    }
    
    def printOverallResult(numOfCharacters:Int,errorCount:ArrayBuffer[Int],seqSize:Int,digitSize:Int,letterSize:Int,err:ErrorActor):Unit=
    {
      if(numOfCharacters.!=(1))
        {
          println("Total number of errors is "+errorCount.last + " out of " + seqSize + ".")
          println("Total error rate is "+roundAt(2)(errorCount.last*1.0/seqSize) +".")
          println("Corresponding accuracy ratio is " + roundAt(1)(100-errorCount.last*100.0/seqSize) + "%.")
        }
        else
        {
            printOutResult("digit",err,digitSize)
            printOutResult("letter",err,letterSize)
            printOutResult("total",err,seqSize)
            err ! terminate(self)
        }
    }
    
    def textRecognition(trainingMat:CharArray2D,hwLables:ArrayBuffer[Char],imageFolder:String,textFolder:String,numOfCharacters:Int):Unit=
    {
         println("Result of analyzing images with "+numOfCharacters+" characters:")
         allImagesToTexts("main/resources/"+imageFolder,"main/resources/"+textFolder)
         val seqArr=getAllFiles("main/resources/"+textFolder).toStream
         val seqSize=seqArr.size
         val digits=new ArrayBuffer[Int]()
         val letters=new ArrayBuffer[Int]
         val errorCount=new ArrayBuffer[Int]()
         errorCount.append(0)
         val err=if(numOfCharacters.==(1)) new ErrorActor() else null
         if(err.!=(null))
           err.start()
         seqArr.foreach 
         { 
           x =>val fileStr=x.getName.split('.')(0)
           val classNumber:String=if(numOfCharacters.==(1))fileStr.substring(0, 1) else fileStr
           if(numOfCharacters==1)
           {
             if(classNumber(0).isDigit)
               digits.+=(1)
             else
               letters.+=(1)    
           }
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
             countAllErrors(numOfCharacters,classNumber,errorCount,classifierResult,err,digits,letters)
           }
         }

        printOverallResult(numOfCharacters, errorCount, seqSize, digits.length, letters.length, err)
    }
    
    def printToCSVFile(f: String)(op: java.io.PrintWriter => Unit) 
    {
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
       textRecognition(trainingMat,hwLables,"testImages","testText",1)
       textRecognition(trainingMat,hwLables,"sequenceWith2Characters","sequenceTextWith2Characters",2)  
       textRecognition(trainingMat,hwLables,"sequenceWith3Characters","sequenceTextWith3Characters",3)  
       textRecognition(trainingMat,hwLables,"sequenceWith4Characters","sequenceTextWith4Characters",4)  
       textRecognition(trainingMat,hwLables,"sequenceWith5Characters","sequenceTextWith5Characters",5) 
       textRecognition(trainingMat,hwLables,"sequenceWith6Characters","sequenceTextWith6Characters",6) 
       val end=new Date()
       println(end)
       println("from "+start+" to "+end)
		   printToCSVFile("main/resources/data.csv")(p => {dataArray.foreach(p.println)})
    }
    val dataArray = new ArrayBuffer[String]()
    dataArray.append("category,classified result,actual class")
    recognition()    
}



