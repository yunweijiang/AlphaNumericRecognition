package edu.neu.coe.scala
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import javax.imageio.ImageIO;
import scala.collection.mutable.ArrayBuffer
object Process {
   def getThreshold(gray:IndexedSeq[IndexedSeq[Int]],w:Int, h:Int):Int=
  {
      val histData=new Array[Int](w*h);
      for (x<-0 to w-1) 
      {
        for (y<- 0 to h-1) {
          val red = 0xFF & gray(x)(y);
          histData(red)=histData(red)+1
        }
      }

      val total:Int = w * h
      val sum=(for{i<- 0 to 255}yield{i*histData(i)}).sum
      
      val sumB=new ArrayBuffer[Float]()
      sumB.append(0)
      val wB=new ArrayBuffer[Int]()
      wB.+=(0)
      val wF=new ArrayBuffer[Int]()
      wF.+=(0)
      val varMax=new ArrayBuffer[Float]
      varMax.+=(0)
      val threshold = new ArrayBuffer[Int];
      for (t<- 0 to 255) 
      {
        wB.+=(wB.last+histData(t))// Weight Background
        wB.last!=0 match
        {
          case true=>
            wF.+=(total - wB.last)
            wF!=0 match
            {
              case true=>
                 val newSumB=sumB.last+(t * histData(t)).toFloat
                 sumB.append(newSumB)
                 val mB = sumB.last / wB.last; // Mean Background
                 val mF = (sum - sumB.last) / wF.last; // Mean Foreground
                    // Calculate Between Class Variance
                 val varBetween = wB.last * wF.last* (mB - mF) * (mB - mF);
                 varBetween > varMax.last match
                 {
                   case true=> varMax.+=(varBetween)
                               threshold.+=(t)
                   case false=>
                 }
              case false=>  
            }
          case false=>
        }
      }
      threshold.last
  }
  def getImageFile(folder:String,path: String, ToPath:String): Boolean=
  {
        val bufferedImage = ImageIO.read(new File(path))
        val (h,w) = (bufferedImage.getHeight(),bufferedImage.getWidth())
        val gray=for{x<-0 until w}yield{
        for{y<- 0 until h} yield
          {
            (((bufferedImage.getRGB(x, y)>>16)&0xFF)*77+((bufferedImage.getRGB(x, y) >> 8)& 0xFF)*150+((bufferedImage.getRGB(x, y) >> 0) & 0xFF)*29+128)>>8
          }
        }
        val threshold = getThreshold(gray, w, h);
        val binaryBufferedImage = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_BINARY)
        for (x<- 0 until w) 
        {
          for (y<-0 until h) 
          {
            binaryBufferedImage.setRGB(x,y,
                if(gray(x)(y)>threshold)
                  gray(x)(y) | 0x00FFFF
                else
                  gray(x)(y) & 0xFF0000)
          }
        }  

      val imageSeq=for{y<-0 until h}yield
      {
        for{x<- 0 until w}yield
        {
          if(isBlack(binaryBufferedImage.getRGB(x, y)))
            1
          else
            0
        }
      }
    writeFile(imageSeq,path,folder,ToPath)
  }
  
def writeFile(imageSeq:IndexedSeq[IndexedSeq[Int]], path:String,folder:String,ToPath:String):Boolean=
  {
    val filePath=path.substring(0,path.indexOf("/"+folder))+"/"+ToPath+"/"+new File(path).getName.substring(0, new File(path).getName.indexOf(".png"))+".txt"
   
    val file=new File(filePath)
    if(!file.exists())
    {
        val out=new FileWriter(file)
        for(x<-imageSeq)
        {
          x.map { y => out.write(y+'0') }
          out.write('\n')
        }
        out.close()
        true
   }
    false
  }
  
  def isBlack(colorInt: Int):Boolean ={
    val color = new Color(colorInt);
    (color.getRed() + color.getGreen() + color.getBlue())<=300
    }

   def isWhite(colorInt: Int):Boolean ={
      val color = new Color(colorInt);
      (color.getRed() + color.getGreen() + color.getBlue())>300
    }

    def isBlackOrWhite(colorInt: Int):Int =
    {
      if(getColorBright(colorInt) < 30 ||getColorBright(colorInt) > 730)
        1
      else 
        0
    }

    def getColorBright(colorInt: Int): Int =
    {
      val color = new Color(colorInt);
      color.getRed() + color.getGreen() + color.getBlue();
    }
  
}
