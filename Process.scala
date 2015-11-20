package src.main.scala
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import javax.imageio.ImageIO;
class Process {
   def ostu(gray:Array[Array[Int]],w:Int, h:Int):Int=
  {
      val histData=new Array[Int](w*h);
      for (x<-0 to w-1) 
      {
        for (y<- 0 to h-1) {
          val red = 0xFF & gray(x)(y);
          histData(red)=histData(red)+1
        }
      }
      val total = w * h
      val sum=(for{i<- 0 to 255}yield{i*histData(i)}).sum
      var sumB:Float = 0;
      var wB = 0;
      var wF = 0;
      var varMax:Float = 0;
      var threshold = 0;
      for (t<- 0 to 255) 
      {
        wB += histData(t) // Weight Background
        wB!=0 match
        {
          case true=>
            wF = total - wB
            wF!=0 match
            {
              case true=>
                 sumB += (t * histData(t)).toFloat;
                 val mB = sumB / wB; // Mean Background
                 val mF = (sum - sumB) / wF; // Mean Foreground
                    // Calculate Between Class Variance
                 val varBetween = wB * wF * (mB - mF) * (mB - mF);
                 varBetween > varMax match
                 {
                   case true=> varMax = varBetween
                               threshold = t
                   case false=>
                 }
              case false=>  
            }
          case false=>
        }
      }
      threshold;
  }
  def getImageFile(folder:String,path: String, ToPath:String): Unit=
  {
        val bufferedImage = ImageIO.read(new File(path))
        val (h,w) = (bufferedImage.getHeight(),bufferedImage.getWidth())
        val gray = Array.ofDim[Int](w,h)
        for(x<- 0 until w)
        {
          for(y <- 0 until h)
          {
            val argb=bufferedImage.getRGB(x, y)
            val (r,g,b)=((argb>>16)&0xFF,(argb >> 8) & 0xFF,(argb >> 0) & 0xFF)
            val grayPixel:Int=(b * 29 + g * 150 + r * 77 + 128) >> 8
            gray(x)(y)=grayPixel
          }
        }
        val threshold = ostu(gray, w, h);
        val binaryBufferedImage = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_BINARY)
        for (x<- 0 until w) 
        {
          for (y<-0 until h) 
          {
            gray(x)(y)>threshold match
            {
              case true=>gray(x)(y)|= 0x00FFFF
              case false=>gray(x)(y) &= 0xFF0000;
            }
            binaryBufferedImage.setRGB(x, y, gray(x)(y));
          }
        }  
    val imageArr=Array.ofDim[Int](h,w)//192*32
    for (y<- 0 until h) {
      for (x<- 0 until w) 
      {
         isBlack(binaryBufferedImage.getRGB(x, y)) match
         {
           case true=> imageArr(y)(x)=1
           case false => imageArr(y)(x)=0
         }
      }
    }
    val tmpName=new File(path).getName
    val fileName=tmpName.substring(0, tmpName.indexOf(".png"))
    val filePath=path.substring(0,path.indexOf("/"+folder))+"/"+ToPath+"/"+fileName+".txt"
    val file=new File(filePath)
    if(!file.exists())
    {
        val out=new FileWriter(file)
       
        for(i<- 0 to h-1)
        {
          for(j<- 0 to w-1)
          {
            out.write(imageArr(i)(j)+'0');
          }
          out.write("\n");
        }
        out.close();
    }
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
      (getColorBright(colorInt) < 30 ||getColorBright(colorInt) > 730)match
      {
        case true=> 1
        case false=>0
      }
    }

    def getColorBright(colorInt: Int): Int =
    {
      val color = new Color(colorInt);
      color.getRed() + color.getGreen() + color.getBlue();
    }
  

}
