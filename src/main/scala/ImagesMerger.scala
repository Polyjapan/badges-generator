import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import com.sksamuel.scrimage.Image

/**
  * Merges all the badges on A4 pages to print them. As this feature was only used for staff badges, the
  * directories names are hardcoded. It can be updated quite easily though.
  * @author Louis Vialar
  */
object ImagesMerger {
  val IMAGES_PER_COLUMN = 4

  def main(args: Array[String]): Unit = {
    // List files in repo badges_staff
    val directory = new File("badges_staff")
    val pages = directory.listFiles.toList
      .map(ImageIO.read)
      .map(im => {
        Image.fromAwt(im).scaleTo(1087, 685).toNewBufferedImage()
      })
      .foldLeft[List[List[BufferedImage]]](List(Nil))((list, elem) => {
        if (list.head.size < (2 * IMAGES_PER_COLUMN))  (elem :: list.head) :: list.tail
        else (elem :: Nil) :: list
      })
      .map(buildPage)

    var i = 1
    val total = pages.size
    val targetDir = new File("badges_staff_pages")
    targetDir.mkdir()
    pages.foreach(im => {
      println(s"[$i/$total] Writing page-$i.png")
      ImageIO.write(im, "png", new File(targetDir, s"page-$i.png"))
      i += 1
    })
  }

  val START_Y = 10
  val SPACE_Y = 10
  val FIRST_X = 50
  val SECOND_X = 1240

  def buildPage(pageContent: List[BufferedImage]): BufferedImage = {
    def writeColumn(target: BufferedImage, x: Int, images: List[BufferedImage]): Unit = {
      var y = START_Y
      images.foreach(im => {
        val graphics = target.getGraphics.asInstanceOf[Graphics2D]
        graphics.setColor(Color.BLACK)
        graphics.fillRect (x - 3, y - 3, im.getWidth + 6, im.getHeight + 6)
        graphics.drawImage(im, x, y, null)
        y += SPACE_Y + im.getHeight
      })
    }

    val page = createPage
    val firstCol = if (pageContent.length > IMAGES_PER_COLUMN) pageContent take IMAGES_PER_COLUMN else pageContent
    val secondCol = if (pageContent.length > IMAGES_PER_COLUMN) pageContent drop IMAGES_PER_COLUMN else Nil

    writeColumn(page, FIRST_X, firstCol)
    writeColumn(page, SECOND_X, secondCol)
    page
  }

  def createPage: BufferedImage = {
    val img = new BufferedImage(2480, 3508, BufferedImage.TYPE_INT_RGB) // 300 DPI 210mm X 297mm
    val graphics = img.getGraphics.asInstanceOf[Graphics2D]
    graphics.setColor(Color.WHITE)
    graphics.fillRect ( 0, 0, img.getWidth(), img.getHeight())

    img
  }
}
