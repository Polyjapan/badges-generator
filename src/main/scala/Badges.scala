import java.awt.{Font, Graphics2D, RenderingHints}
import java.awt.image.BufferedImage
import java.io.{File, FileReader}
import java.util.{Objects, Properties}
import javax.imageio.ImageIO

import scala.collection.mutable
import scala.io.Source


/**
  * @author Louis Vialar
  */
object Badges {

  case class Template(path: String, fontPath: String, fontSize: Float, lineSpace: Int = 30, heightOffset: Int = 0) {
    def image: BufferedImage = ImageIO.read(new File(path))
    val font: Font = Font.createFont(Font.TRUETYPE_FONT, new File(fontPath)).deriveFont(fontSize)
  }

  case class Badge(template: Template, lines: List[String], targetPath: String, targetExt: String = "png") {
    def computeLines(graphics2D: Graphics2D, maxWidth: Int): List[ComputedLine] =
      lines.map(line => {
        def computeIdealFont(font: Font): (Font, Int, Int) = {
          val metrics = graphics2D.getFontMetrics(font)
          val width = metrics.stringWidth(line)
          if (width > maxWidth) computeIdealFont(font.deriveFont(font.getSize - 1F))
          else (font, width, metrics.getHeight)
        }

        computeIdealFont(template.font) match {
          case (font: Font, width: Int, height: Int) => ComputedLine(line, width, height, font)
        }
      })

    def saveImage(bufferedImage: BufferedImage): Unit = ImageIO.write(bufferedImage, targetExt, new File(targetPath))
  }

  case class ComputedLine(text: String, width: Int, height: Int, font: Font) {
    def write(graphics2D: Graphics2D, x: Int, y: Int): Unit = {
      graphics2D.setFont(font)
      graphics2D.drawString(text, x, y)
    }
  }

  def printPatternStructure(name: String): Unit = {
    if (name != null)
      println(s"Merci de créer/modifier le fichier $name.properties avec le format suivant :")
    println("# le template du badge")
    println("base_image=template_vip.jpg")
    println("# la police utilisée pour écrire le texte (true type only)")
    println("font=DroidSans-Bold.ttf")
    println("# la taille de police par défaut")
    println("font_size=80")
    println("# l'espace (en pixels) entre chaque ligne")
    println("line_spacing=30")
    println("# pour décaler le texte sur le template vers le haut ou le bas")
    println("height_offset=0")
    println("(les deux dernières valeurs ainsi que les lignes commençant par # sont optionnelles)")
  }

  def loadPattern(name: String): Template = {
    val file = new File(s"$name.properties")

    if (!file.exists()) {
      throw new Exception(s"$name n'existe pas !")
    }
    val props = new Properties()
    props.load(new FileReader(file))

    try {
      Template(Objects.requireNonNull(props.getProperty("base_image")), props.getProperty("font"), props.getProperty("font_size").toFloat, props.getProperty("line_spacing", "30").toInt, props.getProperty("height_offset", "0").toInt)
    } catch {
      case e: NullPointerException => throw new Exception("Champ obligatoire manquant", e)
      case e: NumberFormatException => throw new Exception("Nombre invalide", e)
      case e: Throwable => throw new Exception("Erreur inconnue", e)
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Lancement sans argument. Vous pouvez aussi fournir deux arguments : [liste source] [dossier cible]")
    }

    val srcFile = if (args.length >= 1) args(0) else "badges.csv"
    val targetDir = if (args.length >= 2) args(1) + "/" else "out/"

    // Load configuration
    val patterns: mutable.Map[String, Template] = mutable.Map()

    if (!new File(srcFile).exists()) {
      println(s"Merci de créer le fichier $srcFile sur le modèle suivant :")
      println("pattern;line 1;line 2;line 3;...")
      println("Chaque pattern doit être associé à un fichier nom.properties sur le format suivant :")
      printPatternStructure(null)
      return
    }

    new File(targetDir).mkdir()

    var i = 0
    val badges = Source.fromFile(srcFile)
      .getLines()
      .map(text => text.split(";"))
      .map(arr => {
        val patName = arr(0)
        if (!patterns.contains(patName)) {
          try {
            val p = loadPattern(patName)
            patterns.put(patName, p)
          } catch {
            case e: Exception =>
              e.getCause.printStackTrace()
              println(e.getMessage)
              printPatternStructure(patName)
              System.exit(0)
          }
        }

        val lines = arr.toList.tail.map(_.trim).filter(_.nonEmpty)
        i += 1
        Badge(patterns(patName), lines, s"$targetDir$i-" + lines.mkString(" ") + ".png")
      }).toList

    val len = badges.size
    println(s"Fichier chargé : $len badges à créer")
    i = 0
    badges.foreach(badge => {
      i += 1
      print(s"[$i/$len] Création du fichier '${badge.targetPath}'... ")
      processBadge(badge)
      println(" OK")
    })

    println("Yay ! Tous les badges ont bien été générés :)")
  }

  def processBadge(badge: Badge): Unit = {
    val image = badge.template.image
    val graphics = image.getGraphics.asInstanceOf[Graphics2D]
    graphics.setRenderingHint( RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    val lines = badge.computeLines(graphics, (image.getWidth() * 0.95).toInt)

    val neededHeight = badge.template.lineSpace * (badge.lines.length - 1) + lines.foldLeft(0)(_ + _.height)

    var height = (image.getHeight - neededHeight) / 2 + badge.template.heightOffset

    lines.foreach(line => {
      val width = (image.getWidth - line.width) / 2

      // println("Line " + line.text + " => " + line.height + " starting at " + (height + line.height))

      height = height + line.height
      line.write(graphics, width, height)
    })

    badge.saveImage(image)
  }

}
