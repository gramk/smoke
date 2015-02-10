package smoke

import java.net.URL
import java.io.{ BufferedInputStream, FileInputStream, File }
import scala.util.Try
import scala.io.Source
import scala.collection.JavaConversions._
import java.util.zip.ZipFile

case class Asset(contentType: String, data: Array[Byte])

trait StaticAssets {
  val publicFolder: String
  val cacheAssets: Boolean = false

  val ApplicationPrefixes =
    this.getClass.getClassLoader.getResources("").toList.map(_.getPath())
  lazy val PublicFolderPrefixes =
    this.getClass.getClassLoader.getResources(publicFolder).toList.map(_.getPath())

  private var cachedAssets = Map[String, Asset]()

  private def getExtension(name: String) = {
    val dotIndex = name.lastIndexOf('.')
    if (dotIndex == -1) "" else name.substring(dotIndex + 1)
  }

  private def isStaticAsset(url: URL) =
    url.getProtocol() match {
      case "file" if new File(url.getFile()).isFile() ⇒
        PublicFolderPrefixes.exists(url.getPath.startsWith(_))

      case "jar" if !isJarDirectory(url) ⇒
        val path = url.getPath().stripPrefix("file:")
        ApplicationPrefixes.exists(path.startsWith(_))

      case _ ⇒
        false
    }

  private def isJarDirectory(url: URL) =
    url.getFile().split("!").toList match {
      case jarPath :: inJarPath :: Nil ⇒
        val jar = new ZipFile(new URL(jarPath).getFile)
        val entry = jar.getEntry(inJarPath)
        entry.isDirectory() || {
          var input: java.io.InputStream = null
          try {
            input = jar.getInputStream(entry)
          } finally {
            if (input != null) input.close()
          }
          input == null
        }
    }

  private def readFile(path: String): Option[Array[Byte]] = {
    this.getClass.getClassLoader.getResources(path).toList.collectFirst {
      case r if isStaticAsset(r) ⇒
        val connection = r.openConnection()
        println(connection.getClass())
        val is = connection.getInputStream()
        println(is.getClass())
        try {
          val bis = new BufferedInputStream(is)
          Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
        } finally {
          is.close()
        }
    }
  }

  private def loadAsset(p: String): Option[Asset] = {
    val path = s"$publicFolder$p"
    readFile(path) map { bytes ⇒
      val extension = getExtension(path)
      Asset(MimeType(extension), bytes)
    }
  }

  private val getAsset: String ⇒ Option[Asset] =
    if (cacheAssets)
      (path: String) ⇒ cachedAssets.get(path).orElse {
        val asset = loadAsset(path)
        asset.map { a ⇒
          cachedAssets += path -> a
        }
        asset
      }
    else (path: String) ⇒ loadAsset(path)

  def responseFromAsset(path: String): Response = {
    getAsset(path) match {
      case Some(asset) ⇒
        Response(Ok, Seq("Content-Type" -> asset.contentType), RawData(asset.data))
      case None ⇒
        Response(NotFound)
    }
  }
}
