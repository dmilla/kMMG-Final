package TFM.MidiWebMiner

/**
  * Created by diego on 30/03/16.
  */

import java.io._
import java.net.{MalformedURLException, URL}

import TFM.CommProtocol._
import TFM.kMarkovMelodyGenerator.kMMGUI
import akka.actor.Actor

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.collection.parallel.mutable.ParArray
import scala.util.matching.Regex

class WebCrawler extends Actor {

  var requestProperties = HashMap(
    "User-Agent" -> "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)",
    "Referer" -> ""
  )
  var crawledUrls = ArrayBuffer.empty[String]
  var currentDepth = 0
  var midisFound = 0
  var downloadsPath = System.getProperty("user.home")


  def crawlUrl(url: String, followIf: String, maxDepth: Int, downloadsDirectory: String) = {
    notify("¡Crawling " + url + "!")
    currentDepth = 0
    crawledUrls = ArrayBuffer.empty[String]
    midisFound = 0
    if (!downloadsDirectory.isEmpty) downloadsPath = downloadsDirectory
    val linkRegex = ("""http://[A-Za-z0-9-_:%&?/.=+]*""" + followIf + """[A-Za-z0-9-_:%&?/.=+]*""").r
    val (contentType, contentDisposition, inputStream) = getHttp(url, "")
    val links = getLinks(scala.io.Source.fromInputStream(inputStream, "UTF-8").getLines.mkString, linkRegex)
    //links.foreach(notify(_))
    var linksWithReferer = links.map((url, _)).toArray.par
    notify("Se han encontrado " + links.size + " links en la web objetivo")
    crawledUrls += url
    while (currentDepth < maxDepth) {
      val newLinks = followLinks(linksWithReferer, linkRegex)
      currentDepth += 1
      notify("Profundidad " + currentDepth + " alcanzada, ¡se encontraron " + newLinks.size + " nuevos links!")
      linksWithReferer = newLinks
    }
  }

  def followLinks(links: ParArray[(String, String)], linkRegex: Regex) = {
    val new_links = ArrayBuffer.empty[(String, String)]
    for ( (referer, url) <- links ) {
      if (!crawledUrls.contains(url)) {
        try {
          val (contentType, contentDisposition, inputStream) = getHttp(url, referer)
          if (contentType contains "text/html") {
            val page_links = getLinks(scala.io.Source.fromInputStream(inputStream, "UTF-8").getLines.mkString, linkRegex)
            for (new_link <- page_links) {
              if (!crawledUrls.contains(new_link)) new_links.append((url, new_link))
            }
          } else if (contentType contains "audio/mid") {
            var fileName = "midi_" + midisFound
            if (contentDisposition != null && contentDisposition.indexOf("=") != -1) {
              fileName = contentDisposition.split("=")(1) replaceAll("\"", "")
            } else {
              fileName = url replaceAll("http://", "") replaceAll("/", "-")
            }
            val nameWithPath = downloadsPath + "/" + fileName
            val midiFile = new File(nameWithPath)
            writeToFile(inputStreamToByteStream(inputStream), midiFile)
            kMMGUI.midiWebMiner.notesExtractor ! NotesExtractionRequest(midiFile)
            midisFound += 1
            notify("Nuevo MIDI descargado: " + nameWithPath)
          } else {
            println("Other ContentType found: " + contentType)
          }
        } catch {
          case e: FileNotFoundException => notify("FileNotFoundException intentando acceder a " + url)
          case e: MalformedURLException => notify("MalformedURLException intentando acceder a " + url)
          case e: Exception => notify("Excepción accediendo a " + url + " con referer " + referer + " : " + e)
        }
      }
    }
    new_links.toArray.par
  }

  def getHttp(url: String, referer: String) = {
    val connection = new URL(url).openConnection
    requestProperties("Referer") = referer
    requestProperties.foreach({
      case (name, value) => connection.setRequestProperty(name, value)
    })
    val contentType =  connection.getHeaderField("Content-Type")
    val contentDisposition =  connection.getHeaderField("Content-Disposition")
    val inputStream = connection.getInputStream
    crawledUrls += url
    (contentType, contentDisposition, inputStream)
  }

  def getLinks(html: String, linkRegex: Regex): Set[String] =
    linkRegex.findAllMatchIn(html).map(_.toString.replace("\"", "")).toSet

  def time[T](f: => T): T = {
    val start = System.nanoTime
    val r = f
    val end = System.nanoTime
    val time = (end - start)/(1e6*1000)
    notify("Crawling finalizado, ¡" + midisFound + " midis descargados! " + crawledUrls.size + " páginas recorridas en " + time +"s")
    kMMGUI.midiWebMiner.notesExtractor ! "report"
    r
  }

  def inputStreamToByteStream(is: InputStream): Stream[Byte] =
    Iterator continually is.read takeWhile (-1 !=) map (_.toByte) toStream

  def writeToFile(data : Stream[Byte], file : File) = {
    val target = new BufferedOutputStream( new FileOutputStream(file) )
    try data.foreach( target.write(_) ) finally target.close
  }

  def notify(msg: String) = kMMGUI.midiWebMiner.addCrawlerOutput(msg)

  def receive = {
    case  CrawlRequest(url, followIf, depth, downloadsDirectory) => time(crawlUrl(url, followIf, depth, downloadsDirectory))
    case _      ⇒ println("WebCrawler received unknown message")
  }

}
