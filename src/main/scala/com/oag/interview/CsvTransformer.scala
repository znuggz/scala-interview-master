package com.oag.interview

import com.opencsv.CSVParserBuilder
import java.io.PrintWriter
import java.nio.file.{Files, Path}
import java.time.LocalDate
import resource.managed
import scala.io.Source
import scala.util.Try
import scala.collection.mutable.LinkedHashMap

/**
  * The goal of this class is to transform a daily production log into a production-by-operated-data summary.
  *
  * That is, the daily production log has one row of production data per calendar day per well.  A by-operated-day
  * summary contains one row per well with columns for various time interval production totals (180, 360, 720, etc
  * days). Note that the output considers *producing* days, not calendar days.  So a calendar day with 0 production
  * would not count towards the 180 days (for example).  To illustrate, in the following data set there are 3 calendar
  * days, but only 2 producing days for oil (1 for gas, and 3 for water):
  *
  * date, oil, gas, water
  * 2017-01-01, 25, 0, 5
  * 2017-01-02, 0, 7, 3
  * 2017-01-03, 31, 0, 12
  *
  * The column headers for an example oil-by-operated-day CSV might be:
  *
  * wellId, cum_oil_180, cum_oil_360, cum_oil_720
  *
  * The starting version of this code produces this structure.  Specifically, you can create an output file for oil,
  * gas, or water depending on which method you call.
  *
  * The goal of the exercise is to add a method that can output by-operated-day summaries for multiple production types
  * in a single file.  So the headers might look like this for oil, gas, and water all included.
  *
  * wellId, cum_oil_180, cum_oil_360, cum_oil_720, cum_gas_180, cum_gas_360, cum_gas_720, cum_water_180, cum_water_360, cum_water_720
  */
class CsvTransformer(inputFile: Path) {
  private val wellIdColumn = "wellId"
  private val dateColumn   = "date"
  private val oilColumn    = "oil"
  private val gasColumn    = "gas"
  private val waterColumn  = "water"

  private val parser = new CSVParserBuilder().build()

  private def wellIdIndex(headers: Array[String]): Int = headers.indexOf(wellIdColumn)

  private def dateIndex(headers: Array[String]): Int = headers.indexOf(dateColumn)

  private def oilIndex(headers: Array[String]): Int = headers.indexOf(oilColumn)

  private def gasIndex(headers: Array[String]): Int = headers.indexOf(gasColumn)

  private def waterIndex(headers: Array[String]): Int = headers.indexOf(waterColumn)

  private def parseDate(dateStr: String): LocalDate = LocalDate.parse(dateStr)

  private def safeToDouble(dblString: String): Double = Try(dblString.toDouble).getOrElse(0.0)

  private def inputFileHeaders: Array[String] = {
    managed(Files.newInputStream(inputFile)) apply { is =>
      val production = Source.fromInputStream(is).getLines().toSeq
      val headers = parser.parseLine(production.head)

      // confirm date parsing on 10 samples
      val samples = production.tail.take(10).map(parser.parseLine)
      val dateIndex = this.dateIndex(headers)
      samples.foreach(a => parseDate(a(dateIndex)))

      headers
    }
  }

  def dailyOilToOperatedDay(outputFile: Path, productionIntervals: Seq[Int] = Seq(180, 360, 720)): Unit = {
    val headers = inputFileHeaders

    val wellIdIndex = this.wellIdIndex(headers)
    val oilIndex = this.oilIndex(headers)

    dailyToOperatedDay(outputFile,
                       "oil",
                       wellIdIndex,
                       oilIndex,
                       productionIntervals)
  }

  def dailyGasToOperatedDay(outputFile: Path, productionIntervals: Seq[Int] = Seq(180, 360, 720)): Unit = {
    val headers = inputFileHeaders

    val wellIdIndex = this.wellIdIndex(headers)
    // bug here, this was oil index, changed it to gas
    val gasIndex = this.gasIndex(headers)

    dailyToOperatedDay(outputFile,
                       "gas",
                       wellIdIndex,
                       gasIndex,
                       productionIntervals)
  }

  // why no "Unit" here??
  def dailyWaterToOperatedDay(outputFile: Path, productionIntervals: Seq[Int] = Seq(180, 360, 720)): Unit = {
    val headers = inputFileHeaders

    val wellIdIndex = this.wellIdIndex(headers)
    val waterIndex = this.waterIndex(headers)

    dailyToOperatedDay(outputFile,
                       "water",
                       wellIdIndex,
                       waterIndex,
                       productionIntervals)
  }

  // not sure if order is important, if it is would have gone with LinkedHashMap, for the sake of this exercise, just using a sequence.
  def dailyAnyToOperatedDay(outputFile: Path, productionTypes: Seq[String], productionIntervals: Seq[Int] = Seq(180, 360, 720)): Unit = {
    val headers = inputFileHeaders

    val wellIdIndex = this.wellIdIndex(headers)
    if(productionTypes.isEmpty || productionTypes.size > 3)
      throw new IllegalArgumentException(s"Invalid number of elements, productionTypes must contain at least one element, but no more than 3")

    // if there is only one element in the production types set, then use one of the existing methods
    // this could be a "match" pattern?
    if(productionTypes.size == 1) {
      if(productionTypes.contains("water")) {
        val waterIndex = this.waterIndex(headers)
        dailyToOperatedDay(outputFile, "water", wellIdIndex, waterIndex, productionIntervals)
      } else if(productionTypes.contains("oil")) {
        val oilIndex = this.oilIndex(headers)
        dailyToOperatedDay(outputFile, "oil", wellIdIndex, oilIndex, productionIntervals)
      } else if(productionTypes.contains("gas")) {
        val gasIndex = this.oilIndex(headers)
        dailyToOperatedDay(outputFile, "gas", wellIdIndex, gasIndex, productionIntervals)
      }
    } else {
      // create a hashmap where the key is the prod type and the val is the column index
      var typeIndexMap: Map[String, Int] = Map()
      for ((k,v) <- typeIndexMap) println(s"key: $k, value: $v")
      // for each type in prod types if it matches any of our predefined types, add to hashmap, otherwise throw an exception
      productionTypes.foreach {
        productionType =>
          if(productionType == "water") {
            typeIndexMap += ("water" -> this.waterIndex(headers))
          } else if(productionType == "oil") {
            typeIndexMap += ("oil" -> this.oilIndex(headers))
          } else if(productionType == "gas") {
            typeIndexMap += ("gas" -> this.gasIndex(headers))
          } else {
            throw new IllegalArgumentException(s"Unknown production type: productionType = $productionType")
          }

      }

      // pass to new version of dailyToOperatedDay
      dailyToOperatedDayMultiple(outputFile, wellIdIndex, typeIndexMap, productionIntervals)
    }

  }

  private def dailyToOperatedDay(outputFile: Path,
                                 productionType: String,
                                 inputFileWellIdIndex: Int,
                                 inputFileProductionIndex: Int,
                                 productionIntervals: Seq[Int] = Seq(180, 360, 720))
  {

    if(inputFileWellIdIndex < 0 || inputFileProductionIndex < 0)
      throw new IllegalArgumentException(s"Invalid index, must be greater than 0: inputFileWellIdIndex = $inputFileWellIdIndex; inputFileProductionIndex = $inputFileProductionIndex")

    val wellIds = uniqueWellIds(inputFileWellIdIndex)

    managed(new PrintWriter(Files.newBufferedWriter(outputFile))) apply { writer =>
      // write headers
      val intervalHeaders = productionIntervals.map { interval => s"cum_${productionType}_$interval" }
      writer.println((wellIdColumn +: intervalHeaders).mkString(","))

      wellIds.foreach {
        wellId =>
          assert(wellId.nonEmpty, s"Blank WellId")

          val slices = this.slices(wellId, productionIntervals, inputFileProductionIndex)

          val cumProdPerInterval = this.cumProdByInterval(slices, inputFileProductionIndex)

          writer.println((wellId +: cumProdPerInterval).mkString(","))
      }
    }
  }

  private def dailyToOperatedDayMultiple(outputFile: Path,
                                         inputFileWellIdIndex: Int,
                                         productionTypesIndicesMap: Map[String, Int],
                                         productionIntervals: Seq[Int] = Seq(180, 360, 720))
  {
    if(inputFileWellIdIndex < 0)
      throw new IllegalArgumentException(s"Invalid index, must be greater than 0: inputFileWellIdIndex = $inputFileWellIdIndex")
    
    // get all well id's
    val wellIds = uniqueWellIds(inputFileWellIdIndex)

    managed(new PrintWriter(Files.newBufferedWriter(outputFile))) apply { writer =>
      // get the column headers
      var intervalHeaders: Seq[String] = Seq()
      productionTypesIndicesMap.keys.foreach( 
	      (productionType) => productionIntervals.foreach ( 
  	      interval => intervalHeaders = intervalHeaders :+ s"cum_${productionType}_$interval"))
      
      // write the colum header to the file
      writer.println((wellIdColumn +: intervalHeaders).mkString(","))

      wellIds.foreach {
        wellId =>
        assert(wellId.nonEmpty, s"Blank WellId")

        var cumProdPerIntervalAgg: Seq[String] = Seq()

        for ((productionType, inputFileProductionIndex) <- productionTypesIndicesMap) {
          val slices = this.slices(wellId, productionIntervals, inputFileProductionIndex)

          val cumProdPerInterval = this.cumProdByInterval(slices, inputFileProductionIndex)

          cumProdPerIntervalAgg ++= cumProdPerInterval
        }

        writer.println((wellId +: cumProdPerIntervalAgg).mkString(","))

      }
    }
  }

  // build list of unique WellIds
  private def uniqueWellIds(wellIdIndex: Int): Set[String] = {
    managed(Files.newInputStream(inputFile)) apply { is =>
      Source.fromInputStream(is).getLines().map { l =>
        val line = parser.parseLine(l)
        line(wellIdIndex)
      }.toSet.tail.filter(_.nonEmpty)
    }
  }

  private def slices(wellId: String,
                     productionIntervals: Seq[Int],
                     prodHeaderIndex: Int): Seq[Seq[Array[String]]] =
  {
    // productionInterval days of non-zero production per per window -
    val data = prodPerWellId(wellId).filter(row => safeToDouble(row(prodHeaderIndex)) > 0.0)

    var sliceStart = 0
    val intervalSize = productionIntervals.head
    productionIntervals.map { interval =>
      val chunk = data.slice(sliceStart, sliceStart + intervalSize)
      sliceStart = interval
      chunk
    }
  }

  // production per wellId
  private def prodPerWellId(wellId: String): Seq[Array[String]] = {
    managed(Files.newInputStream(inputFile)) apply { is =>
      Source.fromInputStream(is).getLines().filter(_.contains(wellId)).map(parser.parseLine).toList
    }
  }

  private def cumProdByInterval(slices: Seq[Seq[Array[String]]], prodIndex: Int): Seq[String] = {
    var lastInterval = 0.0
    slices.map { rows =>
      lastInterval += cum(prodIndex)(rows)
      s"${(math rint (lastInterval * 100)) / 100}"
    }
  }

  private def cum(prodHeaderIndex: Int)(slice: Seq[Array[String]]): Double = {
    slice.foldLeft(0.0) { case (cumProd, daily) =>
      cumProd + safeToDouble(daily(prodHeaderIndex))
    }
  }
}
