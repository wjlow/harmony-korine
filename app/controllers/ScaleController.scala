package controllers

import play.api.mvc.{Action, Controller}
import play.api.libs.json.Json
import play.api.Routes

case class Note(pitchClass: String, octave: Int)

object ScaleController extends Controller {

  implicit val noteWrites = Json.writes[Note]

  val scales = Map(
    "major" -> List(2, 2, 1, 2, 2, 2, 1),
    "minor" -> List(2, 1, 2, 2, 1, 2, 2)
    )

  def getScale(scale: String, root: String, octave: Int) = Action {
    val rootNote = Note(root, octave)
    scales.get(scale) match {
      case Some(_) => Ok(Json.toJson(generateScale(scales.get(scale).get, rootNote)))
      case None => BadRequest(scale + " is not a valid scale")
    }
  }

  // def interval(note: Note, scale: Seq[Int])

  def generateScale(scale: Seq[Int], root: Note): Seq[Note] = {
    root +: generateTailOfScale(scale, root)
  }

  def generateTailOfScale(scale: Seq[Int], acc: Note): Seq[Note] = {
    (scale, acc) match {
      case (Nil, _) => Nil
      case (x :: tail, acc) => {
        val nextNote = transpose(acc, x)
        nextNote +: generateTailOfScale(tail, nextNote)
      }
    }
  }

  def transpose(note: Note, step: Int): Note = {
  	pitch(absPitch(note) + step)
  }

  def absPitch(note: Note): Int = {
  	12 * note.octave + pitchClassToInt(note.pitchClass)
  }

  def pitchClassToInt(pitchClass: String): Int = {
  	pitchClass match {
  		case "C" => 0
  		case "Cs" => 1
  		case "D" => 2
  		case "Ds" => 3
  		case "E" => 4
  		case "F" => 5
  		case "Fs" => 6
  		case "G" => 7
  		case "Gs" => 8
  		case "A" => 9
  		case "As" => 10
  		case "B" => 11
      case _ => throw new RuntimeException("pitch class does not exist")
  	}
  }

  def pitch(absPitch: Int): Note = {
  	val pitch = absPitch % 12
  	val octave = absPitch / 12
  	Note(List("C","Cs","D","Ds","E","F","Fs","G","Gs","A","As","B")(pitch), octave)
  }

}