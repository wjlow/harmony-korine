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

  def harmonise(note: String, octave: Int, scale: String, interval: Int) = Action {
    Ok(Json.toJson(harmony(note, octave, scale, interval)))
  }

  def harmony(note: String, octave: Int, scale: String, degree: Int): Note = {
    val notesInScale: Seq[Note] = generateScale(scales.get(scale).get, Note(note, octave))
    notesInScale(degree - 1)
  }

  def generateScale(scaleSteps: Seq[Int], root: Note): Seq[Note] = {
    scaleSteps.scanLeft(absPitch(root))(_+_).map(abs => pitch(abs))
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