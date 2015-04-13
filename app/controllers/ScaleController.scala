package controllers

import play.api.mvc.{Action, Controller}
import play.api.libs.json.Json
import play.api.Routes

case class Note(pitchClass: String, octave: Int)
case class Scale(notes: Seq[Note])

object ScaleController extends Controller {

  implicit val noteWrites = Json.writes[Note]
  implicit val scaleWrites = Json.writes[Scale]

  def getMajorScale = Action { implicit request =>
  	val root = request.queryString.get("root")
  	root match {
  		case Some(_) => Ok(Json.toJson(majorScale(Note(root.get.head, 3))))
  		case _ => BadRequest("please provide root")
  	}
  }

  def majorScale(root: Note): Scale = {
  	Scale(List(root, transpose(root, 2)))
  }

  def getTranspose = Action { implicit request =>
	val note = request.queryString.get("note")
	val step = request.queryString.get("step")

	Ok(Json.toJson(transpose(Note(note.get.head, 3), step.get.head.toInt)))
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
  	}
  }

  def pitch(absPitch: Int): Note = {
  	val pitch = absPitch % 12
  	val octave = absPitch / 12
  	Note(List("C","Cs","D","Ds","E","F","Fs","G","Gs","A","As","B")(pitch), octave)
  }

}