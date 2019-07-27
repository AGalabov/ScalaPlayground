package akinator.io

import akinator.io.Console._

object CommandsIO {
  def promptInput(prompt: String): IO[String] = for {
    _ <- putStrLn(prompt)
    input <- getStrLn
  } yield input

  def promptQuestion(question : String): IO[Boolean] = for {
    input <- promptInput(s"$question (yes/no)")
  } yield input == "yes"

  def promptStart(): IO[Unit] =
    putStrLn("##########################\n" +
                    "##    PERSON GUESSER    ##\n" +
                    "##########################")

  def promptGuess(guess : String): IO[Boolean] =
    promptQuestion(s"My guess is: $guess \nAm I right?")

  def promptForContinuation: IO[Boolean] =
    promptQuestion("Do you want to play again?")

  def promptForCorrectAnswer(): IO[String] = promptInput("Who was your person?")

  def promptForDifference(person : String, newPerson : String): IO[String] =
    promptInput(s"How can I differentiate $person from $newPerson?")

  def promptYesAnswer(person : String, newPerson : String): IO[Boolean] = for {
    input <- promptInput(s"The answer to which one is 'yes'? ($person/$newPerson)")
  } yield input == person

  def promptSuccess(): IO[Unit] =
    putStrLn("###########################\n" +
                   "##       Sucesss!        ##\n" +
                   "###########################\n")

  def promptPersonSaved(): IO[Unit] =
    putStrLn("##############################\n" +
                   "##  My knowledge increased  ##\n" +
                   "##   with the new person!   ##\n" +
                   "##############################\n")

  def promptEnd(): IO[Unit] =
    putStrLn("###########################\n" +
                   "##  Thanks for playing!  ##\n" +
                   "###########################\n")
}