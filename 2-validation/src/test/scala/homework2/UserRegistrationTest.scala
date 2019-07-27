package homework2

import homework2.UserRegistration._
import org.scalatest.{FlatSpec, Matchers}

class UserRegistrationTest extends FlatSpec with Matchers {

  "checkName" should "see if the name is empty" in {
    checkName("Alex") shouldBe Valid("Alex")
    checkName("") shouldBe Invalid(NameIsEmpty)
  }

  "checkEmail" should "see if the email is valid" in {
    checkEmail("alex@gmail.com") shouldBe Valid(Email("alex","gmail.com"))
    checkEmail("alex") shouldBe Invalid(InvalidEmail("alex"))
    checkEmail("alex@") shouldBe Invalid(InvalidEmail("alex@"))
  }

  "checkPassword" should "see if the password is valid and matches" in {
    //checkPassword("pa$$Word123","pa$$Word123") shouldBe
    //  Valid(PasswordUtils.hash("pa$$Word123"))
    checkPassword("a","b") shouldBe
      Invalid(Chain(PasswordTooShort,PasswordRequiresGreaterSymbolVariety,PasswordsDoNotMatch))
  }

  "checkDate" should "validate the date" in {
    val today = Date(2019, 5, 23)
    checkDate("1997","6","11",today) shouldBe Valid(Date(1997,6,11))
    checkDate("1997a","6a","11a",today) shouldBe
      Invalid(Chain(InvalidBirthdayDate(Chain(YearIsNotAnInteger("1997a"),MonthIsNotAnInteger("6a"),DayIsNotAnInteger("11a")))))
    checkDate("1997","13","33",today) shouldBe
      Invalid(Chain(InvalidBirthdayDate(Chain(MonthOutOfRange(13),DayOutOfRange(33)))))
    checkDate("1997","2","29",today) shouldBe
      Invalid(Chain(InvalidDate(1997,2,29)))
    checkDate("2020","5","28",today) shouldBe
      Invalid(Chain(BirthdayDateIsInTheFuture(Date(2020,5,28))))
  }

  "An empty form" should "generate errors for the non optional fields" in {
    val emptyForm = RegistrationForm("", "", "", "", "", "", "", "")

    val validation = registerUser(Set.empty, Date(2019, 5, 4))(emptyForm)

    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet = errors.toSet
    val birthdayErrors = errorsSet.collectFirst {
      case InvalidBirthdayDate(dateErrors) => dateErrors.toSet
    }

    errorsSet should have size 5

    errorsSet should contain allOf (
      NameIsEmpty,
      InvalidEmail(""),
      PasswordTooShort,
      PasswordRequiresGreaterSymbolVariety
    )

    birthdayErrors shouldEqual Some(Set(
      YearIsNotAnInteger(""),
      MonthIsNotAnInteger(""),
      DayIsNotAnInteger("")
    ))
  }

  "Valid form" should "return User" in {
    val today = Date(2019, 5, 23)
    val form = RegistrationForm(name="Alexandar",
      email="alexandar@abv.bg",
      password="pa$$Word123",
      passwordConfirmation="pa$$Word123",
      birthYear="1997",
      birthMonth="6",
      birthDay="11",
      postalCode="1000")

    val expectedUser = ("Alexandar",
      Email("alexandar","abv.bg"),
      Date(1997,6,11),
      Some("1000"))

    registerUser(_ => true, today)(form) match {
      case Invalid(_) => fail()
      case Valid(User(name,email,_,birthday,postalCode)) =>
        (name,email,birthday,postalCode) shouldBe expectedUser
    }
  }
}