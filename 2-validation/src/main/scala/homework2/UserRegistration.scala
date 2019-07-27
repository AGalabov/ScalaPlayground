package homework2

case class RegistrationForm(name: String,
                            email: String,
                            password: String,
                            passwordConfirmation: String,
                            birthYear: String,
                            birthMonth: String,
                            birthDay: String,
                            postalCode: String)

sealed trait RegistrationFormError

case object NameIsEmpty extends RegistrationFormError

case class InvalidEmail(email: String) extends RegistrationFormError

case object PasswordTooShort extends RegistrationFormError
case object PasswordRequiresGreaterSymbolVariety extends RegistrationFormError
case object PasswordsDoNotMatch extends RegistrationFormError

case class InvalidBirthdayDate(dateErrors: Chain[DateError]) extends RegistrationFormError
case class BirthdayDateIsInTheFuture(date: Date) extends RegistrationFormError

case class InvalidPostalCode(code: String) extends RegistrationFormError

sealed trait DateError
case class YearIsNotAnInteger(year: String) extends DateError
case class MonthIsNotAnInteger(month: String) extends DateError
case class DayIsNotAnInteger(day: String) extends DateError
case class MonthOutOfRange(month: Int) extends DateError
case class DayOutOfRange(day: Int) extends DateError
case class InvalidDate(year: Int, month: Int, day: Int) extends DateError

case class Email(user: String, domain: String)

case class User(name: String,
                email: Email,
                passwordHash: String,
                birthday: Date,
                postalCode: Option[String])

object UserRegistration {

  def checkName(name: String) :Validated[RegistrationFormError,String] =
    if (!name.isEmpty)
      Valid(name)
    else Invalid(NameIsEmpty)

  def checkEmail(email: String) :Validated[RegistrationFormError,Email] =
    if (email.matches("^[^@]+@[^@]+$")){
      val mail = email.split("@")
      Valid(Email(mail.head,mail.tail.head))
    }
    else Invalid(InvalidEmail(email))

  def checkPasswordLength(password: String) :Validated[RegistrationFormError,String] =
    if (password.length > 8)
      Valid(password)
    else Invalid(PasswordTooShort)

  def checkPasswordVariety(password: String) :Validated[RegistrationFormError,String]=
    if (password.matches("^.*[A-Za-z].*$") &&
      password.matches("^.*[0-9].*$") &&
      password.matches("^.*[^0-9A-Za-z].*$"))
      Valid(PasswordUtils.hash(password))
    else Invalid(PasswordRequiresGreaterSymbolVariety)

  def checkPasswordMatching(password: String, confirmPassword: String) :Validated[RegistrationFormError,String] =
    if (confirmPassword.equals(password))
      Valid(PasswordUtils.hash(password))
    else Invalid(PasswordsDoNotMatch)


  def checkIsInteger[E <: DateError](x: String,err: E) :Validated[DateError,Int] =
    if(x.matches("^[0-9]+$"))
      Valid(x.toInt)
    else Invalid(err)

  def checkMonthInRange(month: Int) :Validated[DateError,Int] =
    if(1 <= month && month <= 12)
      Valid(month)
    else Invalid(MonthOutOfRange(month))

  def checkDayInRange(day: Int) :Validated[DateError,Int] =
    if(1 <= day && day <= 31)
      Valid(day)
    else Invalid(DayOutOfRange(day))

  def checkValidDate(tuple: (Int,Int,Int)) :Validated[DateError,Date] =
    Date.applyOption(tuple._1,tuple._2,tuple._3) match {
    case Some(date) => Valid(date)
    case None => Invalid(InvalidDate(tuple._1,tuple._2,tuple._3))
  }

  def checkDateInPast(today:Date)(date: Date) :Validated[RegistrationFormError,Date] = {
    val today = Date(2019, 5, 20)
    if (date.year < today.year)
      Valid(date)
    else if (date.year == today.year) {
      if(date.month < today.month)
        Valid(date)
      else if(date.month == today.month) {
        if(date.day < today.day)
          Valid(date)
        else
          Invalid(BirthdayDateIsInTheFuture(date))
      }
      else
        Invalid(BirthdayDateIsInTheFuture(date))
    }
    else
      Invalid(BirthdayDateIsInTheFuture(date))
  }

  def checkPostalCode(code:String, verifier: String => Boolean) :Validated[RegistrationFormError ,Option[String]] =
    if(code.isEmpty)
      Valid(None)
    else if (verifier(code))
      Valid(Some(code))
    else Invalid(InvalidPostalCode(code))


  def checkDate(year: String, month: String, day: String,today: Date) :Validated[Object,Date] =
    (checkIsInteger(year,YearIsNotAnInteger(year)),
    checkIsInteger(month,MonthIsNotAnInteger(month)).flatMap(checkMonthInRange),
    checkIsInteger(day,DayIsNotAnInteger(day)).flatMap(checkDayInRange)).zip match {
      case v@Valid(_) => v.flatMap(checkValidDate).flatMap(checkDateInPast(today))
      case Invalid(errors) => Invalid(InvalidBirthdayDate(errors.asInstanceOf[Chain[DateError]]))
    }


  def checkPassword(password:String, confirmPassword:String) :Validated[RegistrationFormError,String] =
    (checkPasswordLength(password),
      checkPasswordVariety(password),
      checkPasswordMatching(password,confirmPassword)).zip.flatMap(_ => Valid(PasswordUtils.hash(password)))


    def toUser(name: String, email: Email, passwordHash: String, birthday: Date,postalCode: Option[String]): Validated[RegistrationFormError, User] =
      Valid(User(name,email,passwordHash,birthday,postalCode))

  def registerUser(userCountryPostalCodeVerifier: String => Boolean, today: Date)
                  (form: RegistrationForm): Validated[RegistrationFormError, User] =
    (checkName(form.name),
      checkEmail(form.email),
      checkPassword(form.password,form.passwordConfirmation),
      checkDate(form.birthYear,form.birthMonth,form.birthDay,today),
      checkPostalCode(form.postalCode,userCountryPostalCodeVerifier)).zipMap(User.apply).asInstanceOf[Validated[RegistrationFormError, User]]
}
