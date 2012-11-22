package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  
  def fbLogin(uri: String) = Action {
    implicit request =>
      import java.util.UUID;
      val uuid = UUID.randomUUID().toString();
      Redirect("https://www.facebook.com/dialog/oauth?state=" + uuid + "&scope=user_birthday,email,user_about_me,publish_actions,user_location,publish_stream,user_education_history" +
        "&client_id=" + FBAppId + "&redirect_uri=" + URLEncoder.encode("http://domainname/fb/login/code/uri/None", "utf8")).withSession(session + ("uuid" -> uuid))

  }

  def fbLoginCode(code: String, uri: String, state: String) = Action {
    implicit request: RequestHeader =>

      val uuid = session.get("uuid").get
      if (state.equals(uuid) == false) {
        Redirect("/").flashing("error" -> "You may be a victim of CSRF. ")
      } else {
        val ws = WS.url("https://graph.facebook.com/oauth/access_token?state=" + uuid + "&client_id=" + FBAppId + "&client_secret=" + FBAppSecret + "&code=" + code
          + "&redirect_uri=" + URLEncoder.encode("http://domainname/fb/login/code/uri/None", "utf8")).get().value
        val accessToken = (ws.get.body.split("="))(1).split("&")
        try {
          val facebookClient = new DefaultFacebookClient(accessToken(0));
          val user = facebookClient.fetchObject("me", classOf[com.restfb.types.User]);
          val email = models.User.find("email", user.getEmail())
          email match {
            case Some(email) => {
              models.User.updateBySet(email.id.toString, "fbAccessToken", accessToken(0))
              Redirect("/").withSession("email" -> email.email)
            }
            case None => {
              val newUser = new models.User
              try {
                newUser.email = user.getEmail()

                if (user.getLastName() != null && user.getFirstName() != null) newUser.name = user.getFirstName() + " " + user.getLastName()
                if (user.getLocation() != null) newUser.location = user.getLocation().getName()
                if (user.getBirthdayAsDate() != null) newUser.dateOfBirth = user.getBirthdayAsDate()
                if (user.getId() != null) newUser.facebookId = user.getId()
                if (user.getGender() != null) newUser.gender = user.getGender()
                if (accessToken(0) != null) newUser.fbAccessToken = accessToken(0)
            
                models.User.save(newUser)
              } catch {
                case (e) => println(e.printStackTrace())
              }

              Redirect("/").withSession("email" -> newUser.email)
            }
          }

        } catch {
          case (e) => Redirect(routes.Application.index).flashing(
            "error" -> "An error occurred whilst trying to authenticate using Facebook please try again"
          )
        }
      }

  }
}