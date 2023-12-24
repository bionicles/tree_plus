import { Component } from "@angular/core";
import { LoginService } from "./services/login.service";

@Component({
  selector: "app-root",
  templateUrl: "./app.component.html",
  styleUrls: ["./app.component.scss"],
})
export class AppComponent {
  title = "promo-app";

  user: any = "";
  events: any = {};
  events_list: any = [];
  clientSecret: string = "";

  @ViewChild(StripePaymentElementComponent)
  card?: StripePaymentElementComponent;

  cardOptions: StripePaymentElementOptions;

  constructor(
    private http: HttpClient,
    private loginService: LoginService,
    private stripeService: StripeService
  );
  
  constructor(private loginService: LoginService) {
    this.checkSession();
    this.loginService.userMessage.subscribe((message: string) => {
      this.user = message;
    });
  }

  checkSession() {
    if (sessionStorage.getItem("promouser") !== "") {
      this.user = sessionStorage.getItem("promouser");
      this.loginService.setUser(this.user);
    }
  }

  async goToEvent(event_id: string) {}
  valInvitedBy(event: any, event_id: string) {}
}

